(defgeneric evaluate (interpreter node))

(defmethod evaluate :around ((interp interpreter) node)
  (let ((result (call-next-method)))
    (when (and (typep result 'decayable-object)
               (not (gethash (object-hash result) (interpreter-decayable-registry interp))))
      (register-decayable interp result))
    result))

(defun track-variable-type (interp var-name var-type)
  (setf (gethash var-name (interpreter-variable-types interp)) var-type))

(defun get-variable-type (interp var-name)
  (gethash var-name (interpreter-variable-types interp) :unknown))

(defmethod evaluate ((interp interpreter) (node program-node))
  (let ((result nil))
    (dolist (stmt (program-statements node))
      (setf result (evaluate interp stmt)))
    result))

(defmethod evaluate ((interp interpreter) (node literal-node))
  (check-stability-budget interp :literal-access)
  (consume-stability-budget interp :literal-access)

  (let ((value (literal-value node)))
    (make-decayable interp value 
                    :integrity (if (numberp value) 1.0 0.9)
                    :decay-rate (if (numberp value) 0.001 0.01))))

(defmethod evaluate ((interp interpreter) (node variable-node))
  (let ((var-name (variable-name node)))
    (let ((cost-type (determine-variable-cost-type interp var-name)))
      (check-stability-budget interp cost-type)
      (consume-stability-budget interp cost-type))
    
    (env-get-with-decay (interpreter-current-env interp) var-name interp)))

(defmethod evaluate ((interp interpreter) (node function-call-node))
  (check-stability-budget interp :function-call)
  (consume-stability-budget interp :function-call)

  (let ((func-name (function-call-name node))
        (arg-values (mapcar (lambda (arg) (evaluate interp arg)) 
                           (function-call-arguments node))))
    
    (cond
      ((string= func-name "print")
       (let ((output (format nil "~{~A~^ ~}" (mapcar #'decayable-value arg-values))))
         (format t "~A~%" output)
         (make-decayable interp output)))
      
      (t
       (let ((func-obj (gethash func-name (interpreter-global-env interp))))
         (cond
           ((and func-obj (functionp (decayable-value func-obj)))
            (apply (decayable-value func-obj) arg-values))
           
           ((and func-obj (typep (decayable-value func-obj) 'function-decl-node))
            (execute-function-call interp (decayable-value func-obj) arg-values))

           ((typep func-obj 'function-decl-node)
            (execute-function-call interp func-obj arg-values))
           
           (t
            (error "Undefined function: ~A" func-name))))))))

(defmethod evaluate ((interp interpreter) (node return-node))
  (check-stability-budget interp :control-flow)
  (consume-stability-budget interp :control-flow)

  (let ((value (if (return-value node)
                   (evaluate interp (return-value node))
                   (make-decayable interp nil))))
    (throw 'return value)))

(defmethod evaluate ((interp interpreter) (node block-node))
  (let ((result nil))
    (dolist (stmt (block-statements node))
      (setf result (catch 'return (evaluate interp stmt)))
      (when (typep result 'decayable-object)
        (return-from evaluate result)))
    (or result (make-decayable interp nil))))

(defun object-hash (obj)
  (sxhash obj))

(defun update-access-time (decayable)
  (when *interpreter-instance*
    (setf (decayable-last-accessed decayable) (interpreter-cycle-count *interpreter-instance*))))

(defmethod evaluate ((interp interpreter) (node binary-op-node))
  (check-stability-budget interp :binary-operation)
  (consume-stability-budget interp :binary-operation)

  (let* ((left-obj (evaluate interp (binary-op-left node)))
         (right-obj (evaluate interp (binary-op-right node)))
         (left (decayable-value left-obj))
         (right (decayable-value right-obj))
         (operator (binary-op-operator node))
         (result (compute-binary-op operator left right)))
    
    (make-decayable interp result :integrity (min (decayable-integrity left-obj) 
                                         (decayable-integrity right-obj)))))

(defun coerce-to-number-if-possible (val)
  (cond
    ((numberp val) val)
    ((and (stringp val) (every #'digit-char-p val))
     (parse-integer val))
    (t val)))

(defun compute-binary-op (operator left right)
  (let* ((l (coerce-to-number-if-possible left))
         (r (coerce-to-number-if-possible right))
         (op (if (stringp operator)
                 (intern (string-upcase operator))
                 operator)))
    (case op
      (+ (+ l r))
      (- (- l r))
      (* (* l r))
      (/ (if (= r 0) 0 (/ l r)))
      (== (if (equal l r) 1 0))
      (!= (if (equal l r) 0 1))
      (< (if (< l r) 1 0))
      (> (if (> l r) 1 0))
      (<= (if (<= l r) 1 0))
      (>= (if (>= l r) 1 0))
      (t (error "Unknown operator: ~A" operator)))))

(defmethod evaluate ((interp interpreter) (node unary-op-node))
  (check-stability-budget interp :unary-operation)
  (consume-stability-budget interp :unary-operation) 

  (let* ((right-obj (evaluate interp (unary-op-right node)))
         (right (decayable-value right-obj))
         (operator (unary-op-operator node))
         (result (compute-unary-op operator right)))
    (make-decayable interp result :integrity (decayable-integrity right-obj))))

(defun compute-unary-op (operator right)
  (case (intern operator)
    (- (- right))
    (! (if (= right 0) 1 0))
    (+ right)
    (~ (lognot right))
    (++) (+ right 1)
    (-- (- right))
    (otherwise (error "Unknown unary operator: ~A" operator))))

(defmethod evaluate ((interp interpreter) (node if-node))
  (check-stability-budget interp :control-flow)
  (consume-stability-budget interp :control-flow)

  (let* ((condition-obj (evaluate interp (if-condition node)))
        (condition (decayable-value condition-obj)))
    (if (not (= condition 0))
        (evaluate interp (if-then node))
        (when (if-else node)
          (evaluate interp (if-else node))))))

(defmethod evaluate ((interp interpreter) (node while-node))
  (check-stability-budget interp :control-flow)
  (consume-stability-budget interp :control-flow)

  (let ((result nil))
    (loop while (not (= (decayable-value (evaluate interp (while-condition node))) 0))
          do (setf result (evaluate interp (while-body node))))
    (or result (make-decayable interp nil))))

(defmethod evaluate ((interp interpreter) (node block-node))
  (let ((result nil))
    (dolist (stmt (block-statements node))
      (setf result (evaluate interp stmt)))
    (or result (make-decayable interp nil))))

(defmethod evaluate ((interp interpreter) (node assignment-node))
  (check-stability-budget interp :variable-assignment)
  (consume-stability-budget interp :variable-assignment)

  (let* ((var-name (assignment-variable node))
         (value-obj (evaluate interp (assignment-value node)))
         (env (interpreter-current-env interp)))
    
    (env-set-with-decay env var-name value-obj interp)
    value-obj))

(defmethod evaluate ((interp interpreter) (node variable-decl-node))
  (check-stability-budget interp :variable-declaration)
  (consume-stability-budget interp :variable-declaration)
  
  (let* ((var-name (decl-name node))
         (value-obj (evaluate interp (decl-value node)))
         (var-type (decl-var-type node))
         (env (interpreter-current-env interp))
         (decay-rate (case var-type
                       (stable 0.001)
                       (volatile 0.01)
                       (t 0.005))))
    
    (track-variable-type interp var-name var-type)

    (setf (gethash var-name env) 
          (make-decayable interp  
                         (decayable-value value-obj)
                         :integrity (decayable-integrity value-obj)
                         :decay-rate decay-rate))
    value-obj))
    

(defun determine-variable-cost-type (interp var-name)
  (let ((var-type (get-variable-type interp var-name)))
    (case var-type
      (stable :stable-var-access)
      (volatile :volatile-var-access)
      (t :volatile-var-access))))

(defmethod evaluate ((interp interpreter) (node function-decl-node))
  (check-stability-budget interp :variable-declaration)
  (consume-stability-budget interp :variable-declaration)

  (let ((func-name (decl-func-name node)))
    (let ((func-obj (make-decayable interp node)))
      (setf (gethash func-name (interpreter-global-env interp)) func-obj)
      func-obj)))

(defmethod evaluate ((interp interpreter) (node repair-node))
  (check-stability-budget interp :maintenance-operation)
  (consume-stability-budget interp :maintenance-operation) 

  (let ((target (repair-target node)))
    (format t "Repairing: ~A~%" target)
    (sleep 2.0)
    (restore-stability-budget interp 5.0)
    (make-decayable interp (format nil "Repaired ~A" target))))

(defmethod evaluate ((interp interpreter) (node reinforce-node))
  (check-stability-budget interp :maintenance-operation)
  (consume-stability-budget interp :maintenance-operation) 

  (let ((target (reinforce-target node)))
    (format t "Reinforcing: ~A~%" target)
    ()
    (restore-stability-budget interp 2.0)
    (make-decayable interp (format nil "Reinforced ~A" target))))

(defmethod evaluate ((interp interpreter) (node accelerate-node))
  (check-stability-budget interp :maintenance-operation)
  (consume-stability-budget interp :maintenance-operation) 

  (let ((target (accelerate-target node)))
    (format t "Accelerating: ~A~%" target)
    (restore-stability-budget interp 1.0)
    (make-decayable interp (format nil "Accelerated ~A" target))))

(defun execute-function-call (interp func-node arg-values)
  (let* ((params (decl-parameters func-node))
       (body (decl-body func-node))
       (new-env (make-child-environment (interpreter-global-env interp))))
    
    (loop for param in params
          for arg in arg-values
          do (setf (gethash param new-env) arg))
    
    (let ((old-env (interpreter-current-env interp))
          (result (make-decayable interp nil)))
      (unwind-protect
           (progn
             (setf (interpreter-current-env interp) new-env)
             
             (setf result 
                   (catch 'return
                     (evaluate interp body)
                     (make-decayable interp nil))) 
             )
        (setf (interpreter-current-env interp) old-env))
      result)))
