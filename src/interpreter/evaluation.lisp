(defgeneric evaluate (interpreter node))

(defmethod evaluate ((interp interpreter) (node program-node))
  (let ((result nil))
    (dolist (stmt (program-statements node))
      (setf result (evaluate interp stmt)))
    result))

(defmethod evaluate ((interp interpreter) (node literal-node))
  (let ((value (literal-value node)))
    (make-decayable value 
                    :integrity (if (numberp value) 1.0 0.9)
                    :decay-rate (if (numberp value) 0.001 0.01))))

(defmethod evaluate ((interp interpreter) (node variable-node))
  (let ((var-name (variable-name node)))
    (multiple-value-bind (value found) (gethash var-name (interpreter-current-env interp))
      (if found
          (progn
            (update-access-time value)
            (apply-decay value)
            value)
          (multiple-value-bind (global-value global-found) 
              (gethash var-name (interpreter-global-env interp))
            (if global-found
                (progn
                  (update-access-time global-value)
                  (apply-decay global-value)
                  global-value)
                (error "Undefined variable: ~A" var-name)))))))

(defmethod evaluate ((interp interpreter) (node function-call-node))
  (let ((func-name (function-call-name node))
        (arg-values (mapcar (lambda (arg) (evaluate interp arg)) 
                           (function-call-arguments node))))
    
    (cond
      ((string= func-name "print")
       (let ((output (format nil "~{~A~^ ~}" (mapcar #'decayable-value arg-values))))
         (format t "~A~%" output)
         (make-decayable output)))
      
      (t
       (multiple-value-bind (func-node found) 
           (gethash func-name (interpreter-global-env interp))
         (if (and found (typep func-node 'function-decl-node))
             (execute-function-call interp func-node arg-values)
             (error "Undefined function: ~A" func-name)))))))

(defmethod evaluate ((interp interpreter) (node return-node))
  (let ((value (if (return-value node)
                   (evaluate interp (return-value node))
                   (make-decayable nil))))
    (throw 'return value)))

(defmethod evaluate ((interp interpreter) (node block-node))
  (let ((result nil))
    (dolist (stmt (block-statements node))
      (setf result (catch 'return (evaluate interp stmt)))
      (when (typep result 'decayable)
        (return-from evaluate result)))
    (or result (make-decayable nil))))

(defun update-access-time (decayable)
  (when *interpreter-instance*
    (setf (decayable-last-accessed decayable) (interpreter-cycle-count *interpreter-instance*))))

(defmethod evaluate ((interp interpreter) (node binary-op-node))
  (let* ((left-obj (evaluate interp (binary-op-left node)))
         (right-obj (evaluate interp (binary-op-right node)))
         (left (decayable-value left-obj))
         (right (decayable-value right-obj))
         (operator (binary-op-operator node))
         (result (compute-binary-op operator left right)))
    
    (make-decayable result :integrity (min (decayable-integrity left-obj) 
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
  (let* ((right-obj (evaluate interp (unary-op-right node)))
         (right (decayable-value right-obj))
         (operator (unary-op-operator node))
         (result (compute-unary-op operator right)))
    (make-decayable result :integrity (decayable-integrity right-obj))))

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
  (let* ((condition-obj (evaluate interp (if-condition node)))
        (condition (decayable-value condition-obj)))
    (if (not (= condition 0))
        (evaluate interp (if-then node))
        (when (if-else node)
          (evaluate interp (if-else node))))))

(defmethod evaluate ((interp interpreter) (node while-node))
  (let ((result nil))
    (loop while (not (= (decayable-value (evaluate interp (while-condition node))) 0))
          do (setf result (evaluate interp (while-body node))))
    (or result (make-decayable nil))))

(defmethod evaluate ((interp interpreter) (node block-node))
  (let ((result nil))
    (dolist (stmt (block-statements node))
      (setf result (evaluate interp stmt)))
    (or result (make-decayable nil))))

(defmethod evaluate ((interp interpreter) (node assignment-node))
  (let* ((var-name (assignment-variable node))
         (value-obj (evaluate interp (assignment-value node)))
         (env (interpreter-current-env interp)))
    
    (setf (gethash var-name env) value-obj)
    value-obj))

(defmethod evaluate ((interp interpreter) (node variable-decl-node))
  (let* ((var-name (decl-name node))
         (value-obj (evaluate interp (decl-value node)))
         (var-type (decl-var-type node))
         (env (interpreter-current-env interp))
         (decay-rate (case var-type
                       (stable 0.001)
                       (volatile 0.01)
                       (t 0.005))))
    
    (setf (gethash var-name env) 
          (make-decayable (decayable-value value-obj)
                         :integrity (decayable-integrity value-obj)
                         :decay-rate decay-rate))
    value-obj))

(defmethod evaluate ((interp interpreter) (node function-decl-node))
  (let ((func-name (decl-func-name node)))
    (setf (gethash func-name (interpreter-global-env interp)) node)
    (make-decayable func-name)))

(defun execute-function-call (interp func-node arg-values)
  (let* ((params (decl-parameters func-node))
         (body (decl-body func-node))
         (new-env (make-hash-table :test 'equal)))
    
    (loop for param in params
          for arg in arg-values
          do (setf (gethash param new-env) arg))
    
    (let ((old-env (interpreter-current-env interp))
          (result (make-decayable nil)))
      (unwind-protect
           (progn
             (setf (interpreter-current-env interp) new-env)
             
             (setf result 
                   (catch 'return
                     (evaluate interp body)
                     (make-decayable nil))) 
             )
        (setf (interpreter-current-env interp) old-env))
      result)))
