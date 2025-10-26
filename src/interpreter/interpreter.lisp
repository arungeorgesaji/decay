(defclass interpreter ()
  ((global-env :initform (make-hash-table :test 'equal) :accessor interpreter-global-env)
   (current-env :initform (make-hash-table :test 'equal) :accessor interpreter-current-env)
   (call-stack :initform '() :accessor interpreter-call-stack)
   (cycle-count :initform 0 :accessor interpreter-cycle-count)
   (decay-enabled :initform t :accessor interpreter-decay-enabled)
   (stability-budget :initform 100.0 :accessor interpreter-stability-budget)
   (current-budget :initform 100.0 :accessor interpreter-current-budget)
   (variable-types :initform (make-hash-table :test 'equal) :accessor interpreter-variable-types)))

(defmethod initialize-instance :after ((interp interpreter) &key)
  (setf *interpreter-instance* interp)
  (setf (interpreter-current-env interp) (interpreter-global-env interp)))

(defun check-stability-budget (interp operation-type)
  (let ((cost (calculate-operation-cost interp operation-type)))
    (when (< (interpreter-current-budget interp) cost)
      (error "Stability budget exhausted. Need ~A, have ~A for operation ~A" 
             cost (interpreter-current-budget interp) operation-type))))

(defun consume-stability-budget (interp operation-type)
  (let ((cost (calculate-operation-cost interp operation-type)))
    (decf (interpreter-current-budget interp) cost)
    (format t "Budget consumed: ~A for ~A. Remaining: ~A~%" 
            cost operation-type (interpreter-current-budget interp))))

(defun restore-stability-budget (interp amount)
  (setf (interpreter-current-budget interp) 
        (min (interpreter-stability-budget interp) 
             (+ (interpreter-current-budget interp) amount)))
  (format t "Budget restored: ~A. Total: ~A~%" 
          amount (interpreter-current-budget interp)))

(defun calculate-operation-cost (interp operation-type)
  (case operation-type
    (:slow-function 2.0)
    (:fast-function 0.5)
    (:stable-var-access 0.1)
    (:volatile-var-access 0.01)
    (:literal-access 0.05)
    (:binary-operation 0.2)
    (:unary-operation 0.1)
    (:function-call 0.3)
    (:variable-assignment 0.15)
    (:variable-declaration 0.25)
    (:control-flow 0.4)
    (t 0.5)))

(defmethod evaluate ((interp interpreter) (node literal-node))
  (check-stability-budget interp :literal-access)
  (consume-stability-budget interp :literal-access)
  
  (let ((value (literal-value node)))
    (make-decayable value 
                    :integrity (if (numberp value) 1.0 0.9)
                    :decay-rate (if (numberp value) 0.001 0.01))))

(defmethod evaluate ((interp interpreter) (node variable-node))
  (let ((var-name (variable-name node)))
    (multiple-value-bind (value found) (gethash var-name (interpreter-current-env interp))
      (if found
          (progn
            (let ((cost-type (determine-variable-cost-type interp var-name)))
              (check-stability-budget interp cost-type)
              (consume-stability-budget interp cost-type))
            
            (update-access-time value)
            (apply-decay value)
            value)
          (multiple-value-bind (global-value global-found) 
              (gethash var-name (interpreter-global-env interp))
            (if global-found
                (progn
                  (let ((cost-type (determine-variable-cost-type interp var-name)))
                    (check-stability-budget interp cost-type)
                    (consume-stability-budget interp cost-type))
                  
                  (update-access-time global-value)
                  (apply-decay global-value)
                  global-value)
                (error "Undefined variable: ~A" var-name)))))))

(defun determine-variable-cost-type (interp var-name)
  (let ((env (interpreter-current-env interp)))
    (if (gethash var-name env)
        :volatile-var-access
        :stable-var-access)))

(defmethod evaluate ((interp interpreter) (node binary-op-node))
  (check-stability-budget interp :binary-operation)
  (consume-stability-budget interp :binary-operation)
  
  (let* ((left-obj (evaluate interp (binary-op-left node)))
         (right-obj (evaluate interp (binary-op-right node)))
         (left (decayable-value left-obj))
         (right (decayable-value right-obj))
         (operator (binary-op-operator node))
         (result (compute-binary-op operator left right)))
    
    (make-decayable result :integrity (min (decayable-integrity left-obj) 
                                         (decayable-integrity right-obj)))))

(defmethod evaluate ((interp interpreter) (node unary-op-node))
  (check-stability-budget interp :unary-operation)
  (consume-stability-budget interp :unary-operation)
  
  (let* ((right-obj (evaluate interp (unary-op-right node)))
         (right (decayable-value right-obj))
         (operator (unary-op-operator node))
         (result (compute-unary-op operator right)))
    (make-decayable result :integrity (decayable-integrity right-obj))))

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
         (make-decayable output)))
      
      (t
       (multiple-value-bind (func-node found) 
           (gethash func-name (interpreter-global-env interp))
         (if (and found (typep func-node 'function-decl-node))
             (execute-function-call interp func-node arg-values)
             (error "Undefined function: ~A" func-name)))))))

(defmethod evaluate ((interp interpreter) (node assignment-node))
  (check-stability-budget interp :variable-assignment)
  (consume-stability-budget interp :variable-assignment)
  
  (let* ((var-name (assignment-variable node))
         (value-obj (evaluate interp (assignment-value node)))
         (env (interpreter-current-env interp)))
    
    (setf (gethash var-name env) value-obj)
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
    
    (setf (gethash var-name env) 
          (make-decayable (decayable-value value-obj)
                         :integrity (decayable-integrity value-obj)
                         :decay-rate decay-rate))
    value-obj))

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
    (or result (make-decayable nil))))

(defun execute-function-call (interp func-node arg-values)
  (let* ((params (decl-parameters func-node))
         (body (decl-body func-node))
         (func-type (decl-func-type func-node))
         (new-env (make-hash-table :test 'equal))
         (function-cost (case func-type
                          (slow :slow-function)
                          (fast :fast-function)
                          (t :function-call))))
    
    (check-stability-budget interp function-cost)
    (consume-stability-budget interp function-cost)
    
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

(defun check-budget-status (interp)
  (format t "Current stability budget: ~A/~A~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp)))
