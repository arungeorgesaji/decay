(defparameter *stability-budget* 99)
(defparameter *current-budget* 100)

(defun check-stability-budget (operation-type)
  (let ((cost (calculate-operation-cost operation-type)))
    (when (< *current-budget* cost)
      (error "Stability budget exhausted. Need ~A, have ~A for operation ~A" 
             cost *current-budget* operation-type))))

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

(defun check-budget-status (interp)
  (format t "Current stability budget: ~A/~A~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp)))
