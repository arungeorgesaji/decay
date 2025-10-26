(defparameter *stability-budget* 100)
(defparameter *current-budget* 100)

(defun calculate-operation-cost (operation-type)
  (case operation-type
    (:program-execution 1.0)
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
    (:maintenance-operation 0.5)
    (t 0.5)))


(defun check-stability-budget (operation-type)
  (let ((cost (calculate-operation-cost operation-type)))
    (when (< *current-budget* cost)
      (error "Stability budget exhausted. Need ~A, have ~A for operation ~A" 
             cost *current-budget* operation-type))))

(defun consume-stability-budget (operation-type)
  (let ((cost (calculate-operation-cost operation-type)))
    (decf *current-budget* cost)
    (format t "Budget consumed: ~A for ~A. Remaining: ~A~%" 
            cost operation-type *current-budget*)))

(defun restore-stability-budget (interp amount)
  (setf (interpreter-current-budget interp) 
        (min (interpreter-stability-budget interp) 
             (+ (interpreter-current-budget interp) amount)))
  (format t "Budget restored: ~A. Total: ~A~%" 
          amount (interpreter-current-budget interp)))
