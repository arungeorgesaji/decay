(defparameter *stability-budget* 100)
(defparameter *current-budget* 100)

(defun check-stability-budget (operation-type)
  (let ((cost (case operation-type
                (:slow-function 2.0)
                (:fast-function 0.5)
                (:stable-var 0.1)
                (:volatile-var 0.01)
                (t 0.5))))
    (when (< *current-budget* cost)
      (error "Stability budget exhausted. Need ~A, have ~A" cost *current-budget*))))

(defun consume-stability-budget (operation-type)
  (let ((cost (case operation-type
                (:slow-function 2.0)
                (:fast-function 0.5)
                (:stable-var 0.1)
                (:volatile-var 0.01)
                (t 0.5))))
    (decf *current-budget* cost)))

(defun restore-stability-budget (amount)
  (setf *current-budget* (min *stability-budget* (+ *current-budget* amount))))

(defun repair-target (target)
  (format t "Repairing: ~A~%" target)
  (restore-stability-budget 5.0))

(defun reinforce-target (target)
  (format t "Reinforcing: ~A~%" target)
  (restore-stability-budget 2.0))

(defun accelerate-target (target)
  (format t "Accelerating: ~A~%" target)
  (restore-stability-budget 1.0))
