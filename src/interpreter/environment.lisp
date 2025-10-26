(defun make-child-environment (parent)
  (let ((child (make-hash-table :test 'equal)))
    (setf (gethash :parent child) parent)
    (setf (gethash :children child) (list))
    (when parent
      (push child (gethash :children parent)))
    child))

(defun env-get-with-decay (env key interp)
  (multiple-value-bind (value found) (gethash key env)
    (if found
        (progn
          (when (typep value 'decayable-object)
            (update-access-time value)
            (apply-decay value))
          value)
        (let ((parent (gethash :parent env)))
          (if parent
              (env-get-with-decay parent key interp)
              (make-decayable interp nil :integrity 1.0 :decay-rate 0.0))))))

(defun env-set-with-decay (env key value interp)
  (when (typep value 'decayable-object)
    (register-decayable interp value))
  (setf (gethash key env) value))

(defmethod initialize-instance :after ((interp interpreter) &key)
  (setf *interpreter-instance* interp)
  (setf (interpreter-current-env interp) (interpreter-global-env interp)))

(defun check-stability-budget (interp operation-type)
  (let ((cost (calculate-operation-cost interp operation-type)))
    (when (< (interpreter-current-budget interp) cost)
      (error "Stability budget exhausted. Need ~A, have ~A for operation ~A" 
             cost (interpreter-current-budget interp) operation-type))))
