(defun make-child-environment (parent)
  (let ((child (make-hash-table :test 'equal)))
    (setf (gethash :parent child) parent)
    child))

(defun env-get (env key)
  (multiple-value-bind (value found) (gethash key env)
    (if found
        value
        (let ((parent (gethash :parent env)))
          (when parent
            (env-get parent key))))))

(defun env-set (env key value)
e (setf (gethash key env) value))
