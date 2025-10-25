(defclass interpreter ()
  ((global-env :initform (make-hash-table :test 'equal) :accessor interpreter-global-env)
   (current-env :accessor interpreter-current-env)
   (call-stack :initform '() :accessor interpreter-call-stack)
   (cycle-count :initform 0 :accessor interpreter-cycle-count)
   (decay-enabled :initform t :accessor interpreter-decay-enabled)))

(defmethod initialize-instance :after ((interp interpreter) &key)
  (setf *interpreter-instance* interp)
  (setf (interpreter-current-env interp) (interpreter-global-env interp)))

