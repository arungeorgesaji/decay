(defclass interpreter ()
  ((global-env :initform (make-hash-table :test 'equal) :accessor interpreter-global-env)
   (current-env :initform (make-hash-table :test 'equal) :accessor interpreter-current-env)
   (call-stack :initform '() :accessor interpreter-call-stack)
   (cycle-count :initform 0 :accessor interpreter-cycle-count)
   (decay-enabled :initform t :accessor interpreter-decay-enabled)
   (stability-budget :initform 100.0 :accessor interpreter-stability-budget)
   (current-budget :initform 100.0 :accessor interpreter-current-budget)
   (variable-types :initform (make-hash-table :test 'equal) :accessor interpreter-variable-types)
   (decayable-registry :initform (make-hash-table :test 'equal) :accessor interpreter-decayable-registry)
   (execution-paused :initform nil :accessor interpreter-execution-paused)))

(defun execute-program (interp ast &key (max-cycles 1000))
  (let ((*interpreter-instance* interp))
    (unwind-protect
        (progn
          (initialize-decay-system interp)
          (loop for cycle from 0 below max-cycles
                while (not (interpreter-execution-paused interp))
                do (progn
                     (execute-cycle interp ast)
                     (when (>= cycle (interpreter-cycle-count interp))
                       (incf (interpreter-cycle-count interp)))
                     (apply-global-decay interp)
                     (check-system-stability interp)
                     (when (<= (interpreter-current-budget interp) 0)
                       (handle-budget-exhaustion interp)))))
      (cleanup-decay-system interp))))

(defun execute-cycle (interp ast)
  (handler-case
      (progn
        (push-cycle-to-call-stack interp)
        (evaluate interp ast)
        (pop-cycle-from-call-stack interp))
    (error (e)
      (format t "Cycle ~A error: ~A~%" (interpreter-cycle-count interp) e)
      (setf (interpreter-execution-paused interp) t))))

(defun push-cycle-to-call-stack (interp)
  (push (list :cycle (interpreter-cycle-count interp)
              :budget (interpreter-current-budget interp)
              :timestamp (get-universal-time))
        (interpreter-call-stack interp)))

(defun pop-cycle-from-call-stack (interp)
  (pop (interpreter-call-stack interp)))
