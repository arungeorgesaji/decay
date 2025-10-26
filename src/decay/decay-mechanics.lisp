(defclass decayable-object ()
  ((value :initarg :value :accessor decayable-value)
   (integrity :initarg :integrity :accessor decayable-integrity :type float)
   (decay-rate :initarg :decay-rate :accessor decayable-decay-rate :type float)
   (stability-cost :initarg :stability-cost :accessor decayable-stability-cost :type float :initform 1.0) 
   (performance-multiplier :initarg :performance-multiplier :accessor decayable-performance-multiplier :type float :initform 1.0) 
   (last-accessed :initform 0 :accessor decayable-last-accessed :type integer)))

(defparameter *stability-budget* 100.0)
(defparameter *current-budget* 100.0)
(defvar *interpreter-instance* nil)


(defun make-decayable (interp value &key (integrity 1.0) (decay-rate 0.01))  
  (let ((obj (make-instance 'decayable-object
                           :value value
                           :integrity (max 0.0 (min 1.0 integrity))
                           :decay-rate decay-rate
                           :stability-cost 1.0      
                           :performance-multiplier 1.0))) 
    (register-decayable interp obj)  
    obj))

(defun corrupt-value (decayable)
  (let ((value (decayable-value decayable)))
    (typecase value
      (number (mutate-number-value decayable))
      (string (mutate-string-value decayable))
      (t value))))

(defun mutate-number-value (decayable)
  (let ((value (decayable-value decayable)))
    (case (random 4)
      (0 (setf (decayable-value decayable) (- value)))    
      (1 (setf (decayable-value decayable) (if (= value 0) 0 (/ 1.0 value)))) 
      (2 (setf (decayable-value decayable) (* value value))) 
      (3 (setf (decayable-value decayable) (sqrt (abs value))))
      (t value))))

(defun mutate-string-value (decayable)
  (let ((value (decayable-value decayable)))
    (when (> (length value) 0)
      (let ((pos (random (length value)))
            (chars "!@#$%^&*"))
        (setf (decayable-value decayable)
              (concatenate 'string
                           (subseq value 0 pos)
                           (string (char chars (random (length chars))))
                           (subseq value (min (1+ pos) (length value)))))))))

(defun initialize-decay-system (interp)
  (clrhash (interpreter-decayable-registry interp))
  (setf (interpreter-cycle-count interp) 0)
  (setf (interpreter-current-budget interp) (interpreter-stability-budget interp)))

(defun apply-decay (decayable)
  (decf (decayable-integrity decayable) (decayable-decay-rate decayable))
  
  (let ((integrity (decayable-integrity decayable)))
    (cond
      ((< integrity 0.3) 
       (when (> (random 1.0) 0.5)  
         (corrupt-value decayable)))
      ((< integrity 0.6)   
       (when (> (random 1.0) 0.8) 
         (corrupt-value decayable)))
      (t decayable)))  
  
  decayable)

(defun apply-global-decay (interp)
  (when (interpreter-decay-enabled interp)
    (maphash (lambda (id decayable)
               (declare (ignore id))
               (apply-decay decayable)  
               (when (<= (decayable-integrity decayable) 0)
                 (handle-object-decay interp decayable)))
             (interpreter-decayable-registry interp))))  

(defun register-decayable (interp decayable)
  (setf (gethash (object-hash decayable) (interpreter-decayable-registry interp)) decayable))

(defun unregister-decayable (interp decayable)
  (remhash (object-hash decayable) (interpreter-decayable-registry interp)))

(defun check-system-stability (interp)
  (let ((total-integrity 0.0)
        (object-count 0))
    (maphash (lambda (id decayable)
               (declare (ignore id))
               (incf total-integrity (decayable-integrity decayable))
               (incf object-count))
             (interpreter-decayable-registry interp))
    
    (when (> object-count 0)
      (let ((avg-integrity (/ total-integrity object-count)))
        (when (< avg-integrity 0.3)
          (format t "SYSTEM INSTABILITY: Average integrity ~A~%" avg-integrity)
          (restore-stability-budget interp (* avg-integrity 10)))))))

(defun handle-budget-exhaustion (interp)
  (format t "STABILITY BUDGET EXHAUSTED at cycle ~A~%" (interpreter-cycle-count interp))
  (setf (interpreter-execution-paused interp) t)
  (signal 'budget-exhausted :interpreter interp))

(defun handle-object-decay (interp decayable)
  (format t "Object fully decayed: ~A~%" decayable)
  (unregister-decayable interp decayable))

(defun cleanup-decay-system (interp)
  (format t "Execution completed after ~A cycles~%" (interpreter-cycle-count interp))
  (format t "Final budget: ~A/~A~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp)))


