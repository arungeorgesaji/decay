(defclass decayable-object ()
  ((value :initarg :value :accessor decayable-value)
   (integrity :initarg :integrity :accessor decayable-integrity :type float)
   (decay-rate :initarg :decay-rate :accessor decayable-decay-rate :type float)
   (stability-cost :initarg :stability-cost :accessor decayable-stability-cost :type float)
   (performance-multiplier :initarg :performance-multiplier :accessor decayable-performance-multiplier :type float)
   (last-accessed :initform 0 :accessor decayable-last-accessed :type integer)))

(defparameter *global-registry* (make-hash-table :test 'equal))
(defparameter *stability-budget* 100.0)
(defparameter *current-budget* 100.0)
(defvar *interpreter-instance* nil)

(defun make-decayable (value &key (integrity 1.0) (decay-rate 0.01) 
                            (stability-cost 1.0) (performance-multiplier 1.0))
  (let ((obj (make-instance 'decayable-object
                           :value value
                           :integrity (max 0.0 (min 1.0 integrity))
                           :decay-rate decay-rate
                           :stability-cost stability-cost
                           :performance-multiplier performance-multiplier)))
    (register-decayable obj)
    obj))

(defun register-decayable (obj)
  (let ((id (gensym "DECAYABLE-")))
    (setf (gethash id *global-registry*) obj)
    id))

(defun apply-decay (decayable)
  (when (and *interpreter-instance* 
             (interpreter-decay-enabled *interpreter-instance*))
    (let* ((current-cycle (interpreter-cycle-count *interpreter-instance*))
          (cycles-since-access (- current-cycle (decayable-last-accessed decayable))))
      
      (when (> cycles-since-access 0)
        (let ((decay-amount (* (decayable-decay-rate decayable) cycles-since-access (random 0.1))))
          (decf (decayable-integrity decayable) decay-amount)
          
          (when (and (< (decayable-integrity decayable) 0.7)
                     (< (random 1.0) 0.3))
            (corrupt-value decayable)))))))

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
