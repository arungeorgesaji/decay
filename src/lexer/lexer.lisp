(defclass lexer ()
  ((source :initarg :source      
           :accessor lexer-source
           :type string)
   (position :initform 0         
             :accessor lexer-position
             :type integer)
   (line :initform 1             
         :accessor lexer-line
         :type integer)
   (column :initform 1           
           :accessor lexer-column
           :type integer)))

(defun tokenize (source-code)
  (let ((lexer (make-instance 'lexer :source source-code))
        (tokens '()))  
    
    (loop while (not (at-end-p lexer)) 
          do (let ((token (next-token lexer)))
               (when token  
                 (push token tokens))))
    
    (reverse tokens)))

(defmethod at-end-p ((lexer lexer))
  (>= (lexer-position lexer) (length (lexer-source lexer))))

(defmethod current-char ((lexer lexer))
  (when (not (at-end-p lexer))
    (char (lexer-source lexer) (lexer-position lexer))))

(defmethod advance ((lexer lexer))
  (when (not (at-end-p lexer))
    (let ((char (current-char lexer)))
      (if (char= char #\Newline)
          (progn
            (incf (lexer-line lexer))
            (setf (lexer-column lexer) 1))
          (incf (lexer-column lexer))))
    (incf (lexer-position lexer))))

(defmethod skip-whitespace ((lexer lexer))
  (loop while (and (not (at-end-p lexer))
                   (whitespace-char-p (current-char lexer)))
        do (advance lexer)))

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defmethod skip-comment ((lexer lexer))
  (loop while (and (not (at-end-p lexer))
                   (not (char= (current-char lexer) #\Newline)))
        do (advance lexer)))

(defmethod next-token ((lexer lexer))
  (skip-whitespace lexer)
  
  (when (at-end-p lexer)
    (return-from next-token nil))
  
  (let ((char (current-char lexer))
        (line (lexer-line lexer))
        (column (lexer-column lexer)))
    
    (cond
      ((or (digit-char-p char) (char= char #\.))
       (read-number lexer line column))
      
      ((char= char #\")
       (read-string lexer line column))
      
      ((or (alpha-char-p char) (char= char #\_))
       (read-identifier lexer line column))
      
      ((operator-char-p char)
       (read-operator lexer line column))
      
      ((punctuation-char-p char)
       (read-punctuation lexer line column))
      
      ((char= char #\#)
       (skip-comment lexer)
       (next-token lexer))  
      
      (t
       (error "Lexer error: Unexpected character '~a' at line ~d, column ~d" 
              char line column)))))

(defun operator-char-p (char)
  (find char "+-*/=<>!" :test #'char=))

(defun punctuation-char-p (char)
  (find char "(){}[],;." :test #'char=))

(defmethod read-number ((lexer lexer) line column)
  (let ((start (lexer-position lexer))
        (has-decimal-point nil))
    
    (loop while (and (not (at-end-p lexer))
                     (or (digit-char-p (current-char lexer))
                         (and (char= (current-char lexer) #\.) 
                              (not has-decimal-point))))
          do (when (char= (current-char lexer) #\.)
               (setf has-decimal-point t))
             (advance lexer))
    
    (let ((value (subseq (lexer-source lexer) start (lexer-position lexer))))
      (make-token :type (if has-decimal-point :float :integer)
                  :value value
                  :line line
                  :column column))))

(defmethod read-string ((lexer lexer) line column)
  (advance lexer) 
  
  (let ((start (lexer-position lexer)))
    (loop while (and (not (at-end-p lexer))
                     (not (char= (current-char lexer) #\")))
          do (advance lexer))
    
    (when (at-end-p lexer)
      (error "Unterminated string at line ~d, column ~d" line column))
    
    (let ((value (subseq (lexer-source lexer) start (lexer-position lexer))))
      (advance lexer) 
      (make-token :type :string
                  :value value
                  :line line
                  :column column))))

(defmethod read-identifier ((lexer lexer) line column)
  (let ((start (lexer-position lexer)))
    
    (loop while (and (not (at-end-p lexer))
                     (or (alpha-char-p (current-char lexer))
                         (digit-char-p (current-char lexer))
                         (char= (current-char lexer) #\_)))
          do (advance lexer))
    
    (let ((value (subseq (lexer-source lexer) start (lexer-position lexer))))
      (make-token :type (if (member value *keywords* :test #'string=)
                          :keyword
                          :identifier)
                  :value value
                  :line line
                  :column column))))

(defmethod read-operator ((lexer lexer) line column)
  (let* ((start (lexer-position lexer))
         (char1 (current-char lexer))
         (value (string char1)))  
    (advance lexer)
    (when (and (not (at-end-p lexer))
               (member (concatenate 'string value (string (current-char lexer)))
                       '("==" "!=" "<=" ">=") :test #'string=))
      (setf value (concatenate 'string value (string (current-char lexer))))
      (advance lexer))
    (make-token :type :operator
                :value value
                :line line
                :column column)))

(defmethod read-punctuation ((lexer lexer) line column)
  (let ((char (current-char lexer)))
    (advance lexer)
    (make-token :type :punctuation
                :value (string char)
                :line line
                :column column)))
