(defclass parser ()
  ((tokens :initarg :tokens :accessor parser-tokens)
   (position :initform 0 :accessor parser-position)
   (current-token :initform nil :accessor parser-current-token)))

(defmethod initialize-instance :after ((parser parser) &key)
  (setf (parser-current-token parser) 
        (when (< (parser-position parser) (length (parser-tokens parser)))
          (nth (parser-position parser) (parser-tokens parser)))))

(defmethod at-end-p ((parser parser))
  (>= (parser-position parser) (length (parser-tokens parser))))

(defmethod advance ((parser parser))
  (incf (parser-position parser))
  (setf (parser-current-token parser)
        (when (not (at-end-p parser))
          (nth (parser-position parser) (parser-tokens parser)))))

(defmethod check ((parser parser) token-type &optional (value nil))
  (and (parser-current-token parser)
       (eq (token-type (parser-current-token parser)) token-type)
       (or (null value) 
           (string= (token-value (parser-current-token parser)) value))))

(defmethod match ((parser parser) token-type &optional (value nil))
  (when (check parser token-type value)
    (advance parser)
    t))

(defmethod consume ((parser parser) token-type &optional (value nil) (error-msg "Expected token"))
  (if (check parser token-type value)
      (prog1 (parser-current-token parser)
        (advance parser))
      (error "~A: expected ~A but got ~A at line ~D, column ~D"
             error-msg token-type 
             (if (parser-current-token parser) 
                 (token-type (parser-current-token parser))
                 "EOF")
             (token-line (parser-current-token parser))
             (token-column (parser-current-token parser)))))

(defmethod parse ((parser parser))
  (make-program (parse-statements parser)))

(defmethod parse-statements ((parser parser))
  (let ((statements '()))
    (loop while (and (not (at-end-p parser))
                     (not (check parser :punctuation "}")))
          do (let ((stmt (parse-statement parser)))
               (when stmt
                 (push stmt statements))))
    (reverse statements)))

(defmethod parse-statement ((parser parser))
  (cond
    ((match parser :keyword "stable")
     (parse-variable-decl parser :stable))
    ((match parser :keyword "volatile")  
     (parse-variable-decl parser :volatile))
    
    ((match parser :keyword "slow")
     (parse-function-decl parser :slow))
    ((match parser :keyword "fast")
     (parse-function-decl parser :fast))
    
    ((match parser :keyword "return")
     (parse-return parser))
    
    ((match parser :keyword "if")
     (parse-if parser))
    
    ((match parser :keyword "while")
     (parse-while parser))
    
    ((match parser :punctuation "{")
     (parse-block parser))
    
    ((match parser :keyword "repair")
     (parse-repair parser))
    ((match parser :keyword "reinforce")
     (parse-reinforce parser))
    ((match parser :keyword "accelerate")
     (parse-accelerate parser))
    
    (t
     (parse-expression-statement parser))))

(defmethod parse-variable-decl ((parser parser) var-type)
  (let* ((name-token (consume parser :identifier nil "Variable name"))
         (name (token-value name-token))
         (line (token-line name-token))
         (column (token-column name-token)))
    
    (consume parser :operator "=" "Expected '=' after variable name")
    (let ((value (parse-expression parser)))
      (make-variable-decl var-type name value :line line :column column))))

(defmethod parse-function-decl ((parser parser) func-type)
  (consume parser :keyword "func" "Expected 'func' after slow/fast")
  
  (let* ((name-token (consume parser :identifier nil "Function name"))
         (name (token-value name-token))
         (line (token-line name-token))
         (column (token-column name-token)))
    
    (consume parser :punctuation "(" "Expected '(' after function name")
    
    (let ((parameters (parse-parameters parser)))
      (consume parser :punctuation ")" "Expected ')' after parameters")
      (consume parser :punctuation "{" "Expected '{' before function body")
      
      (let ((body (parse-block parser)))
        (make-function-decl func-type name parameters body :line line :column column)))))

(defmethod parse-parameters ((parser parser))
  (let ((parameters '()))
    (when (not (check parser :punctuation ")"))
      (let ((param (consume parser :identifier nil "Parameter name")))
        (push (token-value param) parameters))
      (loop while (match parser :punctuation ",")
            do (let ((param (consume parser :identifier nil "Parameter name")))
                 (push (token-value param) parameters))))
    (reverse parameters)))


(defmethod parse-block ((parser parser))
  (let ((line (token-line (parser-current-token parser)))
        (column (token-column (parser-current-token parser)))
        (statements (parse-statements parser)))
    (consume parser :punctuation "}" "Expected '}' after block")
    (make-block statements :line line :column column)))

(defmethod parse-if ((parser parser))
  (let ((line (token-line (parser-current-token parser)))
        (column (token-column (parser-current-token parser))))
    
    (let ((condition (parse-expression parser))
          (then-branch nil)
          (else-branch nil))
      
      (consume parser :punctuation "{" "Expected '{' after if condition")
      (setf then-branch (parse-block parser))
      
      (when (match parser :keyword "else")
        (if (match parser :punctuation "{")
            (setf else-branch (parse-block parser))
            (setf else-branch (parse-if parser)))) 
      
      (make-if condition then-branch else-branch :line line :column column))))

(defmethod parse-while ((parser parser))
  (let ((line (token-line (parser-current-token parser)))
        (column (token-column (parser-current-token parser))))
    
    (let ((condition (parse-expression parser)))
      (consume parser :punctuation "{" "Expected '{' after while condition")
      (let ((body (parse-block parser)))
        (make-while condition body :line line :column column)))))

(defmethod parse-return ((parser parser))
  (let ((line (token-line (parser-current-token parser)))
        (column (token-column (parser-current-token parser)))
        (value (if (check parser :punctuation "}") 
                   nil
                   (parse-expression parser))))
    (make-return value :line line :column column)))

(defmethod parse-repair ((parser parser))
  (parse-decay-command parser 'repair-node "repair"))

(defmethod parse-reinforce ((parser parser))
  (parse-decay-command parser 'reinforce-node "reinforce"))

(defmethod parse-accelerate ((parser parser))
  (parse-decay-command parser 'accelerate-node "accelerate"))

(defmethod parse-decay-command ((parser parser) node-type command-name)
  (let ((line (token-line (parser-current-token parser)))
        (column (token-column (parser-current-token parser))))
    
    (consume parser :punctuation "(" (format nil "Expected '(' after ~A" command-name))
    (let ((target-token (consume parser :identifier nil "Target identifier")))
      (consume parser :punctuation ")" (format nil "Expected ')' after ~A target" command-name))
      
      (let ((target (token-value target-token)))
        (case node-type
          (repair-node (make-repair target :line line :column column))
          (reinforce-node (make-reinforce target :line line :column column))
          (accelerate-node (make-accelerate target :line line :column column)))))))

(defmethod parse-expression ((parser parser))
  (parse-assignment parser))

(defmethod parse-assignment ((parser parser))
  (let ((expr (parse-equality parser)))
    (if (match parser :operator "=")
        (let ((value (parse-assignment parser)))
          (if (typep expr 'variable-node)
              (make-assignment (variable-name expr) value
                               :line (node-line expr) :column (node-column expr))
              (error "Invalid assignment target at line ~D, column ~D"
                     (node-line expr) (node-column expr))))
        expr)))

(defmethod parse-equality ((parser parser))
  (let ((expr (parse-comparison parser)))
    (loop while (or (match parser :operator "==")
                    (match parser :operator "!="))
          do (let ((operator (token-value (nth (1- (parser-position parser)) (parser-tokens parser))))
                   (right (parse-comparison parser)))
               (setf expr (make-binary-op operator expr right
                                         :line (node-line expr) :column (node-column expr)))))
    expr))

(defmethod parse-comparison ((parser parser))
  (let ((expr (parse-term parser)))
    (loop while (or (match parser :operator ">")
                    (match parser :operator ">=")
                    (match parser :operator "<") 
                    (match parser :operator "<="))
          do (let ((operator (token-value (nth (1- (parser-position parser)) (parser-tokens parser))))
                   (right (parse-term parser)))
               (setf expr (make-binary-op operator expr right
                                         :line (node-line expr) :column (node-column expr)))))
    expr))

(defmethod parse-term ((parser parser))
  (let ((expr (parse-factor parser)))
    (loop while (or (match parser :operator "+")
                    (match parser :operator "-"))
          do (let ((operator (token-value (nth (1- (parser-position parser)) (parser-tokens parser))))
                   (right (parse-factor parser)))
               (setf expr (make-binary-op operator expr right
                                         :line (node-line expr) :column (node-column expr)))))
    expr))

(defmethod parse-factor ((parser parser))
  (let ((expr (parse-unary parser)))
    (loop while (or (match parser :operator "*")
                    (match parser :operator "/"))
          do (let ((operator (token-value (nth (1- (parser-position parser)) (parser-tokens parser))))
                   (right (parse-unary parser)))
               (setf expr (make-binary-op operator expr right
                                         :line (node-line expr) :column (node-column expr)))))
    expr))

(defmethod parse-unary ((parser parser))
  (if (or (match parser :operator "-")
          (match parser :operator "!"))
      (let* ((token (parser-current-token parser))
             (operator (token-value token))
             (right (parse-unary parser)))
        (make-unary-op operator right 
                       :line (token-line token)
                       :column (token-column token)))
      (parse-primary parser)))

(defmethod parse-primary ((parser parser))
  (let ((token (parser-current-token parser)))
    (cond
      ((or (match parser :integer)
           (match parser :float)
           (match parser :string))
       (make-literal (token-value token) 
                     :line (token-line token) :column (token-column token)))
      
      ((match parser :identifier)
       (let ((name (token-value token)))
         (if (match parser :punctuation "(")
             (parse-function-call parser name (token-line token) (token-column token))
             (make-variable name :line (token-line token) :column (token-column token)))))
      
      ((match parser :punctuation "(")
       (let ((expr (parse-expression parser)))
         (consume parser :punctuation ")" "Expected ')' after expression")
         expr))
      
      (t
       (error "Expected expression but got ~A at line ~D, column ~D"
              (if (parser-current-token parser)
                  (token-type (parser-current-token parser))
                  "EOF")
              (token-line (parser-current-token parser))
              (token-column (parser-current-token parser)))))))

(defmethod parse-function-call ((parser parser) name line column)
  (let ((arguments '()))
    (when (not (check parser :punctuation ")"))
      (loop do (push (parse-expression parser) arguments)
            while (match parser :punctuation ",")))
    (consume parser :punctuation ")" "Expected ')' after function arguments")
    (make-function-call name (reverse arguments) :line line :column column)))

(defmethod parse-expression-statement ((parser parser))
  (let ((expr (parse-expression parser)))
    expr))

(defun parse-tokens (tokens)
  (let ((parser (make-instance 'parser :tokens tokens)))
    (parse parser)))

(defun print-ast (node &optional (indent 0))
  (let ((prefix (make-string indent :initial-element #\Space)))
    (cond
      ((typep node 'program-node)
       (format t "~APROGRAM~%" prefix)
       (dolist (stmt (program-statements node))
         (print-ast stmt (+ indent 2))))

      ((typep node 'variable-decl-node)
       (format t "~ADECL: ~A ~A = ~A~%"
               prefix
               (decl-var-type node)
               (decl-name node)
               (literal-value (decl-value node))))

      ((typep node 'function-decl-node)
       (format t "~AFUNCTION: ~A ~A(~{~A~^, ~})~%"
               prefix
               (decl-func-type node)
               (decl-func-name node)
               (decl-parameters node))
       (print-ast (decl-body node) (+ indent 2)))

      ((typep node 'block-node)
       (format t "~ABLOCK~%" prefix)
       (dolist (stmt (block-statements node))
         (print-ast stmt (+ indent 2))))

      ((typep node 'return-node)
       (format t "~ARETURN (~A)~%"
               prefix
               (if (typep (return-value node) 'binary-op-node)
                   (format nil "~A ~A ~A"
                           (binary-op-left (return-value node))
                           (binary-op-operator (return-value node))
                           (binary-op-right (return-value node)))
                   (return-value node))))

      ((typep node 'if-node)
       (format t "~AIF (~A)~%" prefix (if-condition node))
       (format t "~A THEN:~%" (+ indent 2))
       (print-ast (if-then node) (+ indent 4))
       (when (if-else node)
         (format t "~A ELSE:~%" (+ indent 2))
         (print-ast (if-else node) (+ indent 4))))

      ((typep node 'while-node)
       (format t "~AWHILE (~A)~%" prefix (while-condition node))
       (print-ast (while-body node) (+ indent 2)))

      ((typep node 'binary-op-node)
       (format t "~ABINOP (~A ~A ~A)~%"
               prefix
               (binary-op-left node)
               (binary-op-operator node)
               (binary-op-right node)))

      ((typep node 'literal-node)
       (format t "~ALITERAL (~A)~%" prefix (literal-value node)))

      ((typep node 'variable-node)
       (format t "~AVARIABLE (~A)~%" prefix (variable-name node)))

      ((typep node 'assignment-node)
       (format t "~AASSIGN: ~A = ~A~%"
               prefix
               (assignment-variable node)
               (assignment-value node)))

      ((typep node 'function-call-node)
       (format t "~AFUNCALL: ~A(~{~A~^, ~})~%"
               prefix
               (function-call-name node)
               (function-call-arguments node)))

      ((typep node 'repair-node)
       (format t "~AREPAIR: ~A~%" prefix (repair-target node)))

      ((typep node 'reinforce-node)
       (format t "~AREINFORCE: ~A~%" prefix (reinforce-target node)))

      ((typep node 'accelerate-node)
       (format t "~AACCELERATE: ~A~%" prefix (accelerate-target node)))

      ((typep node 'health-check-node)
       (format t "~AHEALTH-CHECK (~A) on ~A~%"
               prefix
               (health-check-type node)
               (health-check-target node)))


      (t (format t "~A<UNKNOWN NODE> ~A~%" prefix node)))))
