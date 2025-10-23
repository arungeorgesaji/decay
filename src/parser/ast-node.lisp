(defclass ast-node ()
  ((line :initarg :line :accessor node-line :initform 1)
   (column :initarg :column :accessor node-column :initform 1)))

(defclass literal-node (ast-node)
  ((value :initarg :value :accessor literal-value)))

(defclass variable-node (ast-node)
  ((name :initarg :name :accessor variable-name)))

(defclass binary-op-node (ast-node)
  ((operator :initarg :operator :accessor binary-op-operator)
   (left :initarg :left :accessor binary-op-left)            
   (right :initarg :right :accessor binary-op-right)))

(defclass function-call-node (ast-node)
  ((function-name :initarg :function-name :accessor function-call-name)
   (arguments :initarg :arguments :accessor function-call-arguments)))

(defclass variable-decl-node (ast-node)
  ((var-type :initarg :var-type :accessor decl-var-type)
   (name :initarg :name :accessor decl-name)
   (value :initarg :value :accessor decl-value)))

(defclass assignment-node (ast-node)
  ((variable :initarg :variable :accessor assignment-variable)
   (value :initarg :value :accessor assignment-value)))

(defclass return-node (ast-node)
  ((value :initarg :value :accessor return-value)))

(defclass if-node (ast-node)
  ((condition :initarg :condition :accessor if-condition)
   (then-branch :initarg :then-branch :accessor if-then)
   (else-branch :initarg :else-branch :accessor if-else)))

(defclass while-node (ast-node)
  ((condition :initarg :condition :accessor while-condition)
   (body :initarg :body :accessor while-body)))

(defclass block-node (ast-node)
  ((statements :initarg :statements :accessor block-statements)))

(defclass function-decl-node (ast-node)
  ((func-type :initarg :func-type :accessor decl-func-type)
   (name :initarg :name :accessor decl-func-name)
   (parameters :initarg :parameters :accessor decl-parameters)
   (body :initarg :body :accessor decl-body)))

(defclass repair-node (ast-node)
  ((target :initarg :target :accessor repair-target)))

(defclass reinforce-node (ast-node)
  ((target :initarg :target :accessor reinforce-target)))

(defclass accelerate-node (ast-node)
  ((target :initarg :target :accessor accelerate-target)))

(defclass health-check-node (ast-node)
  ((check-type :initarg :check-type :accessor health-check-type)
   (target :initarg :target :accessor health-check-target)))

(defclass program-node (ast-node)
  ((statements :initarg :statements :accessor program-statements)))

(defun make-literal (value &key (line 1) (column 1))
  (make-instance 'literal-node :value value :line line :column column))

(defun make-variable (name &key (line 1) (column 1))
  (make-instance 'variable-node :name name :line line :column column))

(defun make-binary-op (operator left right &key (line 1) (column 1))
  (make-instance 'binary-op-node :operator operator :left left :right right 
                                  :line line :column column))

(defun make-function-call (name arguments &key (line 1) (column 1))
  (make-instance 'function-call-node :function-name name :arguments arguments
                                     :line line :column column))

(defun make-variable-decl (var-type name value &key (line 1) (column 1))
  (make-instance 'variable-decl-node :var-type var-type :name name :value value
                                     :line line :column column))

(defun make-assignment (variable value &key (line 1) (column 1))
  (make-instance 'assignment-node :variable variable :value value
                                  :line line :column column))

(defun make-return (value &key (line 1) (column 1))
  (make-instance 'return-node :value value :line line :column column))

(defun make-if (condition then-branch &optional else-branch (line 1) (column 1))
  (make-instance 'if-node :condition condition :then-branch then-branch 
                          :else-branch else-branch :line line :column column))

(defun make-while (condition body &key (line 1) (column 1))
  (make-instance 'while-node :condition condition :body body :line line :column column))

(defun make-block (statements &key (line 1) (column 1))
  (make-instance 'block-node :statements statements :line line :column column))

(defun make-function-decl (func-type name parameters body &key (line 1) (column 1))
  (make-instance 'function-decl-node :func-type func-type :name name 
                                     :parameters parameters :body body
                                     :line line :column column))

(defun make-repair (target &key (line 1) (column 1))
  (make-instance 'repair-node :target target :line line :column column))

(defun make-reinforce (target &key (line 1) (column 1))
  (make-instance 'reinforce-node :target target :line line :column column))

(defun make-accelerate (target &key (line 1) (column 1))
  (make-instance 'accelerate-node :target target :line line :column column))

(defun print-tokens (tokens)
  (dolist (token tokens)
    (format t "~A (~A) at line ~A, column ~A~%"
            (token-value token)
            (token-type token)
            (token-line token)
            (token-column token))))

(defun make-program (statements &key (line 1) (column 1))
  (make-instance 'program-node :statements statements :line line :column column))

(defmethod print-object ((node literal-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~S" (literal-value node))))

(defmethod print-object ((node variable-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~S" (variable-name node))))

(defmethod print-object ((node binary-op-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A ~A ~A" 
            (binary-op-left node)
            (binary-op-operator node)
            (binary-op-right node))))

(defmethod print-object ((node function-call-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A(~{~A~^, ~})" 
            (function-call-name node)
            (function-call-arguments node))))

(defmethod print-object ((node variable-decl-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A ~A = ~A" 
            (decl-var-type node)
            (decl-name node)
            (decl-value node))))

(defmethod print-object ((node function-decl-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A func ~A(~{~A~^, ~})" 
            (decl-func-type node)
            (decl-func-name node)
            (decl-parameters node))))

(defmethod print-object ((node program-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~D statements" (length (program-statements node)))))
