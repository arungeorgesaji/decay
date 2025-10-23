(defun test-decay ()
  (let ((code "stable PI = 3.14159
volatile temp = 0

slow func add(a, b) {
    return a + b
}

fast func process() {
    if temp > 10 {
        return temp * 2
    }
    return 1
}"))

    (format t "1. SOURCE CODE:~%")
    (format t "---------------~%")
    (format t "~A~%~%" code)
    
    (format t "2. TOKENIZATION:~%")
    (format t "----------------~%")
    (let ((tokens (tokenize code)))
      (format t "Found ~D tokens:~%~%" (length tokens))
      (print-tokens tokens))
    
    (format t "~%3. AST CONSTRUCTION:~%")
    (format t "--------------------~%")
    (format t "Building Abstract Syntax Tree...~%~%")
    
    (let* ((pi-literal (make-literal "3.14159"))
           (pi-decl (make-variable-decl :stable "PI" pi-literal))
           (zero-literal (make-literal "0"))
           (temp-decl (make-variable-decl :volatile "temp" zero-literal))
           (param-a (make-variable "a"))
           (param-b (make-variable "b"))
           (add-expr (make-binary-op "+" param-a param-b))
           (add-return (make-return add-expr))
           (add-body (make-block (list add-return)))
           (add-func (make-function-decl :slow "add" (list "a" "b") add-body))
           (temp-ref (make-variable "temp"))
           (ten-literal (make-literal "10"))
           (condition (make-binary-op ">" temp-ref ten-literal))
           (temp-ref2 (make-variable "temp"))
           (two-literal (make-literal "2"))
           (mult-expr (make-binary-op "*" temp-ref2 two-literal))
           (then-return (make-return mult-expr))
           (then-block (make-block (list then-return)))
           (one-literal (make-literal "1"))
           (else-return (make-return one-literal))
           (if-stmt (make-if condition then-block else-return))
           (process-body (make-block (list if-stmt)))
           (process-func (make-function-decl :fast "process" '() process-body))
           (program (make-program (list pi-decl temp-decl add-func process-func) :line 1 :column 1)))
      
      (format t "PROGRAM AST STRUCTURE:~%")
      (format t "======================~%")
      (format t "Program (root) at line 1, column 1~%")
      (format t "├── VariableDecl: stable PI = ~A~%" pi-literal)
      (format t "├── VariableDecl: volatile temp = ~A~%" zero-literal) 
      (format t "├── FunctionDecl: slow func add(a, b)~%")
      (format t "│   └── Block~%")
      (format t "│       └── Return~%")
      (format t "│           └── BinaryOp: ~A + ~A~%" param-a param-b)
      (format t "└── FunctionDecl: fast func process()~%")
      (format t "    └── Block~%")
      (format t "        └── If statement~%")
      (format t "            ├── Condition: ~A > ~A~%" temp-ref ten-literal)
      (format t "            ├── Then: Block~%")
      (format t "            │   └── Return: ~A * ~A~%" temp-ref2 two-literal)
      (format t "            └── Else: Return: ~A~%" one-literal)
      
      program)))
