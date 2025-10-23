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
      (print-tokens tokens)
      
      (format t "~%3. PARSING:~%")
      (format t "------------~%")
      (format t "Building Abstract Syntax Tree automatically...~%~%")
      
      (let ((ast (parse-tokens tokens)))
        
        (format t "PROGRAM AST STRUCTURE:~%")
        (format t "======================~%")
        (print-ast ast)
        
        ast))))
