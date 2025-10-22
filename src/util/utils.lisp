(defun print-tokens (tokens)
  (format t "~&=== Tokens ===~%")
  (dolist (token tokens)
    (format t "~10a: '~a' (line ~a, col ~a)~%"
            (token-type token)
            (token-value token)
            (token-line token)
            (token-column token)))
  (format t "===============~%"))

(defun test-lexer ()
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
    
    (format t "Testing lexer with sample code:~%~%~a~%~%" code)
    (let ((tokens (tokenize code)))
      (print-tokens tokens))))
