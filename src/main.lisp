(load "src/lexer/tokens.lisp")
(load "src/lexer/lexer.lisp")
(load "src/parser/ast-node.lisp")
(load "src/parser/parser.lisp")
(load "src/util/decay-registry.lisp")
(load "src/decay/decay-mechanics.lisp")
(load "src/interpreter/interpreter.lisp")
(load "src/interpreter/environment.lisp")
(load "src/interpreter/evaluation.lisp")

(defun run-decay-file (filename)
  (when (probe-file filename)
    (let* ((source (with-open-file (in filename
                                       :direction :input
                                       :if-does-not-exist nil)
                     (when in
                       (with-output-to-string (out)
                         (loop for line = (read-line in nil)
                               while line
                               do (format out "~A~%" line))))))
           (interp (make-instance 'interpreter)))

      (format t "~%Running: ~A~%" filename)
      (format t "==============================~%")

      (let* ((tokens (tokenize source))
             (ast (parse-tokens tokens))
             (result (evaluate interp ast)))
        (format t "~%Execution finished.~%")
        (format t "Result: ~A~%" (decayable-value result))
        (format t "Integrity: ~,2f~%" (decayable-integrity result))
        (format t "Stability budget: ~,2f/~,2f~%~%"
                (interpreter-current-budget interp)
                (interpreter-stability-budget interp))))))

(defun main (args)
  (let ((argv (rest args)))  
    (if (and argv (first argv))
        (run-decay-file (first argv))
        (format t "Usage: decay <filename.decay>~%"))))
