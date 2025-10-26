(load "lexer/tokens.lisp")
(load "lexer/lexer.lisp")
(load "parser/ast-node.lisp")
(load "parser/parser.lisp")
(load "util/decay-registry.lisp")
(load "decay/decay-mechanics.lisp")
(load "interpreter/interpreter.lisp")
(load "interpreter/environment.lisp")
(load "interpreter/evaluation.lisp")

(defun string-search (substring string)
  (and (stringp substring) (stringp string) 
       (search substring string)))

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
}")
        (simple-code "stable x = 10
                     volatile y = 5
                     if x > y {
                         x + y
                     } else {
                         x - y
                     }")
        (math-code "5 + 3 * 2")
        (interp (make-instance 'interpreter)))

    (format t "======================~%")
    (format t "    DECAY TEST   ~%")
    (format t "======================~%~%")
    
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
      (format t "Building Abstract Syntax Tree...~%~%")
      
      (let* ((ast (parse-tokens tokens)))
        
        (format t "PROGRAM AST STRUCTURE:~%")
        (format t "======================~%")
        (print-ast ast)
        
        (format t "~%4. INTERPRETER & DECAY SYSTEMS:~%")
        (format t "===============================~%")
        
        (format t "4.1 Basic Interpreter Execution~%")
        (format t "--------------------------------~%")
        (format t "Simple code: ~A~%" simple-code)
        (let* ((simple-tokens (tokenize simple-code))
              (simple-ast (parse-tokens simple-tokens))
              (result (evaluate interp simple-ast)))
          (format t "Result: ~A (integrity: ~,2f)~%" 
                  (decayable-value result)
                  (decayable-integrity result))
          (format t "Stability budget: ~,2f/~,2f~%~%" 
            (interpreter-current-budget interp)
            (interpreter-stability-budget interp)))
        
        (format t "4.2 Decay Mechanics Test~%")
        (format t "------------------------~%")
        (format t "Creating decayable objects...~%")
        
        (let ((stable-var (make-decayable interp 42 :integrity 0.9 :decay-rate 0.001)))
          (format t "Stable variable: value=~A, integrity=~,2f, decay-rate=~,3f~%"
                  (decayable-value stable-var)
                  (decayable-integrity stable-var)
                  (decayable-decay-rate stable-var)))
        
        (let ((volatile-var (make-decayable interp "hello" :integrity 0.7 :decay-rate 0.1)))
          (format t "Volatile variable: value=~A, integrity=~,2f, decay-rate=~,3f~%"
                  (decayable-value volatile-var)
                  (decayable-integrity volatile-var)
                  (decayable-decay-rate volatile-var)))
        
        (format t "~%Simulating corruption...~%")
        (let ((test-number (make-decayable interp 100 :integrity 0.3))) 
          (format t "Before corruption: ~A~%" (decayable-value test-number))
          (corrupt-value test-number)
          (format t "After corruption: ~A~%" (decayable-value test-number)))
        (format t "~%")
        
        (format t "4.3 Variable Declarations Test~%")
        (format t "------------------------------~%")
        (let ((var-code "stable permanent = 100
                         volatile temporary = 50
                         permanent + temporary"))
          (format t "Testing variable types: ~A~%" var-code)
          (let* ((var-tokens (tokenize var-code))
                (var-ast (parse-tokens var-tokens))
                (var-result (evaluate interp var-ast)))
            (format t "Result: ~A~%" (decayable-value var-result))
            
            (format t "Environment contents:~%")
            (maphash (lambda (key value)
                       (format t "  ~A: value=~A, integrity=~,2f, decay-rate=~,3f~%"
                               key 
                               (decayable-value value)
                               (decayable-integrity value)
                               (decayable-decay-rate value)))
                     (interpreter-global-env interp)))
          (format t "~%"))
        
        (format t "4.4 Stability Budget System Test~%")
        (format t "--------------------------------~%")
        (format t "Initial budget: ~,2f/~,2f~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp))
        
        (check-stability-budget interp :stable-var-access)
        (consume-stability-budget interp :stable-var-access)
        (format t "After stable var: ~,2f/~,2f~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp))

        (check-stability-budget interp :volatile-var-access)
        (consume-stability-budget interp  :volatile-var-access)
        (format t "After volatile var: ~,2f/~,2f~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp))
        
        (restore-stability-budget interp 10.0)  
(format t "After restoration: ~,2f/~,2f~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp))
        
        (format t "~%Testing maintenance operations:~%")
        (evaluate interp (make-repair "test_variable"))
        (evaluate interp (make-reinforce "test_function")) 
        (evaluate interp (make-accelerate "test_process"))
        (format t "Final budget: ~,2f/~,2f~%~%" 
          (interpreter-current-budget interp)
          (interpreter-stability-budget interp))
        
        (format t "4.5 Math Operations Test~%")
        (format t "------------------------~%")
        (let ((test-cases '(("5 + 3" 8)
                            ("10 - 4" 6)
                            ("6 * 7" 42)
                            ("15 / 3" 5)
                            ("5 == 5" 1)
                            ("5 != 5" 0)
                            ("10 > 5" 1)
                            ("5 < 10" 1)
                            ("-5" -5)
                            ("!0" 1))))
          
          (dolist (test-case test-cases)
            (let ((code (first test-case))
                  (expected (second test-case)))
              (format t "Testing: ~A => " code)
              (let* ((tokens (tokenize code))
                    (ast (parse-tokens tokens))
                    (result (evaluate interp ast)))
                (format t "~A (expected: ~A)~%" (decayable-value result) expected))))
          (format t "~%"))
        
        (format t "4.6 Function Test~%")
        (format t "-----------------------------~%")

        (let ((function-tests 
       '(("Basic function call" 
          "slow func add(a, b) { return a + b } result = add(5, 3)" 
          "result" 8)
         
         ("Multiple parameters" 
          "slow func multiply(a, b, c) { return a * b * c } product = multiply(2, 3, 4)" 
          "product" 24)
         
         ("Function with conditionals" 
          "slow func max(a, b) { if a > b { return a } return b } max_val = max(17, 42)" 
          "max_val" 42)
         
         ("Global variable access" 
          "stable counter = 100 slow func increment() { return counter + 1 } new_count = increment()" 
          "new_count" 101)
         
         ("Nested function calls" 
          "slow func square(x) { return x * x } slow func sum_squares(a, b) { return square(a) + square(b) } result = sum_squares(3, 4)" 
          "result" 25)
         
         ("Fast vs Slow functions" 
          "slow func slow_add(x) { return x + 10 } fast func fast_mul(x) { return x * 2 } slow_result = slow_add(5) fast_result = fast_mul(5)" 
          "slow_result,fast_result" "slow:15 fast:10"))))
  
  (dolist (test function-tests)
    (let ((test-name (first test))
          (test-code (second test))
          (result-var (third test))
          (expected (fourth test)))
      (format t "~A:~%" test-name)
      (format t "  Code: ~A~%" test-code)
      
      (handler-case
          (let* ((tokens (tokenize test-code))
                 (ast (parse-tokens tokens))
                 (result (evaluate interp ast)))
            
            (let ((actual-result 
                   (cond
                     ((string= test-name "Fast vs Slow functions")
                      (let ((slow-result (gethash "slow_result" (interpreter-global-env interp)))
                            (fast-result (gethash "fast_result" (interpreter-global-env interp))))
                        (format nil "slow:~A fast:~A" 
                                (decayable-value slow-result)
                                (decayable-value fast-result))))
                     (t
                      (let ((result-value (gethash result-var (interpreter-global-env interp))))
                        (decayable-value result-value))))))
              
              (format t "  Result: ~A~%" actual-result)
              
              (format t "  Status: ~A~%" 
                      (if (equal actual-result expected)
                          "PASS" 
                          (format nil "FAIL (expected: ~A)" expected)))))
        
        (error (e) 
          (format t "  Status: ERROR: ~A~%" e)))
      
      (format t "~%"))))

        (format t "========================================~%")
        (format t "           TEST COMPLETE               ~%")
        (format t "========================================~%")
        
        ast))))

(test-decay)
