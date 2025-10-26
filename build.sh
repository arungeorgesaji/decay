sbcl --load src/main.lisp --eval "(save-lisp-and-die \"decay\" :toplevel (lambda () (main *posix-argv*)) :executable t)"
