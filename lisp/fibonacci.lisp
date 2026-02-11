;;;; Fibonacci number calculator.

(defun fibonacci (n)
  "Return the nth Fibonacci number (0-indexed).
   fib(0) = 0, fib(1) = 1, fib(2) = 1, fib(3) = 2, ..."
  (when (< n 0)
    (error "n must be non-negative"))
  (if (<= n 1)
      n
      (loop with a = 0 and b = 1
            for i from 2 to n
            do (psetq a b b (+ a b))
            finally (return b))))

;;; Command-line entry point
(defun main ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (if (/= (length args) 1)
        (progn
          (format t "Usage: sbcl --script fibonacci.lisp <n>~%")
          (sb-ext:exit :code 1))
        (let ((n (parse-integer (first args) :junk-allowed nil)))
          (if (null n)
              (progn
                (format t "Error: argument must be an integer~%")
                (sb-ext:exit :code 1))
              (format t "fib(~a) = ~a~%" n (fibonacci n)))))))

;; Only run main when executed as a script (not when loaded for tests)
(when (and (boundp 'sb-ext:*posix-argv*)
           (not (boundp '*loading-for-test*)))
  (main))
