;;;; Tests for fibonacci function.

(defvar *loading-for-test* t)
(load "fibonacci.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro assert-equal (expected actual description)
  `(if (equal ,expected ,actual)
       (progn
         (incf *tests-passed*)
         (format t "  PASS: ~a~%" ,description))
       (progn
         (incf *tests-failed*)
         (format t "  FAIL: ~a (expected ~a, got ~a)~%"
                 ,description ,expected ,actual))))

(defmacro assert-error (expr description)
  `(if (handler-case (progn ,expr nil)
         (error () t))
       (progn
         (incf *tests-passed*)
         (format t "  PASS: ~a~%" ,description))
       (progn
         (incf *tests-failed*)
         (format t "  FAIL: ~a (expected an error)~%" ,description))))

(defun run-tests ()
  (format t "Running Fibonacci tests...~%~%")

  (format t "Base cases:~%")
  (assert-equal 0 (fibonacci 0) "fib(0) = 0")
  (assert-equal 1 (fibonacci 1) "fib(1) = 1")

  (format t "~%Small values:~%")
  (assert-equal 1  (fibonacci 2)  "fib(2) = 1")
  (assert-equal 2  (fibonacci 3)  "fib(3) = 2")
  (assert-equal 3  (fibonacci 4)  "fib(4) = 3")
  (assert-equal 5  (fibonacci 5)  "fib(5) = 5")
  (assert-equal 8  (fibonacci 6)  "fib(6) = 8")

  (format t "~%Larger values:~%")
  (assert-equal 55     (fibonacci 10) "fib(10) = 55")
  (assert-equal 6765   (fibonacci 20) "fib(20) = 6765")
  (assert-equal 832040 (fibonacci 30) "fib(30) = 832040")

  (format t "~%Error handling:~%")
  (assert-error (fibonacci -1) "fib(-1) raises error")

  (format t "~%Results: ~a passed, ~a failed~%" *tests-passed* *tests-failed*)
  (sb-ext:exit :code (if (zerop *tests-failed*) 0 1)))

(run-tests)
