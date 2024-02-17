(defpackage nonempty/tests
  (:use :cl :parachute)
  (:local-nicknames (:ne :nonempty)))

(in-package :nonempty/tests)

(define-test suite)

(define-test "Lists"
  :parent suite
  (is equal 1 (ne:car (ne:nel 1 2 3))))
