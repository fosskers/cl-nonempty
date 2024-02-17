(defpackage nonempty/tests/main
  (:use :cl
        :nonempty
        :rove))
(in-package :nonempty/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :nonempty)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
