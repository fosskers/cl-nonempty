(defpackage nonempty/tests
  (:use :cl :parachute)
  (:local-nicknames (:ne :nonempty)
                    (:nt :nonempty/transducers)
                    (:t :transducers)))

(in-package :nonempty/tests)

(define-test suite)

(define-test "Lists"
  :parent suite
  (is equalp (ne:nel 1 2 3) (ne:nel 1 2 3))
  (is equalp 1 (ne:car (ne:nel 1 2 3)))
  (is equalp (ne:nel 2 3 4) (ne:map #'1+ (ne:nel 1 2 3)))
  (is equalp (list 2 4 6) (ne:filter #'evenp (ne:nel 1 2 3 4 5 6)))
  (is equal 6 (ne:fold #'+ (ne:nel 1 2 3)))
  (is equal 16 (ne:fold #'+ (ne:nel 1 2 3) :seed 10)))

(define-test "Generics"
  :parent suite
  (is equal 3 (ne:length (ne:nel 1 2 3)))
  (is equalp (ne:nel 3 2 1) (ne:reverse (ne:nel 1 2 3)))
  (is equalp (ne:nel 1) (ne:reverse (ne:nel 1)))
  (is equal 3 (ne:elt (ne:nel 1 2 3) 2))
  (false (ne:elt (ne:nel 1 2 3) 20))
  (is equal (list 1 2 3) (ne:to-list (ne:nel 1 2 3)))
  (is equal 3 (ne:last (ne:nel 1 2 3)))
  (is equal 1 (ne:last (ne:nel 1)))
  (is equalp (ne:nel 1 2 3 4 5 6) (ne:append (ne:nel 1 2 3) (ne:nel 4 5 6)))
  (is equalp (ne:nel 1 2 3 4 5 6) (ne:append (ne:nel 1 2 3) '(4 5 6))))

(define-test "Transducers"
  :parent suite
  (is equalp (list 2 3 4) (t:transduce (t:map #'1+) #'t:cons (ne:nel 1 2 3)))
  (is equalp (ne:nel 2 3 4) (t:transduce (t:map #'1+) #'nt:nelist (ne:nel 1 2 3)))
  (fail (t:transduce (t:filter #'evenp) #'nt:nelist (ne:nel 1 3 5))))
