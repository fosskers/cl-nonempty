(defpackage nonempty/transducers
  (:use :cl)
  (:local-nicknames (:ne :nonempty)
                    (:t :transducers))
  (:documentation "Transducers support for nonempty."))

(in-package :nonempty/transducers)

(defmethod t:transduce (xform f (source ne:nelist))
  (t:transduce xform f (ne:to-list source)))

#+nil
(t:transduce (t:map #'1+) #'t:cons (ne:nel 1 2 3))

(defun nelist (&optional (acc nil a-p) (input nil i-p))
  "Reducer: Collect all results into a non-empty list.

# Conditions

- `transducers:empty-transduction': when no values made it through the transduction."
  (cond ((and a-p i-p) (cons input acc))
        ((and a-p (not i-p) acc) (ne::make-nelist :head (car acc)
                                                  :tail (cdr acc)))
        ((and a-p (not i-p)) (error 't:empty-transduction :msg "nelist: the transduction was empty."))
        (t '())))

#+nil
(t:transduce (t:filter #'evenp) #'nelist (ne:nel 1 2 3 5))
#+nil
(t:transduce (t:filter #'evenp) #'nelist (ne:nel 1 3 5))
