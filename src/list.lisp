(in-package :nonempty)

(defstruct nelist
  "A list guaranteed to contain at least one item."
  (head nil :read-only t)
  (tail nil :read-only t :type list))

(defun nel (item &rest items)
  "Construct a non-empty list from at least one input argument."
  (make-nelist :head item :tail items))

#+nil
(nel 1 2 3)

(declaim (ftype (function (t nelist) nelist) cons))
(defun cons (item list)
  "Append a new item onto the front of a non-empty list."
  (make-nelist :head item :tail (to-list list)))

#+nil
(cons 0 (nel 1 2 3))

(declaim (ftype (function (nelist) t) car))
(defun car (items)
  "The first element of a non-empty list."
  (nelist-head items))

#+nil
(car (nel 1 2 3))

(declaim (ftype (function (nelist) list) cdr))
(defun cdr (items)
  "The possibly-empty tail of a non-empty list."
  (nelist-tail items))

#+nil
(cdr (nel 1 2 3))
#+nil
(cdr (nel 1))

(declaim (ftype (function ((function (t) *) nelist) nelist) map))
(defun map (fn items)
  "Map some FN over the given ITEMS, yielding a new non-empty list."
  (make-nelist :head (funcall fn (nelist-head items))
               :tail (mapcar fn (nelist-tail items))))

#+nil
(map #'1+ (nel 1 2 3))

(declaim (ftype (function ((function (t) boolean) nelist) list) filter))
(defun filter (pred items)
  "Keep ITEMS for which a PRED function succeeds."
  (remove-if-not pred (to-list items)))

#+nil
(filter #'evenp (nel 1 2 3 4 5 6))

(declaim (ftype (function ((function (t t) *) nelist &key (:seed t)) *) fold))
(defun fold (fn items &key (seed nil seedp))
  "Reduce some ITEMS via a 2-arity FN.

The first argument to FN is the accumulator value, and the second is the current
item in the list."
  (if seedp
      (reduce fn (to-list items) :initial-value seed)
      (reduce fn (to-list items))))

#+nil
(fold #'+ (nel 1 2 3))
#+nil
(fold #'+ (nel 1 2 3) :seed 10)
