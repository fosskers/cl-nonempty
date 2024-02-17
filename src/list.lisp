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
  (let ((tail (cl:cons (nelist-head list)
                       (nelist-tail list))))
    (make-nelist :head item :tail tail)))

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