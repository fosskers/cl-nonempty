(defpackage nonempty
  (:use :cl))

(in-package :nonempty)

(defstruct nelist
  "A list guaranteed to contain at least one item."
  (head nil :read-only t)
  (tail nil :read-only t :type list))

;; defgeneric singleton
;; defgeneric from-list

(defun nel (item &rest items)
  "Construct a non-empty list from at least one input argument."
  (make-nelist :head item :tail items))

#+nil
(nel 1 2 3)
