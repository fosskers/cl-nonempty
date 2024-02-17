(in-package :nonempty)

(defgeneric length (items)
  (:documentation "The length of the given non-empty structure."))

(defmethod length ((items nelist))
  (1+ (cl:length (nelist-tail items))))

#+nil
(length (nel 1 2 3))

(defgeneric reverse (items)
  (:documentation "The reverse of the given non-empty structure."))

(defmethod reverse ((items nelist))
  (let ((revd (cl:reverse (cl:cons (nelist-head items)
                                   (nelist-tail items)))))
    (make-nelist :head (cl:car revd) :tail (cl:cdr revd))))

#+nil
(reverse (nel 1 2 3))
#+nil
(reverse (nel 1))

(defgeneric elt (items index)
  (:documentation "The element of ITEMS specified by INDEX."))

(defmethod elt ((items nelist) index)
  "Yields NIL if the index is out of range."
  (if (zerop index)
      (nelist-head items)
      (cl:elt (nelist-tail items) (1- index))))

#+nil
(elt (nel 1 2 3) 2)
#+nil
(elt (nel 1 2 3) 20)
