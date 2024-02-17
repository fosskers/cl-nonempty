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

(defgeneric to-list (items)
  (:documentation "Convert this non-empty collection into a normal list."))

(defmethod to-list ((items nelist))
  (cl:cons (nelist-head items)
           (nelist-tail items)))

#+nil
(to-list (nel 1 2 3))

(defgeneric last (items)
  (:documentation "The last element of the ITEMS. Guaranteed to exist."))

(defmethod last ((items nelist))
  (if (null (nelist-tail items))
      (nelist-head items)
      (cl:car (cl:last (nelist-tail items)))))

#+nil
(last (nel 1 2 3))

(defgeneric append (nonempty other)
  (:documentation "Append some OTHER collection to a NONEMPTY one."))

(defmethod append ((nonempty nelist) (other nelist))
  (make-nelist :head (nelist-head nonempty)
               :tail (cl:append (nelist-tail nonempty)
                                (to-list other))))

(defmethod append ((nonempty nelist) (other list))
  (make-nelist :head (nelist-head nonempty)
               :tail (cl:append (nelist-tail nonempty)
                                other)))

#+nil
(append (nel 1 2 3) (nel 4 5 6))
#+nil
(append (nel 1 2 3) '(4 5 6))
