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
    (make-nelist :head (car revd) :tail (cdr revd))))

#+nil
(reverse (nel 1 2 3))
#+nil
(reverse (nel 1))
