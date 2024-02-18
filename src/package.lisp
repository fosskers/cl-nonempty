(defpackage nonempty
  (:use :cl)
  (:shadow #:cons #:car #:cdr #:last
           #:map #:append
           #:length #:reverse #:elt)
  ;; --- Lists --- ;;
  (:export #:nelist #:nel
           #:cons #:car #:cdr
           #:map #:filter #:fold)
  ;; --- Generics --- ;;
  (:export #:length #:reverse #:elt #:last #:append
           #:to-list)
  (:documentation "Non-empty collections.

Non-emptiness can be a powerful guarantee. There are often times when a
collection is passed to a function, but must contain some value to be useful. In
these cases we must manually check, or otherwise account for NIL in our logic.
This can muddle the clarity of the code, especially when empty is not a valid
state. It is here that non-empty variants of the usual collection types can be
used to great effect.

See `nel', the main way to construct a non-empty list."))

(in-package :nonempty)

