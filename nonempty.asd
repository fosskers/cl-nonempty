(defsystem "nonempty"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "list")
                 (:file "generics"))))
  :description "Non-empty collections."
  :in-order-to ((test-op (test-op "nonempty/tests"))))

(defsystem "nonempty/tests"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:nonempty
               :nonempty/transducers
               :parachute
               :transducers)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for nonempty"
  :perform (test-op (op c) (symbol-call :parachute :test :nonempty/tests)))

(defsystem "nonempty/transducers"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:nonempty :transducers)
  :components ((:module "src"
                :components
                ((:file "transducers"))))
  :description "Transducers support for nonempty.")
