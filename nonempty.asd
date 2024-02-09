(defsystem "nonempty"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "nonempty"))))
  :description ""
  :in-order-to ((test-op (test-op "nonempty/tests"))))

(defsystem "nonempty/tests"
  :author ""
  :license ""
  :depends-on ("nonempty"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for nonempty"
  :perform (test-op (op c) (symbol-call :rove :run c)))
