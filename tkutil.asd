(defsystem "tkutil"
  :version "0.1.0"
  :author "Tokuya Kameshima"
  :license "LLGPL"
  :depends-on ("alexandria"
               "cl-ppcre"
               "cl-reexport"
               "log4cl")
  :components ((:module "src"
                :components ((:file "util")
                             (:file "exception")
                             (:file "auth"))))
  :description "Tiny Common Lisp Utilities."
  :in-order-to ((test-op (test-op "tkutil/tests"))))

(defsystem "tkutil/tests"
  :author ""
  :license ""
  :depends-on ("tkutil"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "tkutil"))))
  :description "Test system for tkutil"
  :perform (test-op (op c) (symbol-call :rove :run c)))
