(asdf:defsystem "todo-cli"
  :description "Simple command-line TODO application"
  :author "Aleksandr Ryzhikov <meet.aryzhikov@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:alexandria
               :cl-fad
               :split-sequence
               :local-time)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils/helpers" :depends-on ("package"))
                 (:file "models/todo" :depends-on ("package" "utils/helpers"))
                 (:file "storage/file-storage" :depends-on ("package" "models/todo"))
                 (:file "cli/parser" :depends-on ("package"))
                 (:file "cli/commands" :depends-on ("package" "models/todo" "storage/file-storage"))
                 (:file "main" :depends-on ("package" "cli/parser" "cli/commands")))))
  :build-operation "program-op"
  :build-pathname "bin/todo"
  :entry-point "todo-cli:main")