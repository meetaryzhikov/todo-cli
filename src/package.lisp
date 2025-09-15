(defpackage :todo-cli.utils
  (:use :cl)
  (:export #:current-timestamp
           #:format-date
           #:string-empty-p
           #:parse-integer-safe))

(defpackage :todo-cli.models
  (:use :cl :todo-cli.utils)
  (:export #:todo
           #:make-todo
           #:todo-id
           #:todo-title
           #:todo-description
           #:todo-completed-p
           #:todo-created-at
           #:todo-completed-at
           #:complete-todo
           #:todo-to-plist
           #:plist-to-todo))