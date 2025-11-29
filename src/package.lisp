(defpackage :todo-cli.utils
  (:use :cl)
  (:export #:current-timestamp
           #:format-date
           #:string-empty-p
           #:parse-integer-safe
           #:parse-date))

(defpackage :todo-cli.models
  (:use :cl :todo-cli.utils)
  (:export #:todo
           #:make-todo
           #:todo-id
           #:todo-title
           #:todo-description
           #:todo-completed-p
           #:todo-priority
           #:todo-created-at
           #:todo-completed-at
           #:todo-due-date
           #:complete-todo
           #:todo-to-plist
           #:plist-to-todo))

(defpackage :todo-cli.storage
  (:use :cl :todo-cli.models :todo-cli.utils)
   (:export #:storage
            #:file-storage
            #:make-file-storage
            #:load-todos
            #:save-todos
            #:add-todo
            #:update-todo
            #:remove-todo
            #:find-todo-by-id
            #:list-todos))

(defpackage :todo-cli.cli
  (:use :cl :todo-cli.models :todo-cli.storage :todo-cli.utils)
  (:export #:parse-args
           #:command
           #:execute-command
           #:help-command
           #:add-command
           #:list-command
           #:complete-command
           #:remove-command))

(defpackage :todo-cli
  (:use :cl :todo-cli.cli :todo-cli.storage)
  (:export #:main))