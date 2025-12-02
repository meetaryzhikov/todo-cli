(in-package :todo-cli.cli)

(defstruct command
  "Structure for representing a command"
  name args options)

(defun parse-args (args)
  "Parse command line arguments"
  (cond
    ((null args)
      (make-command :name :help))
    ((string= (first args) "help")
      (make-command :name :help))
    ((string= (first args) "add")
      (make-command :name :add :args (rest args)))
    ((string= (first args) "list")
      (make-command :name :list :args (rest args)))
    ((string= (first args) "complete")
      (make-command :name :complete :args (rest args)))
    ((string= (first args) "remove")
      (make-command :name :remove :args (rest args)))
     ((string= (first args) "reminders")
       (make-command :name :reminders))
     ((string= (first args) "edit")
       (make-command :name :edit :args (rest args)))
      ((string= (first args) "search")
        (make-command :name :search :args (rest args)))
      (t
        (make-command :name :unknown :args args))))