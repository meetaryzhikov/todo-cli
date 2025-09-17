(in-package :todo-cli)

(defun main (&optional args)
  "The main function of the application"
  (let* ((storage (make-file-storage
                    (merge-pathnames ".todos.lisp"
                      (user-homedir-pathname))))
         (command (parse-args (or args
                                  (rest sb-ext:*posix-argv*)))))
    (execute-command command storage)))
