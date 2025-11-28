(in-package :todo-cli.cli)

(defparameter *default-storage-path*
  (merge-pathnames ".todos.lisp" (user-homedir-pathname)))

(defun execute-command (command storage)
  "Execute command"
  (case (command-name command)
    (:help (help-command))
    (:add (add-command (command-args command) storage))
    (:list (list-command (command-args command) storage))
    (:complete (complete-command (command-args command) storage))
    (:remove (remove-command (command-args command) storage))
    (:unknown (format t "Unknown command. Use 'help' for help.~%"))))

(defun help-command ()
  "Show help"
  (format t "ToDo CLI - Simple Task Management App~%~%")
  (format t "Usage:~%")
  (format t " todo add <title> [description] [priority]  - Add new task (priority: high, medium, low)~%")
  (format t " todo list [--all]               - Show task list~%")
  (format t " todo complete <id>              - Mark task as completed~%")
  (format t " todo remove <id>                - Delete task~%")
  (format t " todo help                       - Show this help~%~%"))

(defun add-command (args storage)
  "Add new task"
  (cond
    ((null args)
      (format t "Error: Task name not specified~%"))
    (t
      (let* ((title (first args))
             (description (if (rest args) (second args) ""))
             (priority-str (if (cddr args) (third args) "medium"))
             (priority (intern (string-upcase priority-str) :keyword)))
        (let ((todo (add-todo storage title description priority)))
          (format t "Added task #~d: ~a~%"
            (todo-id todo) (todo-title todo)))))))

(defun list-command (args storage)
  "Show tasks list"
  (let ((show-all (member "--all" args :test #'string=))
        (todos (list-todos storage :show-completed
                  (member "--all" args :test #'string=))))
    (if todos
      (progn
        (format t "~%Tasks list:~%")
        (format t "~{~a~%~}"
          (mapcar #'format-todo todos)))
      (format t "No tasks to display~%"))))

(defun complete-command (args storage)
  "Mark task as completed"
  (cond
    ((null args)
      (format t "Error: Task ID not specified~%"))
    (t
      (let ((id (parse-integer-safe (first args))))
        (if id
          (let ((todo (find-todo-by-id storage id)))
            (if todo
              (progn
                (complete-todo todo)
                (save-todos storage)
                (format t "Task #~d marked as completed~%" id))
              (format t "Task with ID ~d not found~%" id)))
          (format t "Error: Invalid task ID~%"))))))

(defun remove-command (args storage)
  "Remove task"
  (cond
    ((null args)
      (format t "Error: Task ID not specified~%"))
    (t
      (let ((id (parse-integer-safe (first args))))
        (if id
          (let ((todo (find-todo-by-id storage id)))
            (if todo
              (progn
                (remove-todo storage id)
                (format t "Task #~d removed~%" id))
              (format t "Task with ID ~d not found~%" id)))
          (format t "Error: Invalid task ID~%"))))))

(defun format-todo (todo)
  "Format the task for output"
  (format nil "[~a] #~d ~a~a~a [~a]"
    (if (todo-completed-p todo) "✓" " ")
    (todo-id todo)
    (todo-title todo)
    (if (not (string-empty-p (todo-description todo)))
        (format nil " - ~a" (todo-description todo))
        "")
    (format nil " (~a)" (format-date (todo-created-at todo)))
    (string-downcase (symbol-name (todo-priority todo)))))