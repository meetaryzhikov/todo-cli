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
     (:edit (edit-command (command-args command) storage))
      (:search (search-command (command-args command) storage))
      (:reminders (reminders-command storage))
      (:unknown (format t "Unknown command. Use 'help' for help.~%"))))

(defun help-command ()
  "Show help"
  (format t "ToDo CLI - Simple Task Management App~%~%")
  (format t "Usage:~%")
  (format t " todo add <title> [description] [priority] [due-date]  - Add new task (priority: high, medium, low; due-date: YYYY-MM-DD)~%")
  (format t " todo list [--all] [--sort-by <field>] [--sort-order <asc|desc>] - Show task list (fields: creation-date, priority, due-date)~%")
   (format t " todo complete <id>              - Mark task as completed~%")
   (format t " todo remove <id>                - Delete task~%")
  (format t " todo edit <id> <title> [desc] [pri] [due] - Edit task~%")
    (format t " todo search <key> <value> ...   - Search tasks by title, status, date~%")
    (format t " todo reminders                  - Show overdue tasks~%")
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
             (priority (intern (string-upcase priority-str) :keyword))
              (due-date-str (if (cdddr args) (fourth args) nil))
              (due-date (if due-date-str (parse-date due-date-str) nil)))
        (let ((todo (add-todo storage title description priority due-date)))
          (format t "Added task #~d: ~a~%"
            (todo-id todo) (todo-title todo)))))))

(defun list-command (args storage)
  "Show tasks list"
  (let* ((show-all (member "--all" args :test #'string=))
         (sort-by-pos (position "--sort-by" args :test #'string=))
         (sort-by (if sort-by-pos (nth (1+ sort-by-pos) args) nil))
         (sort-order-pos (position "--sort-order" args :test #'string=))
         (sort-order (if sort-order-pos (nth (1+ sort-order-pos) args) "asc")))
    (when (and sort-by (not (member sort-by '("creation-date" "priority" "due-date") :test #'string=)))
      (format t "Invalid sort-by field: ~a. Valid: creation-date, priority, due-date~%" sort-by)
      (return-from list-command))
    (when (and sort-order (not (member sort-order '("asc" "desc") :test #'string=)))
      (format t "Invalid sort-order: ~a. Valid: asc, desc~%" sort-order)
      (return-from list-command))
    (let ((todos (list-todos storage :show-completed show-all)))
      (when sort-by
        (setf todos (sort-todos todos sort-by sort-order)))
      (if todos
        (progn
          (format t "~%Tasks list:~%")
          (format t "~{~a~%~}"
            (mapcar #'format-todo todos)))
        (format t "No tasks to display~%")))))

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

(defun edit-command (args storage)
  "Edit existing task"
  (cond
    ((null args)
      (format t "Error: Task ID not specified~%"))
    ((null (rest args))
      (format t "Error: Task title not specified~%"))
    (t
      (let ((id (parse-integer-safe (first args))))
        (if id
          (let ((todo (find-todo-by-id storage id)))
            (if todo
              (let* ((title (second args))
                     (description (if (cddr args) (third args) ""))
                     (priority-str (if (cdddr args) (fourth args) "medium"))
                     (priority (intern (string-upcase priority-str) :keyword))
                     (due-date-str (if (cddddr args) (fifth args) nil))
                     (due-date (if due-date-str (parse-date due-date-str) nil)))
                (update-todo storage id title description priority due-date)
                (format t "Task #~d updated~%" id))
              (format t "Task with ID ~d not found~%" id)))
          (format t "Error: Invalid task ID~%"))))))

(defun search-command (args storage)
  "Search and filter tasks"
  (let ((filters (parse-search-args args)))
    (let ((filtered-todos (filter-todos (list-todos storage :show-completed t) filters)))
      (if filtered-todos
        (progn
          (format t "~%Filtered tasks:~%")
          (format t "~{~a~%~}"
            (mapcar #'format-todo filtered-todos)))
        (format t "No tasks match the filters~%")))))

(defun parse-search-args (args)
  "Parse search arguments into filters plist"
  (let ((filters '()))
    (loop for (key value . rest) on args by #'cddr
          do (when value
               (push (cons (intern (string-upcase key) :keyword) value) filters)))
    filters))

(defun filter-todos (todos filters)
  "Filter todos based on filters"
  (remove-if-not
    (lambda (todo)
      (every (lambda (filter)
               (let ((key (car filter))
                     (value (cdr filter)))
                 (case key
                   (:title (or (search value (todo-title todo) :test #'string-equal)
                               (search value (todo-description todo) :test #'string-equal)))
                   (:status (cond ((string-equal value "completed") (todo-completed-p todo))
                                  ((string-equal value "pending") (not (todo-completed-p todo)))
                                  (t t)))
                   (:date (let ((due-date (todo-due-date todo)))
                            (if due-date
                                (string= value (format-date due-date))
                                nil)))
                   (t t))))
             filters))
    todos))

(defun reminders-command (storage)
  "Show overdue tasks"
  (let ((overdue-todos (remove-if-not
                        (lambda (todo)
                          (and (todo-due-date todo)
                               (< (todo-due-date todo) (current-timestamp))
                               (not (todo-completed-p todo))))
                        (list-todos storage :show-completed t))))
    (if overdue-todos
      (progn
        (format t "~%Overdue tasks:~%")
        (format t "~{~a~%~}"
          (mapcar #'format-todo overdue-todos)))
      (format t "No overdue tasks~%"))))

(defun priority-value (priority)
  "Get numeric value for priority sorting"
  (case priority
    (:high 3)
    (:medium 2)
    (:low 1)
    (t 2)))

(defun priority-key (todo)
  (priority-value (todo-priority todo)))

(defun due-date-key (todo)
  (or (todo-due-date todo) most-positive-fixnum))

(defun sort-todos (todos sort-by sort-order)
  "Sort todos by field and order"
  (let ((key-fn (cond ((string= sort-by "creation-date") #'todo-created-at)
                      ((string= sort-by "priority") #'priority-key)
                      ((string= sort-by "due-date") #'due-date-key)
                      (t nil)))
        (test (if (string= sort-order "asc") #'< #'>)))
    (sort todos test :key key-fn)))

(defun format-todo (todo)
  "Format the task for output"
  (format nil "[~a] #~d ~a~a~a [~a]~a"
    (if (todo-completed-p todo) "✓" " ")
    (todo-id todo)
    (todo-title todo)
    (if (not (string-empty-p (todo-description todo)))
        (format nil " - ~a" (todo-description todo))
        "")
    (format nil " (~a)" (format-date (todo-created-at todo)))
    (string-downcase (symbol-name (todo-priority todo)))
    (if (todo-due-date todo)
        (format nil " due: ~a" (format-date (todo-due-date todo)))
        "")))