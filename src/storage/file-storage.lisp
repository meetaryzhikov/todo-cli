(in-package :todo-cli.storage)

(defclass storage ()
  ()
  (:documentation "Base class for storage"))

(defclass file-storage (storage)
  ((file-path :initarg :file-path
              :accessor storage-file-path
              :type string
              :documentation "Path to data file")
    (todos :initform '()
           :accessor storage-todos
           :type list
           :documentation "Task list in memory"))
  (:documentation "Task storage in a file"))

(defun make-file-storage (file-path)
  "Create file storage"
  (let ((storage (make-instance 'file-storage :file-path file-path)))
    (load-todos storage)
    storage))

(defmethod load-todos ((storage file-storage))
  "Load tasks from file"
  (let ((file-path (storage-file-path storage)))
    (when (probe-file file-path)
      (with-open-file (stream file-path :direction :input)
        (let ((data (read stream nil nil)))
          (when data
            (setf (storage-todos storage)
              (mapcar #'plist-to-todo data))))))
    storage))

(defmethod save-todos ((storage file-storage))
  "Save tasks to file"
  (with-open-file (stream (storage-file-path storage)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (prin1 (mapcar #'todo-to-plist (storage-todos storage)) stream))
  storage)

(defmethod add-todo ((storage file-storage) title description)
  "Add new task"
  (let* ((todos (storage-todos storage))
         (next-id (if todos (1+ (reduce #'max todos :key #'todo-id)) 1))
         (todo (make-todo :id next-id
                          :title title
                          :description description
                          :created-at (current-timestamp))))
    (push todo (storage-todos storage))
    (save-todos storage)
    todo))

(defmethod remove-todo ((storage file-storage) id)
  "Delete task by ID"
  (setf (storage-todos storage)
        (remove-if (lambda (todo) (= (todo-id todo) id))
                   (storage-todos storage)))
  (save-todos storage))

(defmethod find-todo-by-id ((storage file-storage) id)
  "Find task by ID"
  (find-if (lambda (todo) (= (todo-id todo) id))
           (storage-todos storage)))

(defmethod list-todos ((storage file-storage) &key show-completed)
  "Get a list of tasks"
  (let ((todos (storage-todos storage)))
    (if show-completed
      todos
      (remove-if #'todo-completed-p todos))))
