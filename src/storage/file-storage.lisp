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