(in-package :todo-cli.models)

(defstruct todo
  "Structure for representing a TODO task"
  (id nil :type (or null integer))
  (title "" :type string)
  (description "" :type string)
  (completed-p nil :type boolean)
  (created-at nil :type (or null integer))
  (completed-at nil :type (or null integer)))

(defun complete-todo (todo)
  "Mark task as completed"
  (setf (todo-completed-p todo) t
        (todo-completed-at todo) (current-timestamp))
  todo)

(defun todo-to-plist (todo)
  "Convert task to plist for serialization"
  (list :id (todo-id todo)
        :title (todo-title todo)
        :description (todo-description todo)
        :completed-p (todo-completed-p todo)
        :created-at (todo-created-at todo)
        :completed-at (todo-completed-at todo)))

(defun plist-to-todo (plist)
  "Create task from plist"
  (make-todo :id (getf plist :id)
             :title (getf plist :title)
             :description (getf plist :description)
             :completed-p (getf plist :completed-p)
             :created-at (getf plist :created-at)
             :completed-at (getf plist :completed-at)))