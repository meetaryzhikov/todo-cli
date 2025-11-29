(in-package :todo-cli.models)

(defstruct todo
  "Structure for representing a TODO task"
  (id nil :type (or null integer))
  (title "" :type string)
  (description "" :type string)
  (completed-p nil :type boolean)
  (priority :medium :type keyword)
  (created-at nil :type (or null integer))
  (completed-at nil :type (or null integer))
  (due-date nil :type (or null integer)))

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
        :priority (todo-priority todo)
        :created-at (todo-created-at todo)
        :completed-at (todo-completed-at todo)
        :due-date (todo-due-date todo)))

(defun plist-to-todo (plist)
  "Create task from plist"
  (make-todo :id (getf plist :id)
             :title (getf plist :title)
             :description (getf plist :description)
             :completed-p (getf plist :completed-p)
             :priority (getf plist :priority :medium)
             :created-at (getf plist :created-at)
             :completed-at (getf plist :completed-at)
             :due-date (getf plist :due-date)))