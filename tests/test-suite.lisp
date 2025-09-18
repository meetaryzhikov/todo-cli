(defpackage :todo-cli.tests
  (:use :cl :todo-cli.models :todo-cli.storage :todo-cli.utils)
  (:export #:run-tests))

(in-package :todo-cli.tests)

(defun test-todo-creation ()
  "Task creation test"
  (let ((todo (make-todo :id 1 :title "Test Task")))
    (assert (= (todo-id todo) 1))
    (assert (string= (todo-title todo) "Test Task"))
    (assert (not (todo-completed-p todo)))
    (format t "✓ Task creation test passed~%")))

(defun test-todo-completion ()
  "Task completion test"
  (let ((todo (make-todo :id 1 :title "Test Task")))
    (complete-todo todo)
    (assert (todo-completed-p todo))
    (assert (todo-completed-at todo))
    (format t "✓ Task completion test passed~%")))

(defun run-tests ()
  "Run all tests"
  (format t "Running tests...~%~%")
  (test-todo-creation)
  (test-todo-completion)
  (format t "~%All tests passed!~%"))