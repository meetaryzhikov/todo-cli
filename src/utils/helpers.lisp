(in-package :todo-cli.utils)

(defun current-timestamp ()
  "Get the current timestamp"
  (get-universal-time))

(defun format-date (timestamp)
  "Format timestamp to readable format"
  (if timestamp
    (multiple-value-bind (sec min hour date month year)
      (decode-universal-time timestamp)
        (declare (ignore sec min hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month date))
    "N/A"))

(defun string-empty-p (str)
  "Check if a string is empty"
  (or (null str) (string= str "")))

(defun parse-integer-safe (str)
  "Safely parse integer from string"
  (when (and str (not (string-empty-p str)))
    (handler-case
      (parse-integer str)
      (error () nil))))