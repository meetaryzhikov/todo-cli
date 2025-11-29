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

(defun parse-date (date-str)
  "Parse date string in YYYY-MM-DD format to universal time"
  (when (and date-str (not (string-empty-p date-str)))
    (handler-case
      (let* ((parts (split-sequence:split-sequence #\- date-str))
             (year (parse-integer (first parts)))
             (month (parse-integer (second parts)))
             (day (parse-integer (third parts))))
        (encode-universal-time 0 0 0 day month year))
      (error () nil))))