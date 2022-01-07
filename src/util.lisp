(defpackage tkutil
  (:use :cl)
  (:export #:split-mail-address
           #:string-to-keyword
           #:keyword-to-string
           #:to-integer
           #:blankp
           #:trim
           #:ensure-value
           #:ensure-string
           #:ensure-keyword
           #:ensure-list
           #:set-slot
           #:set-slot-if
           #:set-slot-if-not
           #:ignore-errors-log
           ))
(in-package :tkutil)

(defun split-mail-address (string)
  (ppcre:split "[,:;]+\\s*" string))

(defun string-to-keyword (string &key valid-keywords)
  (cond ((not string) nil)
        ((string= string "") nil)
        (t (let ((keyword (intern (string-upcase string) :keyword)))
             (when (or (not valid-keywords)
                       (find keyword valid-keywords))
               keyword)))))

(defun keyword-to-string (keyword &key downcase)
  (when keyword
    (let ((string (symbol-name keyword)))
      (if downcase
          (string-downcase string)
          string))))

(defun to-integer (string &key default)
  (or (and string (parse-integer string :junk-allowed t))
      default))

(defun blankp (value)
  (cond ((not value))
        ((and (stringp value)
              (ppcre:scan "^\\s*$" value)))))

(defun trim (string)
  (when (stringp string)
    (string-trim " \t\n\r" string)))

(defun ensure-value (value &rest args)
  (if (functionp value)
      (apply value args)
      value))

(defun ensure-string (string)
  (if (blankp string)
      string
      (princ-to-string string)))

(defun ensure-keyword (value)
  (string-to-keyword (ensure-string value)))

(defun ensure-list (value)
  (if (listp value)
      value
      (list value)))


(defmacro set-slot (object accessor value &key test test-not force-update)
  (let ((gvalue (gensym))
        (gtest (gensym))
        (gtest-not (gensym)))
    `(let ((,gvalue ,value)
           (,gtest ,test)
           (,gtest-not ,test-not))
       (when (cond (,force-update)
                   ((functionp ,gtest) (funcall ,gtest ,gvalue))
                   ((functionp ,gtest-not) (not (funcall ,gtest-not ,gvalue)))
                   (t ,gvalue))
         (setf (,accessor ,object) ,gvalue)))))

(defmacro set-slot-if (test object accessor value)
  `(set-slot ,object ,accessor ,value :test ,test))

(defmacro set-slot-if-not (test-not object accessor value)
  `(set-slot ,object ,accessor ,value :test-not ,test-not))


(defmacro ignore-errors-log (&rest forms)
  "Same as the standard `ignore-errors', but logs the condition that was caught."
  `(handler-case (progn . ,forms)
     (error (condition)
       (log:warn "Error occurred but ignored: ~A: ~A"
                 (type-of condition) condition)
       (values nil condition))))
