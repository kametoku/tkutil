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
           #:setf-init
           #:setf-init-string
           #:setf-pred
           #:ignore-errors-log
           #:with-hash-table
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


(defmacro setf-init (place value &key initial-value test test-not)
  "Change the value of PLACE to VALUE
when the value of PLACE is INITIAL-VALUE and VALUE is not nil
or VALUE satisfies TEST or NOT-TEST."
  (let ((gvalue (gensym))
        (ginitial-value (gensym))
        (gtest (gensym))
        (gtest-not (gensym)))
    `(let ((,gvalue ,value)
           (,ginitial-value ,initial-value)
           (,gtest ,test)
           (,gtest-not ,test-not))
;;        (when (and (equal ,place ,initial-value)
       (when (and (cond ((functionp ,ginitial-value)
                         (funcall ,ginitial-value ,place))
                        ((equal ,place ,initial-value)))
                  (cond ((functionp ,gtest)
                         (funcall ,gtest ,gvalue))
                        ((functionp ,gtest-not)
                         (not (funcall ,gtest-not ,gvalue)))
                        (t ,gvalue)))
         (setf ,place ,gvalue)))))

(defmacro setf-init-string (place value)
  `(setf-init ,place ,value :initial-value #'blankp :test-not #'blankp))

(defmacro setf-pred (place value &key predicate predicate-not)
  "Change the value of PLACE to VALUE
when VALUE is non nil and the value of PLACE is nil or is not equal to VALUE.

If PREDICATE or PREDICATE-NOT is specified,
the value is changed per the result of PREDICATE or PREDICATE-NOT.
The first arguments to PREDICATE or PREDICATE-NOT is PLACE
and the second one is VALUE.
The value of PLACE is changed when PREDICATE returns non-nil or
PREDICATE-NOT returns nil."
  (let ((gvalue (gensym))
        (gpredicate (gensym))
        (gpredicate-not (gensym))
        (gplace-value (gensym)))
    `(let ((,gvalue ,value)
           (,gpredicate ,predicate)
           (,gpredicate-not ,predicate-not))
       (when ,gvalue
         (let ((,gplace-value ,place))
           (when (cond ((not ,gplace-value))
                       ((or ,gpredicate ,gpredicate-not)
                        (or (and ,gpredicate
                                 (funcall ,gpredicate ,gplace-value ,gvalue))
                            (and ,gpredicate-not
                                 (not (funcall ,gpredicate-not ,gplace-value ,gvalue)))))
                       (t (not (equal ,gplace-value ,gvalue))))
             (setf ,place ,gvalue)))))))

(defmacro ignore-errors-log (&rest forms)
  "Same as the standard `ignore-errors', but logs the condition that was caught."
  `(handler-case (progn . ,forms)
     (error (condition)
       (log:warn "Error occurred but ignored: ~A: ~A"
                 (type-of condition) condition)
       (values nil condition))))

(defmacro with-hash-table (hash-table key &body body)
  "Find the entry in HASH-TABLE whose key is KEY and return the asociated
value and T as multiple values.
If there is no such entry, BODY is executed, the result is added to HASH-TABLE,
and the result is returned.
If HASH-TABLE is nil, BODY is always executed and the result is returned."
  (let ((gkey (gensym))
        (gvalue (gensym)))
    `(if ,hash-table
         (let* ((,gkey ,key)
                (,gvalue (gethash ,gkey ,hash-table)))
           (if ,gvalue
               (values ,gvalue t)
               (setf (gethash ,gkey ,hash-table) (progn ,@body))))
         (progn ,@body))))
