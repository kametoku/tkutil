(defpackage tkutil.csv
  (:use :cl)
  (:import-from #:cl-csv
                #:format-csv-value)
  (:export #:csv-fields
           #:csv-fields-for-read
           #:csv-fields-for-write
           ;; reader
           #:csv-value-parser
           #:parse-csv-value
           #:field-name-not-matched
           #:field-count-not-matched
           #:read-csv
           ;; writer
           #:write-csv
           ;; re-export from cl-csv
           #:format-csv-value))
(in-package :tkutil.csv)

(defgeneric csv-fields (class)
  (:documentation "Return default CSV field list of CLASS
as a list of (FIELD-NAME :KEY READER-OR-VALUE)."))

(defgeneric csv-fields-for-read (class)
  (:documentation "Return CSV field list of CLASS for reading
as a list of (FIELD-NAME :KEY READER-OR-VALUE).")
  (:method (class)
    (csv-fields class)))

(defgeneric csv-fields-for-write (class)
  (:documentation "Return CSV field list of CLASS for writing
as a list of (FIELD-NAME :KEY READER-OR-VALUE).")
  (:method (class)
    (csv-fields class)))

;;; Reading CSV

(defgeneric csv-value-parser (class field)
  (:method (class field)
    #'identity))

(defgeneric parse-csv-value (class field value)
  (:method (class field value)
    (let ((parser (csv-value-parser class field)))
      (funcall parser value))))

(define-condition field-not-matched (error)
  ((what :initarg :what :initform nil :reader what)
   (expected :initarg :expected :initform nil :reader expected)
   (actual :initarg :actual :initform nil :reader actual))
  (:report (lambda (condition stream)
             (format stream "Field ~A not matched. Expected ~S, but ~S."
                     (what condition)
                     (expected condition)
                     (actual condition)))))

(define-condition field-name-not-matched (field-not-matched)
  ()
  (:default-initargs :what "name"))

(define-condition field-count-not-matched (field-not-matched)
  ()
  (:default-initargs :what "count"))

(defun validate-field-name (header-row fields)
  (let ((field-count (length header-row))
        (expected-count (length fields)))
    (unless (= field-count expected-count)
      (error 'field-count-not-matched
             :expected expected-count :actual field-count)))
  (loop for field-name in header-row
        for (expected-field-name key reader-or-value) in fields
        unless (string-equal field-name expected-field-name)
          do (error 'field-name-not-matched
                    :expected expected-field-name :actual field-name)))

(defun parse-row (csv-row class fields)
  (loop for value in csv-row
        for (field-name key reader-or-value) in fields
        for parser = (csv-value-parser class key)
        when key
          nconc (list key (funcall parser value))))

(defun make-csv-reader (class fields make-instance-fn skip-first-p)
  (let ((class class)
        (fields fields)
        (make-instance-fn make-instance-fn)
        (skip-first-p skip-first-p))
    (lambda (row)
      (when (eql skip-first-p :validate-field-name)
        (validate-field-name row fields))
      (if skip-first-p
          (setf skip-first-p nil)
          (let ((args (parse-row row class fields)))
            (if class
                (apply make-instance-fn class args)
                args))))))

(defun read-csv (csv-file &key class (skip-first-p :validate-field-name)
                            (external-format :utf-8)
                            (fields #'csv-fields-for-read)
                            (make-instance-fn #'make-instance))
  (let* ((fields (if (functionp fields) (funcall fields class) fields))
         (csv-reader (make-csv-reader class fields make-instance-fn skip-first-p))
         (cl-csv:*default-external-format* external-format))
    (cl-csv:read-csv csv-file :map-fn csv-reader :skip-first-p skip-first-p)))


;;; Writing CSV

(defun header-row (fields)
  "Returns the field name of FIELDS as a list."
  (loop for (field-name key reader-or-value) in fields
        collect field-name))

(defun object-row (object fields)
  (loop for (field-name key reader-or-value) in fields
        if (functionp reader-or-value)
          collect (funcall reader-or-value object)
        else
          collect reader-or-value))

(defun build-rows (objects fields &key write-header-p)
  (loop for object in (nconc (when write-header-p (header-row fields))
                             objects)
        if (listp object)
          collect object
        else
          collect (object-row object fields)))

(defun write-csv (stream &key objects (fields #'csv-fields-for-write)
                           write-header-p always-quote)
  (when objects
    (let* ((fields (if (functionp fields)
                       (funcall fields (type-of (car objects)))
                       fields))
           (rows (build-rows objects fields :write-header-p write-header-p)))
      (cl-csv:write-csv rows :stream stream :always-quote always-quote))))
