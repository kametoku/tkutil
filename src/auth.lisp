(in-package :cl-user)
(defpackage tkutil.auth
  (:use :cl)
  (:export #:current-user
           #:with-current-user
           #:with-admin-user
           #:%admin-user-p
           #:admin-user-p
           #:check-admin-user
           #:find-user
           #:friendly-user-name
           #:auth-user
           #:check-user-availability
           #:bad-password
           #:check-password))
(in-package :tkutil.auth)

(defparameter *password-min-length* 8)

(defvar *current-user* nil)

(defun current-user ()
  *current-user*)

(defmacro with-current-user (user &body body)
  "Execute the forms in BODY with USER."
  `(let ((*current-user* ,user))
     (progn ,@body)))

(defmacro with-admin-user (&body body)
  "Execute the forms in BODY with an admin user."
  `(with-current-user t ,@body))

(define-condition not-admin-user (error)
  ((user :initarg :user :reader user))
  (:report
   (lambda (condition stream)
     (format stream "Not an administrator: ~A."
             (user condition)))))

(defgeneric %admin-user-p (user)
  (:documentation "Return non-nil if USER is an administrator.")
  (:method ((user (eql nil))) (error "no login"))
  (:method ((user (eql t)))   t))

(defun admin-user-p (&optional (user *current-user*))
  (%admin-user-p user))

(defun check-admin-user (&optional (user *current-user*))
  (unless (admin-user-p user)
    (error 'not-admin-user :user user)))

(defgeneric find-user (user))

(defgeneric friendly-user-name (user))

(defgeneric auth-user (user password))

(defgeneric check-user-availability (user))

(define-condition bad-password (tkutil.exception:exception)
  ())

(defun check-password (password password-confirm &key blank-allowed)
  (cond ((and blank-allowed
              (tkutil:blankp password)
              (tkutil:blankp password-confirm))
         nil)
        ((not (string= password password-confirm))
         (error 'bad-password :message "Passwords not matched."))
        ((< (length password) *password-min-length*)
         (error 'bad-password
                :message (format nil "Password must be at least ~A characters."
                                 *password-min-length*)))
        (t password)))
