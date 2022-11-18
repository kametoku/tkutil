(defpackage tkutil.config
  (:use :cl)
  (:export :getenv
           :config-package-name
           :config*
           :config
           :egetf
           :config-not-found))
(in-package :tkutil.config)

(defun getenv (name &key default)
  (or (remove #\Return (uiop:getenv name)) default))

(defvar *config-package-name* nil)

(defun (setf config-package-name) (package-name)
  (setf *config-package-name* package-name))

(define-condition config-not-found (error)
  ((key :initarg :key :reader key))
  (:report (lambda (condition stream)
             (format stream "Configuration for ~S not found or set to nil."
                     (key condition)))))

(defun throw-not-found-error (key)
  (error 'config-not-found :key key))

(defun config* (key &key (default #'throw-not-found-error))
  (cond ((envy:config *config-package-name* key))
        ((functionp default) (funcall default key))
        (t default)))

(defun config (key &key default)
  (config* key :default default))

(defun egetf (plist indicator &key (default #'throw-not-found-error))
  (cond ((getf plist indicator))
        ((functionp default) (funcall default indicator))
        (t default)))
