(in-package :cl-user)
(defpackage :tkutil.ftp
  (:use :cl)
  (:export #:mirror))
(in-package :tkutil.ftp)

(defun egetf (plist indicator &key (default 'throw-error))
  (let ((value (getf plist indicator default)))
    (when (eql value 'throw-error)
      (error "not found: indicator = ~A" indicator))
    value))

(defun mirror (&key ftp-config remote-directory local-directory)
  (let ((hostname (egetf ftp-config :hostname)))
    (ensure-directories-exist local-directory)
    (ftp:with-ftp-connection
        (conn :hostname hostname
              :username (egetf ftp-config :username)
              :password (egetf ftp-config :password)
              :passive-ftp-p (getf ftp-config :passive-ftp-p))
      (log:info "Connected to" hostname)
      (ftp:send-cwd-command conn remote-directory)
      (log:debug "Retrieving file list in" remote-directory)
      (let ((files (ftp:retrieve-file-info-list conn)))
        (log:debug "Retrieved file list in" remote-directory)
        (loop for (type name) in files
              for local-file = (merge-pathnames name local-directory)
;;               do (log:debug type name local-file)
              if (and (eql type :file)
                      (not (uiop:file-exists-p local-file)))
                collect (progn
                          (log:info "Downloading ~S to ~S ..." name local-file)
                          (ftp:retrieve-file conn name local-file)
                          (log:info "Downloading ~S to ~S ...done" name local-file)
                          local-file)
              else
                do (log:debug "Download skipped" name))))))
