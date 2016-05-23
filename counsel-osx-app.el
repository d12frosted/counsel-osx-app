;;; counsel-osx-app.el --- launch osx applications via ivy interface
;;
;; Copyright (c) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.1.0
;; Package-Requires: ((ivy "0.8.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; This package provides `counsel-osx-app' function which is inspired by
;; `counsel-linux-app'.
;;
;; In order to use `counsel-osx-app' simply call `counsel-osx-app' function.  It
;; will allow you to select an app to launch using ivy completion.  Optionally
;; one can select any file to edit in selected application via ivy actions.
;;
;; By default `counsel-osx-app' searches for applications in "/Applications"
;; directory, but it's configurable via `counsel-apps-location' variable.  It can
;; be either string representing root location for all applications or list of
;; such strings.
;;
;; The last configurable thing (but not least) is command for launching
;; application.  Please refer to `counsel-app-launch-cmd' for more information.
;;
;; Although the name of this package is `counsel-osx-app', it's not restricted
;; to OSX only.  One can easily tune it to run under Linux (not sure about
;; Windows).  Just make sure to configure described variables and change
;; implementation of `counsel-apps-list' function.  PRs are welcome on making
;; this package cross-platform.
;;
;;; Code:
;;

(require 'ivy)

(defvar counsel-apps-location "/Applications"
  "Path to directory containing applications.")

(defvar counsel-apps-pattern "*.app"
  "Pattern for applications in `counsel-apps-location'.

Use \"*\" if all files in `counsel-apps-location' are considered
applications.")

(defvar counsel-app-launch-cmd (lambda (app &optional file)
                                     (if (bound-and-true-p file)
                                         (format "open '%s' -a '%s'" file app)
                                       (format "open '%s'" app)))
  "Command for launching application.

Can be either format string or function that accepts path to
application as first argument and filename as optional second
argument and returns command.")

(defun counsel-apps-list ()
  "Get the list of applications under `counsel-apps-location'."
  (let* ((locs (if (stringp counsel-apps-location)
                   `(,counsel-apps-location)
                 counsel-apps-location))
         (files (mapcar (lambda (path)
                          (file-expand-wildcards
                           (concat path "/" counsel-apps-pattern)))
                        locs)))
    (apply #'append files)))

(defun counsel-app-action-default (app)
  "Launch APP using `counsel-app-launch-cmd'."
  (call-process-shell-command
   (cond
    ((stringp counsel-app-launch-cmd)
     (format "%s '%s'" counsel-app-launch-cmd app))
    ((functionp counsel-app-launch-cmd)
     (funcall counsel-app-launch-cmd app))
    (t
     (user-error "Could not construct cmd from `counsel-app-launch-cmd'")))))

(defun counsel-app-action-file (app)
  "Open file in APP using `counsel-app-launch-cmd'."
  (let* ((short-name (file-name-nondirectory app))
         (file (and short-name
                    (read-file-name
                     (format "Run %s on: " short-name)))))
    (if file
        (call-process-shell-command
         (cond
          ((stringp counsel-app-launch-cmd)
           (format "%s '%s' '%s'" counsel-app-launch-cmd app file))
          ((functionp counsel-app-launch-cmd)
           (funcall counsel-app-launch-cmd app file))
          (t
           (user-error "Could not construct cmd from `counsel-app-launch-cmd'"))))
      (user-error "Cancelled"))))

(ivy-set-actions
 'counsel-app
 '(("f" counsel-app-action-file "run on a file")))

;;;###autoload
(defun counsel-osx-app ()
  "Launch an application via ivy interface."
  (interactive)
  (ivy-read "Run application: " (counsel-apps-list)
            :action #'counsel-app-action-default
            :caller 'counsel-app))

(provide 'counsel-osx-app)
;;; counsel-osx-app.el ends here
