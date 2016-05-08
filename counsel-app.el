;;; counsel-app.el --- launch applications via ivy interface
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
;; This package provides `counsel-app' function which is inspired by
;; `counsel-linux-app' (and has some code from counsel package). It was tested
;; on OSX only, but it should also work on Linux as well (with some
;; configurations). Please refer to all variables defined in this package for
;; more information.
;;
;;; Code:
;;

(require 'ivy)

(defvar counsel-apps-location "/Applications"
  "Path to directory containing applications.")

(defvar counsel-apps-pattern "*.app"
  "Pattern for applications in `counsel-apps-location'. Use
  \"*\" if all files in `counsel-apps-location' are
  considered applications.")

(defvar counsel-app-launch-cmd (lambda (app &optional file)
                                     (if (bound-and-true-p file)
                                         (format "open '%s' -a '%s'" file app)
                                       (format "open '%s'" app)))
  "Command for launching application. Can be either string or
  function that accepts path to application as first argument and
  filename as optional second argument and returns command.")

(defvar counsel-apps-alist nil
  "List of data located in `counsel-apps-location'")

(defvar counsel-apps-faulty nil
  "List of faulty data located in `counsel-apps-location'.")

(defun counsel-app--unsupported-os-error ()
  (error "Your system is not supported."))

(defun counsel-apps-list--setup-osx (files)
  (setq counsel-apps-alist files))

(defun counsel-apps-list--setup-linux (files)
  (dolist (file (cl-set-difference files (append (mapcar 'car counsel-apps-alist)
                                                 counsel-apps-faulty)
                                   :test 'equal))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file counsel-apps-location))
      (let (name comment exec)
        (goto-char (point-min))
        (if (re-search-forward "^Name *= *\\(.*\\)$" nil t)
            (setq name (match-string 1))
          (error "File %s has no Name" file))
        (goto-char (point-min))
        (when (re-search-forward "^Comment *= *\\(.*\\)$" nil t)
          (setq comment (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "^Exec *= *\\(.*\\)$" nil t)
          (setq exec (match-string 1)))
        (if (and exec (not (equal exec "")))
            (add-to-list
             'counsel-apps-alist
             (cons (format "% -45s: %s%s"
                           (propertize exec 'face 'font-lock-builtin-face)
                           name
                           (if comment
                               (concat " - " comment)
                             ""))
                   file))
          (add-to-list 'counsel-apps-faulty file))))))

(defun counsel-apps-list ()
  (let ((files
         (delete
          ".." (delete
                "." (file-expand-wildcards (concat counsel-apps-location "/" counsel-apps-pattern))))))
    (cond ((eq system-type 'darwin)
           (counsel-apps-list--setup-osx files))
          ((eq system-type 'gnu/linux)
           (counsel-apps-list--setup-linux files))
          (t (counsel-app--unsupported-os-error))))
  counsel-apps-alist)

(defun counsel-app--extract (app)
  (cond ((eq system-type 'darwin) app)
        ((eq system-type 'gnu/linux) (file-name-nondirectory app))
        (t (counsel-app--unsupported-os-error))))

(defun counsel-app--short-name (app)
  (cond ((eq system-type 'darwin) app)
        ((eq system-type 'gnu/linux) (let ((entry (rassoc app counsel-apps-alist)))
                                       (and entry
                                            (string-match "\\([^ ]*\\) " (car entry))
                                            (match-string 1 (car entry)))))
        (t (counsel-app--unsupported-os-error))))

(defun counsel-app-action-default (app)
  "Launch APP using `counsel-app-launch-cmd'."
  (call-process-shell-command
   (cond ((stringp counsel-app-launch-cmd)
          (format "%s '%s'" counsel-app-launch-cmd (counsel-app--extract app)))
         ((functionp counsel-app-launch-cmd)
          (funcall counsel-app-launch-cmd (counsel-app--extract app)))
         (t (counsel-app--unsupported-os-error)))))

(defun counsel-app-action-file (app)
  "Open file in APP using `counsel-app-launch-cmd'."
  (let* ((short-name (counsel-app--short-name app))
         (file (and short-name
                    (read-file-name
                     (format "Run %s on: " short-name)))))
    (if file
        (call-process-shell-command
         (cond ((stringp counsel-app-launch-cmd)
                (format "%s '%s' '%s'" counsel-app-launch-cmd (counsel-app--extract app) file))
               ((functionp counsel-app-launch-cmd)
                (funcall counsel-app-launch-cmd (counsel-app--extract app) file))
               (t (counsel-app--unsupported-os-error))))
      (user-error "cancelled"))))

(ivy-set-actions
 'counsel-app
 '(("f" counsel-app-action-file "run on a file")))

;;;###autoload
(defun counsel-app ()
  "Launch an application via ivy interface."
  (interactive)
  (ivy-read "Run application: " (counsel-apps-list)
            :action #'counsel-app-action-default
            :caller 'counsel-app))

(provide 'counsel-app)
