;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; The Early Init File

;;; Code:

;; hide uneeded UI elements
(menu-bar-mode -1)
(dolist (func '(tool-bar-mode scroll-bar-mode))    ; doesn't exist in TTY frames
  (when (fboundp func)
    (funcall func -1)))

(setq-default
 use-dialog-box nil                                ; disable dialog box
 visible-bell t)

(setopt
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent  ; disable native comp warning pop-up
 inhibit-startup-message t)                        ; inhibit startup message

;;; early-init.el ends here
