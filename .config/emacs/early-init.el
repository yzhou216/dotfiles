;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; The Early Init File

;;; Code:

;; hide uneeded UI elements
(menu-bar-mode -1)
(when
    (and
     (fboundp 'tool-bar-mode)
     (fboundp 'scroll-bar-mode))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(setq-default
 use-dialog-box nil                            ; disable dialog box
 visible-bell t)

(setopt
 native-comp-async-report-warnings-errors nil  ; hide native comp warnings
 inhibit-startup-message t)                    ; inhibit startup message

;;; early-init.el ends here
