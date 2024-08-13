;; use-package
(require 'use-package)
(use-package use-package
  :ensure nil
  :custom use-package-always-ensure t)

;; package archives
(use-package package
  :ensure nil
  :config
  (package-initialize)
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  :custom
  (package-enable-at-startup nil)) ; Prevent initialization after early-init.el

;; no-littering (keeping ~/.config/emacs clean)
(use-package no-littering)

;; Emacs settings
(use-package emacs
  :after no-littering
  :config
  ;; hide uneeded UI elements
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  ;; disable dialog box
  (setq use-dialog-box nil)

  ;; inhibit startup message
  (setq inhibit-startup-message t
        visible-bell t)

  ;; hide native comp warnings
  (setq native-comp-async-report-warnings-errors nil))
