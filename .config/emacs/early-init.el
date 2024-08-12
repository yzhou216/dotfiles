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
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; no-littering (keeping ~/.config/emacs clean)
(use-package no-littering)
