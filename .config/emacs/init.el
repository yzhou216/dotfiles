;; inhibit startup message
(setq inhibit-startup-message t
      visible-bell t)

;; hide uneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; set gray line highlight
(global-hl-line-mode t) ; Enable hl-line-mode
(custom-set-faces ; Customize the hl-line face to be gray
 '(hl-line ((t (:background "gray20")))))

;; set non blinking cursor
(blink-cursor-mode 0)

;; line number
(global-display-line-numbers-mode 1)

;; set indentation for C
(setq-default c-default-style "linux"
	      indent-tabs-mode t)

;; set dark mode
(when (display-graphic-p)
  (invert-face 'default)
)
(set-variable 'frame-background-mode 'dark)

;; set path for customise system
(setq custom-file "~/.config/emacs/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.

;; use-package
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; ensure packages are automatically installed when using use-package
(setq use-package-always-ensure t)

;; Evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)

  ;; use Emacs bindings in insert mode
  (setq evil-disable-insert-state-bindings t)

  :config
  (evil-mode 1)

  ;; set space key as global leader
  (evil-set-leader 'normal " ")

  ;; set backslash as local leader
  (evil-set-leader 'normal "\\"))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((java-mode . lsp)
   ;; which-key integration
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; lsp-java
(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

;; company-mode
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; helm-mode
(use-package helm
  :config (helm-mode))
(use-package helm-lsp)

;; flycheck
(use-package flycheck)

;; dap-mode
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; dap-java
(use-package dap-java
  :ensure nil)
