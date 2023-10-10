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

;; update buffers when files on the disk changes
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; fido-vertical-mdoe (icomplete-mode with flex match)
(fido-vertical-mode 1)
(setq completions-detailed t)

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
  (evil-set-leader 'normal "\\" t))

;; git-gutter
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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

;; flycheck
(use-package flycheck)

;; dap-mode
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)

  ;; Evil local leader macros
  (evil-define-key 'normal 'global (kbd "<localleader>b") 'dap-breakpoint-toggle)
  (evil-define-key 'normal 'global (kbd "<localleader>d") 'dap-breakpoint-delete-all)
  (evil-define-key 'normal 'global (kbd "<localleader>n") 'dap-next)
  (evil-define-key 'normal 'global (kbd "<localleader>s") 'dap-step-in)
  (evil-define-key 'normal 'global (kbd "<localleader>c") 'dap-continue)
  (evil-define-key 'normal 'global (kbd "<localleader>q") 'dap-disconnect))

;; dap-java
(use-package dap-java
  :ensure nil)
