;; inhibit startup message
(setq inhibit-startup-message t
      visible-bell t)

;; hide uneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; hide native comp warnings
(setq native-comp-async-report-warnings-errors nil)

;; enable line number
(global-display-line-numbers-mode 1)

;; relative line numbers
(display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; set non blinking cursor
(blink-cursor-mode 0)

;; enable hl-line-mode to highlight the current line
(global-hl-line-mode t)

;; customize the hl-line face to be gray
(custom-set-faces
 '(hl-line ((t (:background "gray20")))))

;; light on dark
(set-background-color "black")
(set-foreground-color "white")

;; set path for customise system
(setq custom-file "~/.config/emacs/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.

;; update buffers when files on the disk changes
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; ensure smooth scrolling
(setq scroll-conservatively 101)

;; set a margin of 10 lines at the top and bottom for scrolling
(setq scroll-margin 10)

;; fido-vertical-mdoe (icomplete-mode with flex match)
(fido-vertical-mode 1)
(setq completions-detailed t)

;; midnight.el
(require 'midnight)
(midnight-delay-set 'midnight-delay 16200) ; (eq (* 4.5 60 60) "4:30am")

;; set indentation style for C
(setq-default c-default-style "linux"
	      indent-tabs-mode t)

;; set indentation for Java
(defun java-mode-indentation()
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'java-mode-indentation)
(add-hook 'java-ts-mode-hook 'java-mode-indentation)

;; set the dictionary server for dictionary lookup to dict.org
(setq dictionary-server "dict.org")
(global-set-key (kbd "C-c l") #'dictionary-lookup-definition)

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses
`flyspell-prog-mode' for modes derived from `prog-mode', so only strings and
comments get checked.  All other buffers get `flyspell-mode' to check all text.
If flyspell is already enabled, does nothing.

source: https://www.emacswiki.org/emacs/FlySpell "
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
    (progn
      (if (derived-mode-p 'prog-mode)
        (progn
          (message "Flyspell on (code)")
          (flyspell-prog-mode))
        ; else
        (progn
          (message "Flyspell on (text)")
          (flyspell-mode 1))))))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses
`flyspell-on-for-buffer-type' so code-vs-text is handled appropriately.

source: https://www.emacswiki.org/emacs/FlySpell "
  (interactive)
    (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
      ; else - flyspell is off, turn it on
      (flyspell-on-for-buffer-type)))

(global-set-key (kbd "C-c f") 'flyspell-toggle)

(defun yiyu/delete-other-windows-and-kill-buffers ()
  "Make current window fill its frame and kill the buffers displayed in them."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (window (window-list))
      (unless (eq (window-buffer window) current-buffer)
        (kill-buffer (window-buffer window))
        (delete-window window))))
  (message "Other windows deleted and buffers killed."))

;; open Emacs config file
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "clear" "clear 1")))

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; use-package
(require 'use-package)

;; ensure packages are automatically installed when using use-package
(setq use-package-always-ensure t)

;; auto-package-update
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  (evil-mode 1)

  ;; set space key as global leader
  ;;(evil-set-leader 'normal " ")

  ;; set backslash as local leader
  ;;(evil-set-leader 'normal "\\" t)

  ;; buffer operation
  (evil-define-key 'normal 'global (kbd "<leader>bs") 'switch-to-only-file-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bS") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> w1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader> W1") 'delete-other-windows-and-kill-buffers)
  (evil-define-key 'normal 'global (kbd "C-w O") 'delete-other-windows-and-kill-buffers)

  ;; file operation
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'find-file)

  ;; emacs config
  (evil-define-key 'normal 'global (kbd "<leader>fc") 'open-init-file))

;; Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; general.el
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer yiyu/leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ",")

  (general-create-definer yiyu/localleader
    :states '(normal insert visual emacs)
    :prefix "C-,"))

(defun yiyu/eval-eol-sexp ()
  (interactive)
  (let ((is-in-insert-state (evil-insert-state-p))
	(cursor-pos (point))) ; preserve cursor position
    (evil-insert 1)
    (end-of-line)
    (eval-last-sexp nil)
    (if is-in-insert-state
	(evil-insert 1)
      (progn
	(evil-force-normal-state)
	(forward-char 1))) ; counteract evil state switching offset
    (goto-char cursor-pos)))

;; global leader
(yiyu/leader
  "h" 'help
  "bs" 'switch-to-buffer
  "bk" 'kill-buffer
  "wk" 'yiyu/delete-other-windows-and-kill-buffers
  "fc" 'open-init-file
  "fs" 'find-file
  "gs" 'magit-status)

;; local leader for emacs-lisp-mode
(yiyu/localleader
  :keymaps 'emacs-lisp-mode-map
  "e" 'yiyu/eval-eol-sexp)

;; git-gutter
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
  (evil-define-key 'normal 'global (kbd "<leader>gj") 'git-gutter:next-hunk)
  (evil-define-key 'normal 'global (kbd "<leader>gk") 'git-gutter:previous-hunk))

;; git-gutter-fringe.el (disable in tty frame)
(if (display-graphic-p)
    (use-package git-gutter-fringe
    :config
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)))

;; ERC
(setq
  erc-nick "yiyu"
  erc-user-full-name "Yiyu Zhou"
  erc-track-shorten-start 8
  erc-kill-buffer-on-part t
  erc-auto-query 'bury
  erc-fill-column 120
  erc-fill-function 'erc-fill-static
  erc-fill-static-center 16)

;; Libera Chat
(defun libera-chat ()
  (interactive)
  (let ((password (read-passwd "Password: ")))
    (erc-tls :server "irc.libera.chat"
             :port "6697"
             :password password)))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; yasnippet
(use-package yasnippet
  :config (yas-global-mode))

;; yasnippet-snippets
(use-package yasnippet-snippets)

;; Eglot
(require 'eglot)
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
  (evil-define-key 'normal 'global (kbd "<leader>li") 'eglot-code-action-organize-imports)
  (evil-define-key 'normal 'global (kbd "<leader>lh") 'eldoc)
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'eglot-format)
  (evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-code-actions)
  (evil-define-key 'normal 'global (kbd "<leader>lj") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "<leader>lk") 'flymake-goto-prev-error))

;; Eglot language specific hooks
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'python-base-mode-hook 'eglot-ensure)

;; Eglot setup for rust-analyzer
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

;; eglot-java (Eclipse JDT LS)
(use-package eglot-java
  :hook
  (java-mode    . eglot-java-mode)
  (java-ts-mode . eglot-java-mode)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>ln") 'eglot-java-file-new)
  (evil-define-key 'normal 'global (kbd "<leader>lx") 'eglot-java-run-main)
  (evil-define-key 'normal 'global (kbd "<leader>lt") 'eglot-java-run-test)
  (evil-define-key 'normal 'global (kbd "<leader>lN") 'eglot-java-project-new)
  (evil-define-key 'normal 'global (kbd "<leader>lT") 'eglot-java-project-build-task)
  (evil-define-key 'normal 'global (kbd "<leader>lR") 'eglot-java-project-build-refresh))

;; dape
(use-package dape
  :config
  (evil-define-key 'normal 'global (kbd "<leader>dl") 'dape)
  (evil-define-key 'normal 'global (kbd "<leader>dq") 'dape-quit)
  (evil-define-key 'normal 'global (kbd "<leader>db") 'dape-breakpoint-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>dc") 'dape-breakpoint-remove-all)
  (evil-define-key 'normal 'global (kbd "<leader>dn") 'dape-next)
  (evil-define-key 'normal 'global (kbd "<leader>ds") 'dape-step-in)
  (evil-define-key 'normal 'global (kbd "<leader>do") 'dape-step-out))

;; company-mode
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default: 0.2
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)))) ; disable company in eshell

;; flycheck
(use-package flycheck)

;; Magit
(use-package magit
  :config
  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status))

;; Racket Mode
(use-package racket-mode)

;; org-superstar-mode
(use-package org-superstar
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; rust-mode (major mode for Rust)
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;; go-mode.el (major mode for Go)
(use-package go-mode)

;; markdown-mode (major mode for Markdown)
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; libvterm
(use-package vterm)

;; perspective.el
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode)
  :config
  ;; key bindings
  (define-key global-map (kbd "M-p") 'persp-prev)
  (define-key global-map (kbd "M-n") 'persp-next)
  (evil-define-key 'normal 'global (kbd "<leader>ps") 'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader>pk") 'persp-kill)
  (evil-define-key 'normal 'global (kbd "<leader>pmb") 'persp-set-buffer)) ;; move buffer to current perspective
