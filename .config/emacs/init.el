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

(defun delete-other-windows-and-kill-buffers ()
  "Make current window fill its frame and kill the buffers displayed in them."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (window (window-list))
      (unless (eq (window-buffer window) current-buffer)
        (kill-buffer (window-buffer window))
        (delete-window window))))
  (message "Other windows deleted and buffers killed."))
(global-set-key (kbd "C-x C-1") 'delete-other-windows-and-kill-buffers)

(defun switch-to-only-file-buffer (buffer-or-name &optional norecord force-same-window)
  "Switch to only file buffer, with the exception of '*scratch*'."
  (interactive
   (let ((force-same-window
          (cond
           ((window-minibuffer-p) nil)
           ((not (eq (window-dedicated-p) t)) 'force-same-window)
           ((pcase switch-to-buffer-in-dedicated-window
              (`nil (user-error
                     "Cannot switch buffers in a dedicated window"))
              (`prompt
               (if (y-or-n-p
                    (format "Window is dedicated to %s; undedicate it"
                            (window-buffer)))
                   (progn
                     (set-window-dedicated-p nil nil)
                     'force-same-window)
                 (user-error
                  "Cannot switch buffers in a dedicated window")))
              (`pop nil)
              (_ (set-window-dedicated-p nil nil) 'force-same-window))))))
     (list (read-buffer "Buffer: "
                        (other-buffer (current-buffer))
                        (confirm-nonexistent-file-or-buffer)
                        (lambda (name.buf)
  (let ((buf-name (car name.buf))
        (buf (cdr name.buf)))
    (and (not (string= "*scratch*" buf-name)) ; exception: *scratch* buffer
         (not (string-match "^\\*.*\\*$" buf-name)) ; exclude buffers with names surrounded by asterisks
         (not (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))))) ; exclude dired buffers
  (switch-to-buffer buffer-or-name norecord force-same-window))

(global-set-key (kbd "C-x b") 'switch-to-only-file-buffer) ; only file buffer (except *scratch*)
(global-set-key (kbd "C-x B") 'switch-to-buffer) ; all buffer

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

;; Evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  ;; use Emacs bindings in insert mode
  (setq evil-disable-insert-state-bindings t)

  :config
  (evil-mode 1)

  ;; set space key as global leader
  (evil-set-leader 'normal " ")

  ;; set backslash as local leader
  (evil-set-leader 'normal "\\" t)

  ;; buffer operation
  (evil-define-key 'normal 'global (kbd "<leader>bs") 'switch-to-only-file-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bS") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)

  ;; file operation
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'find-file)

  ;; emacs config
  (evil-define-key 'normal 'global (kbd "<leader>ec") 'open-init-file))

;; Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

;; which-key
(use-package which-key
  :config
  (which-key-mode))

;; treesit-auto
(use-package treesit-auto
  :config
  (setq treesit-auto-install t)
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
(add-hook 'python-base-mode-hook 'eglot-ensure)

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

;; org-bullets
(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

;; Emacs Application Framework (EAF)
(use-package quelpa-use-package)
;; external dependencies required, run M-x eaf-install to install
(use-package eaf
  :demand t
  :quelpa (eaf :fetcher github
              :repo  "manateelazycat/emacs-application-framework"
              :files ("*"))
  :load-path "~/.config/emacs/site-lisp/emacs-application-framework"
  :init
  (use-package epc      :defer t :ensure t)
  (use-package ctable   :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s        :defer t :ensure t)
  (setq browse-url-browser-function 'eaf-open-browser)
  :config
  (require 'eaf-browser))
