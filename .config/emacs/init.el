(use-package emacs
  :config
  (load-theme 'modus-vivendi)
  (display-battery-mode)
  (display-time-mode)

  :custom
  ;; disable binking cursor in a text terminal (TTY frames)
  (visible-cursor nil)

  ;; relative line numbers
  (display-line-numbers-type 'relative)

  ;; ensure smooth scrolling
  (scroll-conservatively 101)

  ;; set a margin of 10 lines at the top and bottom for scrolling
  (scroll-margin 10)

  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Use `cape-dict' as an alternative
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not used via M-x.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :hook
  ;; update buffers when files on the disk changes
  (after-init . global-auto-revert-mode)

  :config
  (set-face-attribute 'default nil :height 125) ; default font size

  ;; set path for customise system
  (setq custom-file "~/.config/emacs/custom.el")
  (ignore-errors (load custom-file)) ;; It may not yet exist.

  ;; enable line number
  (global-display-line-numbers-mode 1)

  ;; set non blinking cursor
  (blink-cursor-mode 0)

  ;; enable hl-line-mode to highlight the current line
  (global-hl-line-mode t))

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
	  (progn ; else
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
    (flyspell-on-for-buffer-type))) ; else - flyspell is off, turn it on

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

;; Eshell
(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda ()
			 (eshell/alias "clear" "clear 1")))
  :config (bind-key* "M-RET" 'eshell))

;; auto-package-update
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

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
  :custom (which-key-idle-delay 0.1)
  :config (which-key-mode))

;; corfu.el (COmpletion in Region FUnction)
(use-package corfu
  :init (global-corfu-mode)
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto t)
  :bind (:map corfu-map
	      ("M-SPC" . corfu-insert-separator)))

;; corfu-terminal (corfu popup in TTY frames)
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config (corfu-terminal-mode +1))

;; orderless (with fido-vertical-mode)
(use-package orderless
  :config (fido-vertical-mode)
  :custom (completion-styles '(orderless))
  :hook
  (icomplete-minibuffer-setup . (lambda ()
                                  (setq-local completion-styles '(orderless)))))

;; Marginalia (rich annotations)
(use-package marginalia
  :init (marginalia-mode))

;; org-modern
(use-package org-modern
  :after org
  :config (global-org-modern-mode))

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
(use-package eglot
  :ensure nil
  :hook
  ;; Format on save
  (eglot-managed-mode . (lambda () (add-hook 'before-save-hook #'eglot-format-buffer t t)))
  ((haskell-mode
    rust-mode
    rust-ts-mode
    go-ts-mode
    python-base-mode
    nix-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
		 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  :config
  (setq-default eglot-workspace-configuration '((:gopls . ((gofumpt . t))))))

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
(use-package dape)

;; extra major modes
(use-package racket-mode)
(use-package haskell-mode)
(use-package rust-mode)
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; lilypond-mode (Major mode for editing GNU LilyPond files)
;; GNU LilyPond system package automatically adds its Elisp files into the
;; /usr/share/emacs/site-lisp (/run/current-system/sw/share/emacs/site-lisp on
;; Nix) directory, which is in the default `load-path'.
;; Therefore, make sure GNU LilyPond is installed and set ensure to nil.
(when (executable-find "lilypond")
  (use-package lilypond-mode
    :ensure nil
    :mode ("\\.\\(ly\\|ily\\)$" . LilyPond-mode)
    :hook (LilyPond-mode . turn-on-font-lock)))

;; pdf-tools
(use-package pdf-tools
  :pin manual ; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

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
  (define-key global-map (kbd "M-n") 'persp-next))

;; diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)      ; handle unsaved buffers
  (unless (display-graphic-p) ; enable margin mode for TTY frames
    (diff-hl-margin-mode 1)))

;; Magit
(use-package magit
  :hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Evil
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  (evil-mode 1))

;; Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; general.el
(use-package general
  :after evil
  :hook (after-init . (lambda ()
			(when-let ((messages-buffer (get-buffer "*Messages*")))
			  (with-current-buffer messages-buffer
			    (evil-normalize-keymaps)))))
  :config
  (general-evil-setup t)

  ;; Custom window comamnds
  (general-define-key
     :keymaps 'override
     "C-h" 'evil-window-left
     "C-l" 'evil-window-right
     "C-j" 'evil-window-down
     "C-k" 'evil-window-up)

  (general-create-definer yiyu/leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ",")

  (general-create-definer yiyu/localleader
    :states '(normal insert visual emacs)
    :prefix "C-,")

  ;; global leader
  (yiyu/leader
    ","   (lambda () (interactive) (insert ","))               ; escape to a comma: ","
    "SPC" (lambda () (interactive) (insert ", "))              ; escape to a comma followed by a space: ", "
    "c"   (lambda () (interactive) (find-file user-init-file)) ; open Emacs config file
    "x"   'execute-extended-command
    "h"   'help
    "b"   'switch-to-buffer
    "k"   'kill-buffer
    "w"   'yiyu/delete-other-windows-and-kill-buffers
    "f"   'find-file
    "gs"  'magit-status
    "gj"  'diff-hl-next-hunk
    "gk"  'diff-hl-previous-hunk
    "ps"  'persp-switch
    "pk"  'persp-kill
    "pb"  'persp-set-buffer)                                   ; move buffer to current perspective

  ;; local leader for emacs-lisp-mode
  (yiyu/localleader
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e" (lambda () ; eval the last sexp at the end of the line
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
            (goto-char cursor-pos))))

  ;; local leader for eglot and dape (LSP and DAP)
  (yiyu/localleader
    :keymaps '(eglot-mode-map dape-global-map)
    "r" 'eglot-rename
    "i" 'eglot-code-action-organize-imports
    "h" 'eldoc
    "a" 'eglot-code-actions
    "j" 'flymake-goto-next-error
    "k" 'flymake-goto-prev-error
    "d" 'dape
    "q" 'dape-quit
    "b" 'dape-breakpoint-toggle
    "c" 'dape-breakpoint-remove-all
    "n" 'dape-next
    "s" 'dape-step-in
    "o" 'dape-step-out)

  (yiyu/localleader
    :keymaps '(org-mode-map)
    "f" 'org-fill-paragraph
    "p" 'org-latex-export-to-pdf))
