;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs Initialization File

;;; Code:

(use-package emacs
  :hook (after-init . global-auto-revert-mode)    ; update buffers when files on the disk changes
  :custom
  ;; Backups
  (backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)

  ;; disable blinking cursor in a text terminal (TTY frames)
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

  ;; set path for customize system
  (custom-file
   (concat user-emacs-directory "custom.el"))
  :config
  (ignore-errors (load custom-file))              ; custom file may not yet exist.
  (set-default-toplevel-value 'lexical-binding t) ; default 'lexical-binding' to t
  (global-completion-preview-mode)
  (set-face-attribute 'default nil
		      :height 125)                ; default font size
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode t)                         ; highlight the current line
  (blink-cursor-mode 0)
  (load-theme 'modus-vivendi)
  (display-battery-mode)
  (display-time-mode))

;; package archives
(use-package package
  :config
  (package-initialize)
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
  (add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; window.el
(use-package window
  :config
  (defun yiyu/delete-other-windows-and-kill-buffers ()
    "Make current window fill its frame and kill the buffers displayed in them."
    (interactive)
    (let ((current-buffer (current-buffer)))
      (dolist (window (window-list))
        (unless (eq (window-buffer window) current-buffer)
  	  (kill-buffer (window-buffer window))
  	  (delete-window window))))
    (message "Other windows deleted and buffers killed.")))

;; midnight.el
(use-package midnight
  :config
  (midnight-mode +1)
  (midnight-delay-set 'midnight-delay 16200)) ; (eq (* 4.5 60 60) "4:30am")

;; dictionary-mode
(use-package dictionary
  :custom
  (dictionary-server "dict.org"))

;; Flyspell
(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  ((org-mode
    text-mode)
   . flyspell-mode)
  :custom
  (flyspell-use-meta-tab nil)) ; Do not bind M-<tab>, used for `completion-at-point'

;; Eshell
(use-package eshell
  :hook (eshell-mode . (lambda ()
			 (eshell/alias "clear" "clear 1")))
  :config (bind-key* "M-RET" 'eshell))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eshell-visual-commands '())
  :config
  (eat-eshell-mode))

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; ERC
(use-package erc
  :custom
  (erc-nick "yiyu")
  (erc-user-full-name "Yiyu Zhou")
  (erc-track-shorten-start 8)
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  (erc-fill-column 120)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 16)
  :config
  (defun yiyu/libera-chat ()
    (interactive)
    (let ((password (read-passwd "Password: ")))
      (erc-tls :server "irc.libera.chat"
	       :port "6697"
	       :password password))))

;; which-key
(use-package which-key
  :custom (which-key-idle-delay 0.1)
  :config (which-key-mode))

;; EditorConfig
(use-package editorconfig
  :config (editorconfig-mode 1))

;; dired-preview
(use-package dired-preview
  :ensure t
  :config (dired-preview-global-mode)
  :custom (dired-preview-delay 0.1))

;; orderless
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;; icomplete (fido-vertical-mode)
(use-package icomplete
  :hook
  (icomplete-minibuffer-setup . (lambda ()
                                  (setq-local completion-styles '(orderless))))
  :config (fido-vertical-mode))

;; consult.el (Consulting completing-read)
(use-package consult
  :ensure t
  :custom
  (completion-in-region-function 'consult-completion-in-region))

;; Marginalia (rich annotations)
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; Org Mode
(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . turn-on-auto-fill)
  (org-mode . (lambda ()
		(add-hook 'before-save-hook
			  (lambda ()
			    (when (derived-mode-p 'org-mode)
			      (yiyu/org-fill-paragraph-buffer))))))
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil) ; Trust execution
  (org-latex-pdf-process '("tectonic %f")) ; Use Tectonic for PDF export
  :config
  (defun yiyu/org-fill-paragraph-buffer()
    (org-with-wide-buffer
     (let* ((elements (reverse
		       (org-element-map (org-element-parse-buffer)
					'(paragraph) #'identity))))
       (progn
	 (dolist (el elements)
	   (goto-char (org-element-property :contents-begin el))
	   (org-fill-paragraph))))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t))))

;; org-babel-lilypond
(use-package ob-lilypond)

(use-package ob-haskell)

;; org-modern
(use-package org-modern
  :ensure t
  :after org
  :config (global-org-modern-mode))

;; indent-bars
(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

;; AUCTeX
(use-package auctex
  :ensure t
  :hook
  (after-change-major-mode . (lambda ()
			       (when-let* ((project (project-current))
					   (proot (project-root project)))
				 (when (file-exists-p (expand-file-name "Tectonic.toml" proot))
				   (setopt TeX-output-dir (expand-file-name "build/index" proot))))))
  :custom
  (TeX-engine-alist '((default
                       "Tectonic"
                       "tectonic -X compile -f plain %T"
                       "tectonic -X watch"
                       nil)))
  (LaTeX-command-style '(("" "%(latex)")))
  (TeX-process-asynchronous t)
  (TeX-check-TeX nil)
  (TeX-engine 'default))

(use-package tex
  :after auctex
  :config
  (let ((tex-list (assoc "TeX" TeX-command-list))
	(latex-list (assoc "LaTeX" TeX-command-list)))
    (setf (cadr tex-list) "%(tex)"
          (cadr latex-list) "%l")))

;; treesit-auto
(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install t)
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (add-to-list 'treesit-auto-langs 'haskell)
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'haskell
		:ts-mode 'haskell-ts-mode
		:remap 'haskell-mode
		:url "https://github.com/tree-sitter/tree-sitter-haskell"
		:ext "\\.hs\\'")))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

;; Flymake
(use-package flymake
  :hook
  (emacs-lisp-mode . flymake-mode))

;; flylisp
(use-package flylisp
  :ensure t
  :hook
  ((emacs-lisp-mode
    common-lisp-mode
    scheme-mode
    racket-mode)
   . flylisp-mode))

;; Eglot
(use-package eglot
  :hook
  ;; Format on save
  (eglot-managed-mode . (lambda () (add-hook 'before-save-hook #'eglot-format-buffer t t)))
  ((scheme-mode
    haskell-ts-mode
    rust-ts-mode
    go-ts-mode
    bash-ts-mode
    python-base-mode
    java-ts-mode
    nix-ts-mode
    mhtml-ts-mode
    css-mode
    javascript-mode
    json-ts-mode
    markdown-ts-mode
    TeX-mode
    bibtex-mode)
   . eglot-ensure)
  :config
  (dolist (config '((scheme-mode . ("scheme-langserver"))
		    (nix-ts-mode . ("nil" :initializationOptions
                                    (:formatting (:command ["nixfmt"]))))
                    (rust-ts-mode . ("rust-analyzer" :initializationOptions
                                     (:check (:command "clippy"))))))
    (let ((major-mode (car config))
          (contact (cdr config)))
      (add-to-list 'eglot-server-programs (cons major-mode contact))))
  (setq-default eglot-workspace-configuration '((:gopls . ((gofumpt . t)
							   (hints . ((assignVariableTypes . t)
								     (compositeLiteralFields . t))))))))

;; dape
(use-package dape
  :ensure t
  :hook
  ;; Auto save and load breakpoints
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  ;; Kill compile buffer on build success
  (dape-compile . kill-buffer)
  ;; Save buffers on startup, useful for interpreted languages
  (dape-start . (lambda () (save-some-buffers t t)))
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t))

;; extra major modes
(use-package racket-mode
  :ensure t)
(use-package haskell-ts-mode
  :ensure t
  :mode "\\.hs\\'"
  :hook
  (haskell-ts-mode . haskell-ts-setup-eglot))
(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

;; lilypond-mode (Major mode for editing GNU LilyPond files)
;; GNU LilyPond system package automatically adds its Elisp files into the
;; /usr/share/emacs/site-lisp (/run/current-system/sw/share/emacs/site-lisp on
;; Nix) directory, which is in the default `load-path'.
;; Therefore, make sure GNU LilyPond is installed and set ensure to nil.
(when (executable-find "lilypond")
  (use-package lilypond-mode
    :mode ("\\.\\(ly\\|ily\\)$" . LilyPond-mode)
    :hook (LilyPond-mode . turn-on-font-lock)))

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

;; Visual undo tree
(use-package vundo
  :ensure t
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; pyim (中文输入法)
(use-package pyim
  :ensure t
  :after pyim-basedict)
(use-package pyim-basedict
  :ensure t
  :config (pyim-basedict-enable))

;; perspective.el
(use-package perspective
  :ensure t
  :init
  (persp-mode)
  :custom
  (persp-suppress-no-prefix-key-warning t))

;; diff-hl
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)      ; handle unsaved buffers
  (unless (display-graphic-p) ; enable margin mode for TTY frames
    (diff-hl-margin-mode 1)))

;; Magit
(use-package magit
  :ensure t
  :hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Evil
(use-package evil
  :ensure t
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Evil Collection
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; general.el
(use-package general
  :ensure t
  :after evil
  :hook (after-init . (lambda ()
			(when-let* ((messages-buffer (get-buffer "*Messages*")))
			  (with-current-buffer messages-buffer
			    (evil-normalize-keymaps)))))
  :config
  (general-evil-setup t)

  ;; Custom window commands
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
    "RET" (lambda () (interactive) (insert ",\n"))             ; escape to a comma followed by a return: ",\n"
    "c"   (lambda () (interactive) (find-file user-init-file)) ; open Emacs config file
    "d"   'dictionary-lookup-definition
    "x"   'execute-extended-command
    "h"   'help
    "/"   'consult-line
    "m"   'consult-man
    "b"   'consult-buffer
    "k"   'kill-buffer
    "w"   'yiyu/delete-other-windows-and-kill-buffers
    "f"   'find-file
    "u"   'vundo
    "gs"  'magit-status
    "gj"  'diff-hl-next-hunk
    "gk"  'diff-hl-previous-hunk
    "s"   'yiyu/flyspell-toggle
    "p"   'perspective-map :which-key "perspective"
    "P"   'persp-list-buffers
    "R"   'restart-emacs)

  ;; local leader for emacs-lisp-mode
  (yiyu/localleader
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "E" (defun yiyu/eval-last-sexp-eol ()
	  "Evaluate the last sexp at the end of the line.
Evaluate at the end of the sexp when a comment exists at the end of the
line.  Restore the current position of point and the Evil state after
the call."
	  (interactive)
	  (save-excursion
	    (evil-save-state
	      (evil-insert 1)
	      (unless (search-forward ";" (pos-eol) t)
		(end-of-line))
	      (eval-last-sexp nil)))))

  ;; local leader for LSP/DAP and frontend minor modes
  (yiyu/localleader
    :keymaps '(flymake-mode-map eglot-mode-map dape-global-map)
    "e" 'consult-flymake
    "j" 'flymake-goto-next-error
    "k" 'flymake-goto-prev-error
    "h" 'eldoc
    "r" 'eglot-rename
    "a" 'eglot-code-actions
    "i" 'eglot-code-action-organize-imports
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
    "p" 'org-latex-export-to-pdf)

  (yiyu/localleader
    :keymaps '(LaTeX-mode-map)
    "f" 'LaTeX-fill-paragraph)

  (yiyu/localleader
    :keymaps '(go-ts-mode-map)
    "t" 'go-ts-mode-test-function-at-point
    "T" 'go-ts-mode-test-this-package))

;; Restart Emacs
(use-package restart-emacs
  :ensure t)

;;; init.el ends here
