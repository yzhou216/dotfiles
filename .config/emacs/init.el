;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; The Emacs Initialization File

;;; Code:

(use-package emacs
  :custom
  (mode-line-collapse-minor-modes 1)
  (user-full-name "Yiyu Zhou")
  (user-mail-address "yiyu@yiyuzhou.io")
  (custom-enabled-themes '(modus-vivendi))

  ;; Backups
  (backup-directory-alist
   `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
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

  ;; Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not used via M-x.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Revert all buffers (useful for Dired)
  (global-auto-revert-non-file-buffers t)

  ;; set path for customize system
  (custom-file
   (concat user-emacs-directory "custom.el"))
  :config
  (ignore-errors (load custom-file))              ; custom file may not yet exist.
  (set-default-toplevel-value 'lexical-binding t) ; default 'lexical-binding' to t
  (set-face-attribute 'default nil
		      :height 125)                ; default font size
  (global-display-line-numbers-mode 1)
  (global-visual-wrap-prefix-mode 1)              ; Visual-Wrap-Prefix mode in all buffers
  (global-hl-line-mode t)                         ; highlight the current line
  (global-completion-preview-mode)                ; completion preview
  (global-auto-revert-mode 1)                     ; update buffers when files on the disk changes
  (recentf-mode 1)                                ; recently visited files
  (save-place-mode 1)                             ; drop point to last visited location
  (blink-cursor-mode 0)
  (column-number-mode)
  (display-battery-mode))

;; package archives
(use-package package
  :config
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
  (add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

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
  (midnight-delay-set 'midnight-delay 16200) ; (eq (* 4.5 60 60) "4:30am")
  (midnight-mode +1))

(use-package time
  :custom
  (display-time-format "%Y-%m-%dT%H:%M:%S%:z") ; ISO 8601
  (display-time-interval 1)
  :config (display-time-mode))

;; dictionary-mode
(use-package dictionary
  :custom (dictionary-server "dict.org"))

;; Flyspell
(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  ((org-mode
    text-mode)
   . flyspell-mode)
  :custom (flyspell-use-meta-tab nil)) ; Do not bind M-<tab>, used for `completion-at-point'

;; Eshell
(use-package eshell
  :hook (eshell-mode . (lambda ()
			 (eshell/alias "clear" "clear 1")))
  :config (bind-key* "M-RET" 'eshell))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom (eshell-visual-commands '())
  :config (eat-eshell-mode))

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (unless (getenv "REKA")
    (auto-package-update-maybe))
  (auto-package-update-at-time "09:00"))

(use-package gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :custom
  (mail-user-agent 'gnus-user-agent)
  (read-mail-command 'gnus)
  (message-send-mail-function 'smtpmail-send-it)
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods
   '((nntp "news.gwene.org")
     (nnimap "zoho"
             (nnimap-address "imappro.zoho.com")
             (nnimap-server-port 993)
             (nnimap-stream tls)
             (nnimap-authenticator plain)
             (nnimap-user "yiyu@yiyuzhou.io"))
     (nnimap "cock"
             (nnimap-address "mail.cock.li")
             (nnimap-server-port 993)
             (nnimap-stream tls)
             (nnimap-authenticator plain)
             (nnimap-user "yiyu@cock.li"))))
  (gnus-posting-styles
   '(("nnimap\\+zoho:.*"
      (address "yiyu@yiyuzhou.io")
      ("X-Message-SMTP-Method" "smtp smtppro.zoho.com 465")
      (gcc "nnimap+zoho:INBOX"))
     ("nnimap\\+cock:.*"
      (address "yiyu@cock.li")
      ("X-Message-SMTP-Method" "smtp mail.cock.li 465")
      (gcc "nnimap+cock:INBOX"))))
  (gnus-home-directory (expand-file-name "gnus/" user-emacs-directory))
  (gnus-directory (expand-file-name "gnus/news/" user-emacs-directory))
  (gnus-cache-directory (expand-file-name "gnus/news/cache/" user-emacs-directory))
  (message-directory (expand-file-name "gnus/mail/" user-emacs-directory))
  (nndraft-directory (expand-file-name "gnus/drafts/" user-emacs-directory))
  (gnus-permanently-visible-groups ":INBOX$")
  (gnus-gcc-mark-as-read t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-interactive-exit nil)
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-number
     gnus-thread-sort-by-subject
     gnus-thread-sort-by-date))
  (message-confirm-send t)
  (message-forward-as-mine t))

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
  (erc-log-insert-log-on-open 'erc-log-new-target-buffer-p)
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

(use-package dired :hook (dired-mode . turn-on-gnus-dired-mode))

;; dired-preview
(use-package dired-preview
  :ensure t
  :config (dired-preview-global-mode)
  :custom (dired-preview-delay 0))

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
  :custom (completion-in-region-function 'consult-completion-in-region))

;; Marginalia (rich annotations)
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

;; Org Mode
(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . refill-mode)
  (org-mode . auto-fill-mode)
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil) ; Trust execution
  (org-latex-pdf-process '("tectonic %f")) ; Use Tectonic for PDF export
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t))))

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

(use-package treesit
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'always))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

;; yasnippet-snippets
(use-package yasnippet-snippets :ensure t)

;; Flymake
(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode))

;; flylisp
(use-package flylisp
  :ensure t
  :hook ((emacs-lisp-mode
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
  (setq-default eglot-workspace-configuration
		'((:gopls . ((gofumpt . t)
			     (hints . ((assignVariableTypes . t)
				       (compositeLiteralFields . t)))))))
  :custom
  (eglot-documentation-renderer 'markdown-ts-view-mode))

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

(use-package buffer-env
  :ensure t
  :hook
  (hack-local-variables . buffer-env-update)
  (comint-mode . buffer-env-update)
  :config
  (add-to-list 'buffer-env-command-alist ; direnv integration
	       '("/\\.envrc\\'" . "direnv exec . env -0")))

;; Third-party major modes
(use-package haskell-ts-mode
  :ensure t
  :hook (haskell-ts-mode . haskell-ts-setup-eglot))
(use-package racket-mode :ensure t)
(use-package nix-ts-mode :ensure t)

;; lilypond-mode (Major mode for editing GNU LilyPond files) GNU
;; LilyPond system package automatically adds its Elisp files into the
;; /usr/share/emacs/site-lisp
;; (/run/current-system/sw/share/emacs/site-lisp on Nix) directory,
;; which is in the default `load-path'.  Therefore, make sure GNU
;; LilyPond is installed and set ensure to nil.
(when (executable-find "lilypond")
  (use-package lilypond-mode
    :mode ("\\.\\(ly\\|ily\\)$" . lilypond-mode)
    :hook ((lilypond-mode . (lambda ()
			      (turn-on-font-lock)
			      (add-hook 'after-save-hook
					(lambda ()
					  (save-window-excursion
					    (project-recompile)))
					nil t))))))

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

;; pyim backend
(use-package pyim-basedict
  :ensure t
  :config (pyim-basedict-enable))

;; perspective.el
(use-package perspective
  :ensure t
  :init (persp-mode)
  :custom (persp-suppress-no-prefix-key-warning t))

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
  :config (evil-mode 1))

;; Evil Collection
(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

;; general.el
(use-package general
  :ensure t
  :after evil
  :hook
  (after-init . (lambda ()
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
    "wk"  'yiyu/delete-other-windows-and-kill-buffers
    "wt"  'window-layout-transpose
    "wr"  'window-layout-rotate-clockwise
    "wfh" 'window-layout-flip-leftright
    "wfv" 'window-layout-flip-topdown
    "f"   'find-file
    "F"   'recentf-open-files
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
(use-package restart-emacs :ensure t)

;; river compositor with reka window manager
(when (getenv "REKA")
  (add-to-list 'load-path "/run/current-system/sw/lib")
  (require 'reka)

  (defun yiyu/reka-spawn (cmd)
    "Run CMD asynchronously without an Emacs buffer."
    (start-process "reka-spawn" nil "sh" "-c" cmd))

  (defun yiyu/reka-focus-next-window () (interactive) (next-window))
  (defun yiyu/reka-focus-prev-window () (interactive) (previous-window))
  (defun yiyu/reka-focus-next-output () (interactive) (other-frame 1))
  (defun yiyu/reka-focus-prev-output () (interactive) (other-frame -1))
  (defun yiyu/reka-swap-next () (interactive) (transpose-buffers 1))
  (defun yiyu/reka-swap-prev () (interactive) (transpose-buffers -1))

  (defun yiyu/reka-send-to-output (n)
    (interactive)
    (let ((buf (current-buffer))
          (frame (if (> n 0)
                     (next-frame (selected-frame) t)
                   (previous-frame (selected-frame) t))))
      (select-frame-set-input-focus frame)
      (switch-to-buffer buf)))
  (defun yiyu/reka-send-next-output () (interactive) (yiyu/reka-send-to-output 1))
  (defun yiyu/reka-send-prev-output () (interactive) (yiyu/reka-send-to-output -1))

  (defun yiyu/reka-exit ()
    (interactive)
    (start-process "river-exit" nil "pkill" "-TERM" "river"))

  (defun yiyu/reka-terminal () (interactive) (yiyu/reka-spawn "alacritty"))
  (defun yiyu/reka-launcher ()
    (interactive)
    (yiyu/reka-spawn "pkill wmenu; wmenu-run -l 24 -N 000000ff"))
  (defun yiyu/reka-dismiss-notifications ()
    (interactive) (yiyu/reka-spawn "fnottctl dismiss all"))
  (defun yiyu/reka-wayshot-full ()
    (interactive)
    (yiyu/reka-spawn "wayshot - | wl-copy -t image/png; wayshot -- ~/Pictures/Screenshots/$(date +%s%N | cut -b1-13)-wayshot.png"))
  (defun yiyu/reka-wayshot-region ()
    (interactive)
    (yiyu/reka-spawn "region=$(slurp); wayshot --geometry \"$region\" - | wl-copy -t image/png; wayshot --geometry \"$region\" -- ~/Pictures/Screenshots/$(date +%s%N | cut -b1-13)-wayshot.png"))

  (defun yiyu/reka-volume-up () (interactive) (yiyu/reka-spawn "pamixer -i 5"))
  (defun yiyu/reka-volume-down () (interactive) (yiyu/reka-spawn "pamixer -d 5"))
  (defun yiyu/reka-volume-mute () (interactive) (yiyu/reka-spawn "pamixer --toggle-mute"))
  (defun yiyu/reka-media-play-pause () (interactive) (yiyu/reka-spawn "playerctl play-pause"))
  (defun yiyu/reka-media-prev () (interactive) (yiyu/reka-spawn "playerctl previous"))
  (defun yiyu/reka-media-next () (interactive) (yiyu/reka-spawn "playerctl next"))
  (defun yiyu/reka-brightness-up () (interactive) (yiyu/reka-spawn "xbacklight -inc 10"))
  (defun yiyu/reka-brightness-down () (interactive) (yiyu/reka-spawn "xbacklight -dec 10"))

  ;; Keybindings are registered with river as XKB intercept prefixes so they
  ;; reach Emacs even when another app has keyboard focus. On Emacs 32 the
  ;; composed function-key events (e.g. `s-return') make event-basic-type
  ;; return nil, so reka-push-intercept-prefix would resolve the keysym as
  ;; "nil". Register via reka-register-xkb-prefix with explicit keysym names.
  (defun yiyu/reka-bind (key-string keysym-name command)
    "Register KEY-STRING (kbd syntax) as an XKB prefix matching KEY-NAME
and bind COMMAND to it in the global keymap."
    (let* ((data (reka--key-to-xkb key-string))
           (event (car data))
           (mods (caddr data))
           (event-for-rust (if (integerp event) event (symbol-name event))))
      (reka-register-xkb-prefix reka-handle event-for-rust keysym-name mods nil)
      (global-set-key (kbd key-string) command)))

  (defun yiyu/reka-bind-fullscreen (key-string keysym-name)
    "Register KEY-STRING for reka's builtin fullscreen toggle."
    (let* ((mods (caddr (reka--key-to-xkb key-string))))
      (reka-register-xkb-prefix reka-handle keysym-name keysym-name
                                mods 'toggle-fullscreen)))

  (defun yiyu/reka-keys ()
    (interactive)
    ;; window management
    (yiyu/reka-bind "s-j" "j" #'yiyu/reka-focus-next-window)
    (yiyu/reka-bind "s-k" "k" #'yiyu/reka-focus-prev-window)
    (yiyu/reka-bind "s-S-j" "j" #'yiyu/reka-swap-next)
    (yiyu/reka-bind "s-S-k" "k" #'yiyu/reka-swap-prev)
    (yiyu/reka-bind "s-." "period" #'yiyu/reka-focus-next-output)
    (yiyu/reka-bind "s-," "comma" #'yiyu/reka-focus-prev-output)
    (yiyu/reka-bind "s-S->" "greater" #'yiyu/reka-send-next-output)
    (yiyu/reka-bind "s-S-<" "less" #'yiyu/reka-send-prev-output)
    (yiyu/reka-bind "s-S-<return>" "Return" #'delete-other-windows)
    (yiyu/reka-bind "s-S-q" "q" #'kill-current-buffer)
    (yiyu/reka-bind "s-S-s" "s" #'yiyu/reka-wayshot-region)
    ;; system keys
    (yiyu/reka-bind "s-<return>" "Return" #'yiyu/reka-terminal)
    (yiyu/reka-bind "s-d" "d" #'yiyu/reka-launcher)
    (yiyu/reka-bind "s-c" "c" #'yiyu/reka-dismiss-notifications)
    (yiyu/reka-bind "s-s" "s" #'yiyu/reka-wayshot-full)
    (yiyu/reka-bind "s-C-e" "e" #'yiyu/reka-exit)
    (yiyu/reka-bind "<XF86AudioRaiseVolume>" "XF86AudioRaiseVolume" 'yiyu/reka-volume-up)
    (yiyu/reka-bind "<XF86AudioLowerVolume>" "XF86AudioLowerVolume" 'yiyu/reka-volume-down)
    (yiyu/reka-bind "<XF86AudioMute>" "XF86AudioMute" 'yiyu/reka-volume-mute)
    (yiyu/reka-bind "<XF86AudioPlay>" "XF86AudioPlay" 'yiyu/reka-media-play-pause)
    (yiyu/reka-bind "<XF86AudioPrev>" "XF86AudioPrev" 'yiyu/reka-media-prev)
    (yiyu/reka-bind "<XF86AudioNext>" "XF86AudioNext" 'yiyu/reka-media-next)
    (yiyu/reka-bind "<XF86MonBrightnessUp>" "XF86MonBrightnessUp" 'yiyu/reka-brightness-up)
    (yiyu/reka-bind "<XF86MonBrightnessDown>" "XF86MonBrightnessDown" 'yiyu/reka-brightness-down)
    (yiyu/reka-bind-fullscreen "s-f" "f"))

  (add-hook 'reka-enable-hook #'yiyu/reka-keys)
  (reka-enable))

;;; init.el ends here
