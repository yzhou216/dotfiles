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

;; set indentation for Java
(add-hook 'java-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)))

;; set dark mode
(when (display-graphic-p)
  (invert-face 'default)
)
(set-variable 'frame-background-mode 'dark)

;; set path for customise system
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; use-package
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; evil mode
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-redo))
