;; dark-mode
(define-configuration buffer
    ((default-modes (append '(dark-mode) %slot-value%))))

;; vi keybinds
(define-configuration buffer
    ((default-modes
	 (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

;; blocker-mode
(define-configuration web-buffer
    ((default-modes
	 (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

(define-configuration input-buffer
    ((override-map
      (let ((map (make-keymap "override-map")))
	(define-key map "M-x" 'execute-command)))))
