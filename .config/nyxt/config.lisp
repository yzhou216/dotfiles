;; emacs-mode
(define-configuration (web-buffer prompt-buffer panel-buffer
                       nyxt/mode/editor:editor-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration input-buffer
  ((override-map
    (let ((map (make-keymap "override-map")))
      (define-key map "M-x" 'execute-command)))))
