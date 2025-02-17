;; hide uneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq-default
 use-dialog-box nil                            ; disable dialog box
 visible-bell t)

(setopt
 native-comp-async-report-warnings-errors nil  ; hide native comp warnings
 inhibit-startup-message t)                    ; inhibit startup message
