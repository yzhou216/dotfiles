(require-builtin steel/process)
(require "steel/result")

(define (with-stdout-piped cmd)
  (set-piped-stdout! cmd)
  cmd)

(define (install-from-git url)
  (displayln "Installing: " url)
  (~> (command "forge" (list "pkg" "install" "--git" url))
      with-stdout-piped
      spawn-process
      Ok->value
      wait->stdout
      Ok->value
      displayln))

(install-from-git "https://github.com/mattwparas/vim.hx.git")

(require "vim-hx/init.scm")
(set-vim-keybindings!)
