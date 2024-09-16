# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# Local customized path and environment settings
if [ -f ~/.bash_local ]; then
  . ~/.bash_local
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
  PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

if [ -d ~/.bashrc.d ]; then
  for rc in ~/.bashrc.d/*; do
    if [ -f "$rc" ]; then
      . "$rc"
    fi
  done
fi

unset rc

# User specific aliases and functions
alias ls='ls --color=auto'
alias emacs='emacs --no-window-system'
alias golines='golines --max-len=80 --tab-len=8 --write-output'

# set VIM as default editor
export VISUAL=vim
export EDITOR="$VISUAL"

# autograder
export PATH=~/autograder:$PATH
