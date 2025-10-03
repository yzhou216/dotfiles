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

# Better cd
shopt -s autocd cdspell

# User specific aliases and functions
alias ls='ls --color=auto'
alias info='info --vi-keys'
alias emacs='emacs --no-window-system'
alias shfmt='shfmt --indent 2 --write'
alias golines='golines --max-len=80 --tab-len=8 --write-output'
alias python='rustpython'
alias python3='rustpython'

alias 'nh-switch'='nh os switch -- --impure'
nixos-switch() {
  cd ~/.config/flake || exit 1

  local rebuild_cmd="nixos-rebuild switch --flake . --impure"
  if [ -x "$(command -v doas)" ]; then
    doas "$rebuild_cmd"
  else
    sudo "$rebuild_cmd"
  fi

  cd - || exit 1
}

nsh() {
  if [ "$#" -eq 0 ]; then
    echo "Error: No packages specified"
    return 1
  fi

  packages=()
  for package in "$@"; do
    packages+=("nixpkgs#$package")
  done

  nix shell "${packages[@]}"
}

# set VIM as default editor
export VISUAL=vim
export EDITOR="$VISUAL"

# autograder
export PATH=~/autograder:$PATH
