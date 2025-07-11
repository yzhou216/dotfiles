# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

if [[ $- == *i* ]]; then
  # Auto-launching ssh-agent
  env=~/.ssh/agent.env

  agent_load_env() { test -f "$env" && . "$env" >|/dev/null; }

  agent_start() {
    (
      umask 077
      ssh-agent >|"$env"
    )
    . "$env" >|/dev/null
  }

  agent_load_env

  # agent_run_state: 0=agent running w/ key; 1=agent w/o key; 2=agent not running
  agent_run_state=$(
    ssh-add -l >|/dev/null 2>&1
    echo $?
  )

  if [ ! "$SSH_AUTH_SOCK" ] || [ $agent_run_state = 2 ]; then
    agent_start
    ssh-add
  elif [ "$SSH_AUTH_SOCK" ] && [ $agent_run_state = 1 ]; then
    ssh-add
  fi

  unset env
fi

# Fix for Java GUI
export _JAVA_AWT_WM_NONREPARENTING=1

# GTK theme
export GTK_THEME="Adwaita-dark";
export XCURSOR_THEME="Adwaita";
export XCURSOR_SIZE="24";

# QT programs settings
export QT_QPA_PLATFORMTHEME=qt5ct

# Autostart Wayland compositor
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  river
fi
