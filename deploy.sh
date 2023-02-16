#!/bin/bash
SCRIPT_DIR="$(dirname "$(realpath "$0")")"

cp $SCRIPT_DIR/.vimrc $HOME/

cp $SCRIPT_DIR/.bashrc $HOME/
source $HOME/.bashrc
