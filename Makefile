.PHONY: default install clean sync

default:
	@# vim-plug
	@if [ ! -f ~/.config/vim/autoload/plug.vim ]; then \
		echo "vim-plug not found. Installing..."; \
		curl -fLo ~/.config/vim/autoload/plug.vim --create-dirs \
			https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim; \
	else \
		echo "vim-plug is already installed."; \
	fi
	@# Tmux Plugin Manager
	@if [ ! -d ~/.config/tmux/plugins/tpm ]; then \
		echo "TPM not found. Installing..."; \
		git clone https://github.com/tmux-plugins/tpm ~/.config/tmux/plugins/tpm; \
	else \
		echo "TPM is already installed."; \
	fi

install:
	stow .

clean:
	stow -D .

sync:
	git pull
