.PHONY: install

install:
	stow .

clean:
	stow -D .

sync:
	git pull
