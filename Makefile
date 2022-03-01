EMACS ?= emacs
ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# .PHONY: emacs-build
# emacs-build:

.PHONY: emacs-test
emacs-test:
	@$(EMACS) --batch --eval '(load "$(ROOT_DIR)/.emacs.d/init.el")'

build-wsl:  build-emacs build-xrdp

build-emacs:
	sudo apt-get update && sudo apt-get upgrade -y
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	sudo apt install build-essential automake clang libclang-dev -y
	sudo apt install texinfo libjpeg-dev libtiff-dev \
		libgif-dev libxpm-dev libgtk-3-dev gnutls-dev \
		libncurses5-dev libxml2-dev libxt-dev
	mkdir $${HOME}/packages || true
	git clone https://github.com/emacs-ng/emacs-ng.git $${HOME}/packages
	cd $${HOME}/packages && \
		./autogen.sh &&\
		./configure &&\
		make -j$(nproc) &&\
		sudo make install

# reference : https://yoshimemo.com/post-723/
build-xrdp:
	sudo apt-get upate && sudo apt-get upgrade -y
	sudo apt-get install -y xrdp xubuntu-desktop
	sudo sed -i -e '/^port/s/3389/13390/g' /etc/xrdp/xrdp.ini
	sudo /etc/init.d/dbus start
	sudo /etc/init.d/xrdp start
	echo xfce4-session > ~/.xsession
	echo "export XDG_SESSION_DESKTOP=xubuntu" > ~/.xsessionrc
	echo "export XDG_DATA_DIRS=/usr/share/xfce4:/usr/share/xubuntu:/usr/local/share:/usr/share:/var/lib/snapd/desktop:/usr/share" >> ~/.xsessionrc
	echo "export XDG_CONFIG_DIRS=/etc/xdg/xdg-xubuntu:/etc/xdg:/etc/xdg" >> ~/.xsessionrc
