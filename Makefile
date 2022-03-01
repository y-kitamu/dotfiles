EMACS ?= emacs
ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# .PHONY: emacs-build
# emacs-build:

.PHONY: emacs-test
emacs-test:
	@$(EMACS) --batch --eval '(load "$(ROOT_DIR)/.emacs.d/init.el")'

build-wsl:  build-emacs build-xrdp

# Build emacs-ng
build-emacs:
	apt-get update && apt-get upgrade -y
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	apt install build-essential automake clang libclang-dev -y
	apt install texinfo libjpeg-dev libtiff-dev \
		libgif-dev libxpm-dev libgtk-3-dev gnutls-dev \
		libncurses5-dev libxml2-dev libxt-dev
	mkdir $${HOME}/packages || true
	git clone https://github.com/emacs-ng/emacs-ng.git $${HOME}/packages
	cd $${HOME}/packages && \
		./autogen.sh &&\
		./configure &&\
		make -j$(nproc) &&\
		make install

# Install xrdp to be ablet to use gui applications on WSL. reference : https://yoshimemo.com/post-723/
build-xrdp:
	apt-get upate && apt-get upgrade -y
	apt-get install -y xrdp xubuntu-desktop
	sed -i -e '/^port/s/3389/13390/g' /etc/xrdp/xrdp.ini
	/etc/init.d/dbus start
	/etc/init.d/xrdp start
	echo xfce4-session > ~/.xsession
	echo "export XDG_SESSION_DESKTOP=xubuntu" > ~/.xsessionrc
	echo "export XDG_DATA_DIRS=/usr/share/xfce4:/usr/share/xubuntu:/usr/local/share:/usr/share:/var/lib/snapd/desktop:/usr/share" >> ~/.xsessionrc
	echo "export XDG_CONFIG_DIRS=/etc/xdg/xdg-xubuntu:/etc/xdg:/etc/xdg" >> ~/.xsessionrc

# Install ubuntu setting tools
build-linux:
	LANG=C xdg-user-dirs-gtk-update  # change directory name to English
	sudo apt-get update && sudo apt-get upgrade -y
	sudo apt install gnome-tweaks dconf-editor ssh -y
	sudo systemctl start sshd
