ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
OS := $(shell uname)

EMACS ?= emacs
WAKATIME_VERSION := wakatime-cli-linux-amd64
PKG_MANAGER := sudo apt-get install -y

ifeq ($(OS), Darwin)
WAKATIME_VERSION := wakatime-cli-darwin-amd64
PKG_MANAGER := brew install
endif

# .PHONY: emacs-build
# emacs-build:

.PHONY: emacs-test
emacs-test: install-emacs-deps
	@$(EMACS) --batch --eval '(load "$(ROOT_DIR)/.emacs.d/init.el")'

## WSL settings
build-wsl:  build-emacs build-xrdp

# Install ubuntu setting tools
build-linux: build-emacs
	LANG=C xdg-user-dirs-gtk-update  # change directory name to English
	sudo apt-get update && sudo apt-get upgrade -y
	sudo apt install gnome-tweaks dconf-editor ssh -y
	sudo systemctl start sshd
	sudo add-apt-repository ppa:git-core/ppa -y  # Install latest git
	sudo apt-get update; sudo apt-get install git -y

# Build emacs-ng
build-emacs:
	sudo apt-get update && sudo apt-get upgrade -y
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > rustup.sh
	sh ./rustup.sh -y && rm rustup.sh
	sudo apt install -y build-essential automake clang libclang-dev
	sudo apt install -y texinfo libjpeg-dev libtiff-dev \
		libgif-dev libxpm-dev libgtk-3-dev gnutls-dev \
		libncurses5-dev libxml2-dev libxt-dev
	mkdir $${HOME}/packages; git clone https://github.com/emacs-ng/emacs-ng.git $${HOME}/packages || true
	cd $${HOME}/packages && \
		./autogen.sh &&\
		./configure &&\
		make -j$(nproc) &&\
		sudo make install

install-emacs-deps: install-wakatime
	$(PKG_MANAGER) cmake libtool libtool-bin

install-wakatime:
	cd $(ROOT_DIR)/.emacs.d/
	wget https://github.com/wakatime/wakatime-cli/releases/download/v1.49.0/$(WAKATIME_VERSION).zip
	unzip $(WAKATIME_VERSION).zip && rm $(WAKATIME_VERSION).zip
	mkdir -p ~/.local/bin; mv $(WAKATIME_VERSION) ~/.local/bin/

# Install xrdp to be ablet to use gui applications on WSL. reference : https://yoshimemo.com/post-723/
build-xrdp:
	sudo apt-get update && sudo apt-get upgrade -y
	sudo apt-get install -y xrdp xubuntu-desktop
	sudo sed -i -e '/^port/s/3389/13390/g' /etc/xrdp/xrdp.ini
	sudo /etc/init.d/dbus start
	sudo /etc/init.d/xrdp start
	echo xfce4-session > ~/.xsession
	echo "export XDG_SESSION_DESKTOP=xubuntu" > ~/.xsessionrc
	echo "export XDG_DATA_DIRS=/usr/share/xfce4:/usr/share/xubuntu:/usr/local/share:/usr/share:/var/lib/snapd/desktop:/usr/share" >> ~/.xsessionrc
	echo "export XDG_CONFIG_DIRS=/etc/xdg/xdg-xubuntu:/etc/xdg:/etc/xdg" >> ~/.xsessionrc
