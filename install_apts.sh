#!/bin/bash

# run as root

apt-get update && apt-get upgrade

# nvidia-driver
ubuntu-drivers autoinstall

# docker

# docker-compose

# emacs
apt-get install curl \
            gnupg \
            gpm \
            imagemagick \
            ispell \
            libacl1 \
            libasound2 \
            libcanberra-gtk3-module \
            libdbus-1-3 \
            libgif7 \
            libgnutls30 \
            libgtk-3-0 \
            libjansson4 \
            libjpeg8 \
            liblcms2-2 \
            libm17n-0 \
            libpng16-16 \
            librsvg2-2 \
            libsm6 \
            libtiff5 \
            libx11-xcb1 \
            libxml2 \
            libxpm4 \
            openssh-client \
            texinfo
apt-get install silversearch-ag

git clone https://github.com/emacs-mirror/emacs.git
cd emacs && ./configure && make && make install
