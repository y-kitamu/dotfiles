#!/bin/bash

sudo apt install build-essential automake clang libclang-dev
sudo apt install texinfo libjpeg-dev libtiff-dev \
libgif-dev libxpm-dev libgtk-3-dev gnutls-dev \
libncurses5-dev libxml2-dev libxt-dev

git clone https://github.com/emacs-ng/emacs-ng.git

cd emacs-nt
