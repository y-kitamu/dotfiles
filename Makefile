EMACS ?= emacs
ROOT_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# .PHONY: emacs-build
# emacs-build:

.PHONY: emacs-test
emacs-test:
	@$(EMACS) --batch --eval '(load "$(ROOT_DIR)/.emacs.d/init.el")'
