#!/bin/bash

gsettings set org.gnome.desktop.wm.keybindings maximize-vertically "['<Primary>asciicircum']"
gsettings set org.gnome.desktop.wm.keybindings maximize "['<Primary><Alt>asciicircum']"

gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1 "['<Primary><Shift>exclam']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-2 "['<Primary><Shift>quotedbl']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-3 "['<Primary><Shift>numbersign']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-4 "['<Primary><Shift>dollar']"

gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Primary>1']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Primary>2']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Primary>3']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Primary>4']"

gsettings set org.gnome.desktop.wm.keybindings show-desktop "@as []"


gsettings set org.gnome.settings-daemon.plugins.media-keys www "['<Primary><Alt>w']"

gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ name 'emacs'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ command 'emacs'
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ binding '<Primary><Alt>e'
