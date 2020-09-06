#!/bin/bash
sudo alsactl init
sudo apt install --reinstall alsa-base pulseaudio
mv ~/.config/pulse ~/.config/pulse.bk
sudo reboot
