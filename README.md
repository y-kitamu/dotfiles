# インストールメモ (dell inspiron 7380, 2019/3)


- ホームのディレクトリ名を英語に
```
LANG=C xdg-user-dirs-gtk-update
```

- tweaks のインストール
```
sudo apt install gnome-tweaks
```
    - keyboar の設定 (capslock -> ctrl)
    - デスクトップにアイコンを表示しない
    - emacs の keybinding 有効化
    
- xkb の設定 (keyboard layout)


- sshd のインストール
```
sudo apt-get install ssh
systemctl start sshd
```

- 各種アプリの設定移行 : ssh でコピペ
  - vivaldi : ~/.config/vivaldi 
  - thunderbird : ~/.thunderbird
  - dconf-editor : ~/.config/dconf

- emacs install
emacs 26.1
```
sudo apt install libgtk-3-dev libtiff5-dev libgif-dev libpng-dev libxpm-dev libncurses5-dev
sudo apt install libgnutls28-dev
wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-26.1.tar.gz
tar -zxvf emacs-26.1.tar.gz
cd emacs-26.1/
./configure
make
sudo make install
```

- dconf-editor
```
sudo apt-get install dconf-editor
```

- ubuntu ショートカット設定
  - workspace
  - 縦方向拡大
