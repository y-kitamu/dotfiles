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
    

- sshd のインストール
```
sudo apt-get install ssh
systemctl start sshd
```

- 各種アプリの設定移行 : ssh でコピペ
  - vivaldi : ~/.config/vivaldi 
  - thunderbird : ~/.thunderbird
  - dconf-editor : ~/.config/dconf  ??? 必要 ???
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

- redshift 
  ubuntu software center からインストール。 ~/.config/redshift.conf に設定

- screen
```
sudo apt-get install screen
```

- Docker, Docker-compose
  公式参考に

- Dropbox

- xkb の設定 (keyboard layout)
    - 入力メソッド Mozc 以外の削除
    - gconf-editorで /org/gnome/settings-daemon/plugins/keyboard/active をfalse 
    - gnome-session-properties で自動起動に load_xkbmap.sh を登録
    https://qiita.com/uchan_nos/items/a2485b51f5f3fb0db8f8
  
  
