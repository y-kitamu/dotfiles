# インストールメモ (dell inspiron 7380, 2019/3)


- ホームのディレクトリ名を英語に
```
LANG=C xdg-user-dirs-gtk-update
```

- tweaks のインストール
    - keyboar の設定 (capslock -> ctrl)
    - デスクトップにアイコンを表示しない
    - emacs の keybinding 有効化
```
sudo apt install gnome-tweaks
```
    

- sshd のインストール
```
sudo apt-get install ssh
systemctl start sshd
```

- 各種アプリの設定移行 : ssh でコピペ
  - vivaldi : ~/.config/vivaldi 
  - thunderbird : ~/.thunderbird
  - dconf-editor : ~/.config/dconf  ??? 必要 ???

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
    https://qiita.com/uchan_nos/items/a2485b51f5f3fb0db8f8
    - 入力メソッド Mozc 以外の削除
    - gconf-editorで /org/gnome/settings-daemon/plugins/keyboard/active をfalse 
    - gnome-session-properties で自動起動に load_xkbmap.sh を登録

- Ubuntu の Dock をデフォルトで非表示にする
```
gsettings set org.gnome.shell.extensions.dash-to-dock autohide false && gsettings set org.gnome.shell.extensions.dash-to-dock dock-fixed false && gsettings set org.gnome.shell.extensions.dash-to-dock intellihide false
```

- nvidia-driver ・ nvidia-docker のインストール
```bash
sudo ubuntu-drivers autoinstall
curl -s -L https://nvidia.github.io/nvidia-container-runtime/gpgkey |  sudo apt-key add -
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
curl -s -L https://nvidia.github.io/nvidia-container-runtime/$distribution/nvidia-container-runtime.list |  sudo tee /etc/apt/sources.list.d/nvidia-container-runtime.list
sudo apt-get update
apt-get install nvidia-container-runtime
```
