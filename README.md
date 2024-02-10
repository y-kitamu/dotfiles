# My dotfiles

## Usage

- setup WSL
```shell
make build-wsl # must be run as user (not root)
```

- build emacs
```shell
make build-emacs # must be run as user (not root)
```

- deploy setting files
```shell
./install.sh
```

- enable custom keyboard mapping
```shell
./install_xkbmap.sh
```

- enable custom ubuntu shortcuts
```shell
./install_ubuntu_shortcuts.sh
```
## Windows setup

### installするもの
- Bash for windows
- VSCode
- fakeymacs

### Keyboard layoutの変更
- レジストリエディタで`コンピューター\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout`に新しいScancode Mapを追加する
- keyhac + fakeymacsでemacs化
