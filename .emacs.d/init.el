;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emacs Setting File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load-path の設定
;; if using emacs23 or before
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; subdirectory も自動で追加する関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add arguments' directory & subdirectory to the load-path
(add-to-load-path "elpa")

;; use-package settings
;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;;; ELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa stable" . "https://melpa.org/packages/"))
(package-initialize)

;;; ELPAなどで自動で追加される設定をcustom.elに書き込む
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;; emacs internal shell path
(add-to-list 'exec-path "~/.local/bin")

;;; 再起動時に前回開いていたファイルを開く
(desktop-save-mode 1)

;;; global key map settings
(define-key global-map (kbd "C-m") 'newline-and-indent)      ; 改行して indent する
;;(define-key global-map (kbd "C-c l") 'toggle-truncate-lines) ; 行を折り返すかを切り換える
(define-key global-map (kbd "C-t") 'other-window)            ; window の切替
(global-set-key (kbd "C-o") (lambda ()
                              (interactive)
                              (other-window -1)))

;;; insert parenthesis/brackets by pair
(electric-pair-mode 1)

;;; Frame settings (見た目の設定)
;; maximize window
(set-frame-parameter nil 'fullscreen 'maximized)

;; set background alpha
(if window-system
    (progn (set-frame-parameter nil 'alpha 80)))
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha : ")
  (set-frame-parameter nil'alpha (cons alpha-num '(90))))

;; hide bars
(tool-bar-mode 0)   ; tool bar を非表示
(scroll-bar-mode 0) ; scroll bar を非表示
(menu-bar-mode 0)   ; menu bar を非表示

;; standard mode line setting
(column-number-mode t)             ; column 番号も表示
(size-indication-mode t)           ; file size を表示
(setq display-time-day-and-date t) ; 曜日,月,日を表示
(setq display-time-24hr-format t)  ; 24時間表示
(display-time-mode t)

;; extensional mode line setting
;; region選択時に行数と文字数を表示する
(defun count-lines-and-chars ()
  (if mark-active
      (format "(%d lines,%d chars) "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'mode-line-format
             '(:eval (count-lines-and-chars)))

;; title bar setting
(setq frame-title-format "%@%f") ; title bar に表示する文字列

;;; Theme
(load-theme 'zenburn t)

;; buffer が作られるときに frame を 縦に分割しない
(setq-default split-height-threshold 200)

;;; Indent settings
(setq-default tab-width 4)          ; default の tab の表示幅
(setq-default indent-tabs-mode nil) ; indent に tab文字を使用しない
(setq-default python-indent-offset 4)
;; defaultのIndent Style を設定. M-x describe-variable RET c-style-alist RET で詳細表示
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style `"stroustrup")))

(c-add-style "briancpp" '((c-offsets-alist
                           (access-label . /)
                           (defun-open . 0)
                           (defun-close . 0)
                           (statement-block-intro . +)
                           (substatement-open . 0)
                           (substatement-label . 0)
                           (label . 0)
                           (statement-cont . +)
                           (inline-open . 0)
                           (inline-close . 0)
                           (inlambda . 0)
                           (innamespace . 0))))
(add-hook 'c++-mode-hook (lambda () 
               (c-set-style "briancpp")))

;;; Highlight settings
;; 現在行の Highlight
(defface my-hl-line-face
  '((((class color) (background dark))  ; 背景がdarkの場合、背景色を紺にする
     (:background "NavyBlue" t))
    (((class color) (background light)) ; 背景がligthの場合、背景色を青にする
     (:backgorund "LightSkyBlue" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; ;; 対応する括弧を強調表示
(setq show-paren-delay 0)                    ; 表示するまでの秒数
(show-paren-mode t)                          ; 有効化
(setq show-paren-style 'expression)          ; expression は括弧内も強調表示


;;; Backup and Auto save setting
;; if you want to recover "init.el" from auto save file, run below command.
;; M-x recover-file RET ~/.emacs.d/init.el RET
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))              ; backup の保存 directory を設定
(setq auto-save-file-name-transforms
      `((".*",(expand-file-name "~/.emacs.d/backups/") t))) ; autosave の保存 directory を設定
(setq auto-save-timeout 15)                                 ; auto save file を作成するまでの秒間隔
(setq auto-save-interval 60)                                ; auto save file を作成するまでの type 間隔


;;; 変更されたfileの自動更新
(global-auto-revert-mode t)


;;; Hook
;; after save hook
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p) ; fileが!#で初まる場合、+x を付けて保存

;; emacs lisp mode hook
;; Elisp 関数や変数の情報を echo area に表示
(defun elisp-mode-hooks () "lisp-mode-hooks"
       (when (require 'eldoc nil t)
         (setq eldoc-idle-delay 0.2)
         (setq eldoc-echo-area-use-multiline-p t)
         (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)


;;; ファイル作成時に生成するテンプレートの設定 (autoinsert)
;; autoinsert
;; http://ymotongpoo.hatenablog.com/entry/2012/12/02/190248
(require 'autoinsert)
(setq user-full-name "Yusuke Kitamura")
(setq user-mail-address "ymyk6602@gmail.com")

;; テンプレートのディレクトリ
(setq auto-insert-directory "~/.emacs.d/insert")

;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.hpp$" . ["template.hpp" my-template])
               ("\\.py$" . ["template.py" my-template])
               ) auto-insert-alist))
(require 'cl)

(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%date%" . (lambda () (format-time-string "%Y-%m-%d %H:%M:%S")))
    ("%mail%" . (lambda () (identity user-mail-address)))
    ("%name%" . (lambda () (identity user-full-name)))
    ("%include-guard%"    . (lambda () (format "%s_HPP__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))
))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ELPA Package Settings  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; インストールしたパッケージの設定とか keybind とか ;;;;



;;; Helm
(require 'helm-config)                                   ; activate helm

;; helm-c-occur (検索機能) setting
;; TODO : Not working 
(when (require 'helm-c-moccur nil t)
  (setq
   helm-idle-delay 0.1
   helm-c-moccur-helm-idle-delay 0.1
   helm-c-occur-higligt-info-line-flag t
   helm-c-moccur-enable-auto-look-flag t
   helm-c-moccur-enable-initial-pattern t)
  (define-key global-map (kbd "C-M-o") 'helm-c-moccur-occur-by-moccur))

;; helm keybind settings
(helm-descbinds-mode)                                    ; C-h b (keybind display list) をhelmで表示
(define-key global-map (kbd "M-y") 'helm-show-kill-ring) ; helm-kill-ring への keybind の割当
(define-key global-map (kbd "C-x b") 'helm-for-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x") 'helm-M-x)

;; helm-gtags
(custom-set-variables
 '(helm-gtagssuggested-keymapping t)
 '(helm-gtags-auto-update t))
(defun helm-gtags-hook ()
  (local-set-key (kbd "M-t") 'helm-gtags-find-tag)    ; 入力したタグの定義元へジャンプ
  (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)   ; 入力タグを参照する場所へジャンプ
  (local-set-key (kbd "M-s") 'helm-gtags-find-symbol) ; 入力したシンボルを参照する場所へジャンプ
  (local-set-key (kbd "M-p") 'helm-gtags-pop-stack))  ; ジャンプ前の場所へ戻る
(add-hook 'c++-mode-hook 'helm-gtags-hook)
(add-hook 'python-mode-hook 'helm-gtags-hook)

;; helm-man
(require 'helm-elisp)
(require 'helm-man)
(setq helm-for-document-sources
      '(helm-source-info-elisp
        helm-source-info-cl
        helm-source-info-pages
        helm-source-man-pages))
(defun helm-for-document ()
  "Preconfigured `helm' for helm-for-document."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (nconc
           (mapcar (lambda (func)
                           (funcall func default))
                   helm-apropos-function-list)
           helm-for-document-sources)
          :buffer "*helm for document*")))


;;; auto-complete
;; (when (require 'auto-complete-config nil t)
;;   ;(define-key ac-mode-map (kbd "TAB") 'auto-complete)
;;   (ac-config-default)
;;   (setq ac-use-menu-map t)
;;   (setq ac-ignore-case nil))

;;;; 補完機能 company + irony
;;; company-mode (auto-complete より良い？)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)                 ; activate company for all buffer
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)        ; 次の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)    ; 前の候補を選択
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)  ; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection)       ; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)        ; 各種メジャーモードでも C-M-iで

;;; c++ の補完設定 (irony)
;;; (cmake で -DCMAKE_EXPORT_COMPILE_COMMANDS=1 としてやって compile_commands.json を作成する?)
;;; 初回のみ M-x irony-install-server RET でサーバーをインストールする
;;; apt で clang 系のライブラリ（全部?） 入れる必要あり
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'irony
  '(progn
     (custom-set-variables '(irony-additional-clang-options '("-std=c++1z")))
     (add-to-list 'company-backends 'company-irony)
     (add-to-list 'company-backends 'company-gtags)
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (add-hook 'c-mode-common-hook 'irony-mode)))

;;; python の補完設定 (company-jedi)
;;; 初回のみ M-x jedi:install-server RET でサーバーをインストールする
(require 'jedi-core)
(setq jedi:complete-on-dot t) ; . を入力したときにも（メソッドを）補完
(setq jedi:use-shortcuts t)  ; activate keybind (M-. : go to definition,  M-, : back from definition)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi) ; backendに追加

;;; flycheck setting (Syntax check)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode)) ; flycheck-pos-tip-mode
(eval-after-load "flycheck"
  '(progn
     (when (locate-library "flycheck-irony")
       (flycheck-irony-setup))))

;;; flycheck setting (Syntax check)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)) ; flycheck-pos-tip-mode
;;
(setq elpy-rpc-python-command "python3")

;;; autopep8
(require 'python)
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;; extension of Search and Replace
;; moccur setting
(when (require 'color-moccur nil t)
  (define-key global-map (kbd "M-o") 'occur-by-moccur) ; keybind setting
  (setq moccur-split-word t)                           ; space で AND 検索
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")  ; directory検索するときに除外するfile
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

(require 'moccur-edit nil t)
(defadvice moccur-edit-change-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))

;; wgrep setting
(require 'wgrep nil t)


;;; history setting
;; undo tree setting.  C-x u visualize undo tree
(when (require 'undo-tree nil t) 
  (global-undo-tree-mode))


;;; window management
;; Elscreen
(when (require 'elscreen nil t)
  (elscreen-start)
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs))
  (define-key elscreen-map (kbd "SPC") 'elscreen-next)
  (define-key elscreen-map (kbd "C-SPC") 'elscreen-next))


;;; 矩形編集
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;; mode setting
;; web mode setting
(when (require 'web-mode nil t)
  ; web-mode で起動する拡張子
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.gs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
  (defun web-mode-hook ()
    (setq web-mode-markup-indent-offset 2) ; HTML の Indent
    (setq web-mode-css-indent-offset 2)  ; CSS の Indent
    (setq web-mode-code-indent-offset 2) ; JS, PHP, Ruby などの Indent
    (setq web-mode-style-padding 1)      ; <style>内の Indent
    (setq web-mode-script-padding 1))    ; <script>内の Indent
  (add-hook 'web-mode-hook 'web-mode-hook))
(setq web-mode-content-types-alist
      '(("javascript" . "\\.gs\\'")  ; google app scripts file
        ))

;; docker mode setting
(require 'dockerfile-mode nil t)
(require 'docker-compose-mode nil t)

;; c++ (cuda) mode setting
;; (add-to-list 'auto-mode-alist '("\\.cu\\" . c++-mode))

;; markdown mode setting
;; TODO : markdown-preview-mode not working
;; (add-to-list 'markdown-preview-javascript
;;              "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML") ; add MathJax
;; (add-to-list 'markdown-preview-javascript
;;              '("http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML" . async))


;;; quickrun (run scripts in Emacs)
(define-key global-map (kbd "C-c q") 'quickrun)


;;; magit
(define-key global-map (kbd "C-x g") 'magit-status)
(global-auto-revert-mode t) ; ファイルに変更があった場合に即座に反映する
;; windows の場合、git の .exe file の場所を指定
(when (equal system-type 'windows-nt)
  (setq magit-git-executable "c:/Users/y-kitamura/AppData/Local/Programs/Git/bin/git.exe"))


;;; multi-term
;; .bashrc に $TERM が eterm-color の場合にも color-prompt にするように設定を追記する
(require 'multi-term)
(add-to-list 'term-unbind-key-list "C-t")
(add-to-list 'term-unbind-key-list "C-o")
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map "\C-y" 'term-paste)           ; char-mode でペースト
            (define-key term-raw-map "\C-c\C-j" 'term-line-mode))) ; line-mode へ切り替え


;;; buffer-move setting
(require 'buffer-move nil t)
(define-key global-map (kbd "C-c C-l") 'buf-move-left)
(define-key global-map (kbd "C-c C-r") 'buf-move-right)


;;; open-junk-file setting
;;; org メモを使うための設定
(require 'open-junk-file nil t)
(setq org-archive-location (concat "~/.emacs.d/junk/"
                                   (format-time-string "%Y_%m_%d" (current-time))
                                   ".org::"))
(setq open-junk-file-format "~/.emacs.d/junk/%Y_%m_%d.org")
(define-key global-map (kbd "C-x j") 'open-junk-file)
(define-key global-map (kbd "C-x C-j") (lambda() (interactive) (find-file "~/.emacs.d/junk/todo.org")))
;;; org mode setting
(setq org-confirm-babel-evaluate nil) ; 評価時に確認メッセージをださない
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))                     ; python コードブロックを評価できるようにする

;;; ein setting (emacs で jupyter notebook を使えるようにしたもの)
;;; 参考 : https://pod.hatenablog.com/entry/2017/08/06/220817
(require 'ein)
(setq ein:worksheet-enable-undo t)

;;; glsl-mode の "Major mode is unknown to Irony, see `irony-supported-major-modes'" に対処
(push 'glsl-mode irony-supported-major-modes)


;;;; json をフォーマットする
;;;; sudo apt install jq
(defun jq-format (beg end)
  "Set region from BEG to END where to be formatted."
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;; align config
;;; not working
;; (require 'align)
;; (add-to-list 'align-rules-list
;;              '(camma-assignment
;;                (regexp . ",\\( *\\)")
;;                (repeat . t)))              ; 複数回適用を有効に

;;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippets"
        ))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(yas-global-mode 1)

;;
;; YaTeX (melpa)
;;
(use-package yatex                                                                                   
  :mode ("\\.tex\\'" . yatex-mode)
  :config
  ;; (setq YaTeX-electric-indent-mode t) ; 改行時の自動インデントの設定
  (let (
        (prefix "docker run --rm -v $PWD:/workdir paperist/alpine-texlive-ja ")
        (cmds '(
                bibtex-command
                dvi2-command
                makeindex-command
                tex-command
                YaTeX-dvipdf-command
                )))
    (cl-loop for cmd in cmds collect (set cmd (concat prefix (eval cmd))))))
