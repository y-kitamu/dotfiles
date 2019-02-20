;;;;;;;;; Emacs Setting File ;;;;;;;;;

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
(add-to-load-path "elisp" "elpa" "conf")

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


;;; global key map settings
(define-key global-map (kbd "C-m") 'newline-and-indent)      ; 改行して indent する
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines) ; 行を折り返すかを切り換える
(define-key global-map (kbd "C-t") 'other-window)            ; window の切替

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
;; defaultのIndent Style を設定. M-x describe-variable RET c-style-alist RET で詳細表示
;; TODO : setting clang format 
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")))


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


;;;;;;;;; ELPA Package Settings ;;;;;;;;;

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

;;; company-mode (auto-complete より良い？)
(require 'company)
(global-company-mode)                                                   ; activate company for all buffer
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(setq company-selection-warp-around t)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)        ; 次の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)    ; 前の候補を選択
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)  ; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection)       ; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)        ; 各種メジャーモードでも C-M-iで

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

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
;; add-to-list auto-mode-alist を書くとファイルを開いたときのモードが全部 Fundamental になっていしまう
;; web mode setting
(when (require 'web-mode nil t)
  ; web-mode で起動する拡張子
  ;; (add-to-list 'auto-mode-alist '("\\.html\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.css\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ctp\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsp\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.erb\\" . web-mode))
  (defun web-mode-hook ()
    (setq web-mode-mark-indent-offset 2) ; HTML の Indent
    (setq web-mode-css-indent-offset 2)  ; CSS の Indent
    (setq web-mode-code-indent-offset 2) ; JS, PHP, Ruby などの Indent
    (setq web-mode-style-padding 1)      ; <style>内の Indent
    (setq web-mode-script-padding 1))    ; <script>内の Indent
  (add-hook 'web-mode-hook 'web-mode-hook))

;; docker mode setting
(require 'dockerfile-mode nil t)
(require 'docker-compose-mode nil t)
;; (add-to-list 'auto-mode-alist '("Dockerfile\\" . dockerfile-mode))

;; c++ (cuda) mode setting
;; (add-to-list 'auto-mode-alist '("\\.cu\\" . c++-mode))

;; markdown mode setting
;; TODO : markdown-preview-mode not working
;; (add-to-list 'markdown-preview-javascript
;;              "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML") ; add MathJax
;; (add-to-list 'markdown-preview-javascript
;;              '("http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML" . async))


;;; flycheck setting (Syntax check)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode)) ; flycheck-pos-tip-mode


;;; autopep8
(require 'python)
(require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-saveg)


;;; quickrun (run scripts in Emacs)
(define-key global-map (kbd "C-c q") 'quickrun)


;;; magit
(define-key global-map (kbd "C-x g") 'magit-status)


;;; multi-term
(require 'multi-term)
(add-to-list 'term-unbind-key-list "C-t")
;; 色の設定

