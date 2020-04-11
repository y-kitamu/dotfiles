(setq package-selected-packages
      '(;; lsp packages
        lsp-mode     ;; core
        lsp-ui       ;; ui stuff + flycheck support
        lsp-treemacs ;; more ui stuff
        company-lsp  ;; company support
        ccls         ;; ccls support
        helm-lsp     ;; helm support
        lsp-origami  ;; code folding support
        dap-mode     ;; debugger support
        yasnippet    ;; helpers
        ;dap-ui-mode
        ;; major modes not in core
        dockerfile-mode
        docker-compose-mode
	    go-mode
	    typescript-mode
        use-package
        zenburn-theme
        helm-descbinds
        open-junk-file
        ag
        wgrep-ag
        undo-tree
        elscreen
        web-mode
        magit
        multi-term
        yatex
        ein
        gxref
        buffer-move
	    init-loader
        flycheck
	    which-key
        pyvenv
        avy
        ))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			             ("MELPA Stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;;; ELPAなどで自動で追加される設定をcustom.elに書き込む
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

;; use-package settings
;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;;; emacs internal shell path
(add-to-list 'exec-path "~/.local/bin")

;;; inits 以下の設定ファイルを読み込む
(setq init-loader-show-log-after-init 'error-only)
(init-loader-load)

;;; Backup and Auto save setting
;; if you want to recover "init.el" from auto save file, run below command.
;; M-x recover-file RET ~/.emacs.d/init.el RET
(if (not (file-directory-p "~/.emacs.d/backups/"))
    (make-directory "~/.emacs.d/backups/"))
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))              ; backup の保存 directory を設定
(setq auto-save-file-name-transforms
      `((".*",(expand-file-name "~/.emacs.d/backups/") t))) ; autosave の保存 directory を設定
(setq auto-save-timeout 15)                                 ; auto save file を作成するまでの秒間隔
(setq auto-save-interval 60)                                ; auto save file を作成するまでの type 間隔

;;; 変更されたfileの自動更新
(global-auto-revert-mode t)

;;; 再起動時に前回開いていたファイルを開く
(desktop-save-mode 1)

; window の切替
(define-key global-map (kbd "C-t") 'other-window)
(global-set-key (kbd "C-o") (lambda ()
                              (interactive)
                              (other-window -1)))

;; maximize window
(set-frame-parameter nil 'fullscreen 'maximized)

;; buffer が作られるときに frame を 縦に分割しない
(setq-default split-height-threshold 200)

;; set background alpha
(if window-system
    (progn (set-frame-parameter nil 'alpha 80)))
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha : ")
  (set-frame-parameter nil'alpha (cons alpha-num '(90))))

;; hide bars
(tool-bar-mode 0)   ; tool bar を非表示
(menu-bar-mode 0)   ; menu bar を非表示
(if window-system
    (scroll-bar-mode 0) ; scroll bar を非表示
  )

;; standard mode line setting
(column-number-mode t)             ; column 番号も表示
(size-indication-mode t)           ; file size を表示
(setq display-time-day-and-date t) ; 曜日,月,日を表示
(setq display-time-24hr-format t)  ; 24時間表示
(display-time-mode t)

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

;;; insert parenthesis/brackets by pair
(electric-pair-mode 1)
;; ;; 対応する括弧を強調表示
(setq show-paren-delay 0)                    ; 表示するまでの秒数
(show-paren-mode t)                          ; 有効化
(setq show-paren-style 'expression)          ; expression は括弧内も強調表示

;; fileが!#で初まる場合、+x を付けて保存
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p) 

;; emacs lisp mode で Elisp 関数や変数の情報を echo area に表示
(defun elisp-mode-hooks () "lisp-mode-hooks"
       (when (require 'eldoc nil t)
         (setq eldoc-idle-delay 0.2)
         (setq eldoc-echo-area-use-multiline-p t)
         (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;;; ファイル作成時に生成するテンプレートの設定 (autoinsert)
(use-package autoinsert
  :init
  (setq user-full-name "Yusuke Kitamura")
  (setq user-mail-address "ymyk6602@gmail.com")
  :custom
  ;; テンプレートのディレクトリ
  (auto-insert-directory "~/.emacs.d/insert")
  ;; 各ファイルによってテンプレートを切り替える
  (auto-insert-alist (nconc '(("test_.*\.cpp$" . ["test_template.cpp" my-template])
                              ("\\.cpp$" . ["template.cpp" my-template])
                              ("\\.hpp$" . ["template.hpp" my-template])
                              ("\\.py$" . ["template.py" my-template]))))
  :config
  (defvar template-replacements-alists
    '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%file-without-ext%" .
       (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ("%date%" . (lambda () (format-time-string "%Y-%m-%d %H:%M:%S")))
      ("%mail%" . (lambda () (identity user-mail-address)))
      ("%name%" . (lambda () (identity user-full-name)))
      ("%include-guard%" .
       (lambda () (format "%s_HPP__" (upcase (file-name-sans-extension
                                              (file-name-nondirectory buffer-file-name))))))
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
  )

;; gdb
(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gdb-use-separate-io-buffer t)

;;; 矩形編集 (+ C-v, M-v で一番下、一番上まで移動できるようになる)
(cua-mode t)
(setq cua-enable-cua-keys nil)

(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
       (yatex-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Package Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Theme
(load-theme 'zenburn t)
(zenburn-with-color-variables
  (custom-theme-set-faces
    'zenburn
    `(hl-line-face ((t (:background ,zenburn-bg+2 ))))
    `(hl-line ((t (:background ,zenburn-bg+2 ))))
    )
  )
(global-hl-line-mode t)

(use-package vline
  :load-path "./packages"
  )

(use-package col-highlight
  :load-path "./packages"
  :config
  (toggle-highlight-column-when-idle 1)
  (col-highlight-set-interval 1)
  :after vline
  )

;; wgrep setting
(use-package wgrep
  :commands wgrep-ag-setup
  :config 
  (setq wgrep-auto-save-buffer t)
  :hook
  (ag-mode . wgrep-ag-setup)
  )

;; undo tree setting.  C-x u visualize undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

;;; buffer-move setting
(use-package buffer-move
  :bind (("C-c C-l" . buf-move-left)
         ("C-c C-r" . buf-move-right))
  )

;;; Elscreen 
(use-package elscreen
  :config
  (elscreen-start)
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs))
  :bind
  (:map elscreen-map
        ("SPC" . 'elscreen-next)
        ("C-SPC" . 'elscreen-next)
        )
  )

;;; magit
(use-package magit
  :config
  (global-auto-revert-mode t) ; ファイルに変更があった場合に即座に反映する
  (when (equal system-type 'windows-nt) ;; windows の場合、git の .exe file の場所を指定
    (setq magit-git-executable "c:/Users/y-kitamura/AppData/Local/Programs/Git/bin/git.exe"))
  :bind
  ("C-x g" . magit-status)
  )

;;; multi-term
;; .bashrc に $TERM が eterm-color の場合にも color-prompt にするように設定を追記する
(use-package multi-term
  :config
  (add-to-list 'term-unbind-key-list "C-t")
  (add-to-list 'term-unbind-key-list "C-o")
  :hook
  (term-mode . 
   (lambda ()
     (define-key term-raw-map "\C-y" 'term-paste)           ; char-mode でペースト
     (define-key term-raw-map "\C-c\C-j" 'term-line-mode))) ; line-mode へ切り替え
  )

;; automatically update gtags	
;; project root で gtags -v とかで GTAGS, GPATH, GRTAGS を作成する	
(defun c-mode-update-gtags ()	
  (let* ((file (buffer-file-name (current-buffer)))	
     (dir (directory-file-name (file-name-directory file))))	
    (when (executable-find "global")	
      (start-process "gtags-update" nil	
             "global" "-uv"))))	
(add-hook 'after-save-hook	
          'c-mode-update-gtags)
(use-package gxref
  :config
  (add-to-list 'xref-backend-functions 'gxref-xref-backend)
  )

;;; org mode setting
(use-package org
  :custom
  (org-startup-indented t)
  (org-indent-indentation-per-level 2)
  (org-startup-folded 'showall)
  (org-confirm-babel-evaluate nil) ; 評価時に確認メッセージをださない
  (org-directory "~/.emacs.d/junk")
  (org-agenda-files (list org-directory))
  (org-agenda-skip-scheduled-if-done t) ; agenda に DONE を表示しない
  (org-log-done 'time) ; DONE の時間を記録
  :config
  ; python コードブロックを評価できるようにする	
  (org-babel-do-load-languages
   'org-babel-load-languages	
   '((python . t)))
  :bind
  ("C-c a" . 'org-agenda)
  )
;;; open-junk-file setting	
(use-package open-junk-file	
  :config	
  (setq org-archive-location (concat "~/.emacs.d/junk/"	
                                     (format-time-string "%Y_%m_%d" (current-time))	
                                     ".org::")) 	
  (setq open-junk-file-format "~/.emacs.d/junk/%Y_%m_%d.org")	
  :bind	
  ("C-x j" . open-junk-file)	
  ("C-x C-j" . (lambda() (interactive) (find-file "~/.emacs.d/junk/todo.org")))	
  )

;;; python の仮想環境の設定
;;; cd [project_root] && python -m venv [venv name] で仮想環境作成
;;; dotfiles/requirements.txt をインストール
;;; M-x pyvenv-activate [project_root]/[venv name] で activate
;;; project のファイルを開くと、自動で仮想環境の lsp が立ち上がる。
;;; すでにファイルが開いている場合は、 pyvenv-activate のあと、lsp-workspace-restart とする
(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

;;; ein.el setting (emacs で jupyter notebook を使えるようにしたもの)
;;; 参考 : https://pod.hatenablog.com/entry/2017/08/06/220817
(use-package ein
  :config
  (setq ein:worksheet-enable-undo t)
  )

;;;; json をフォーマットする
;;;; sudo apt install jq
(defun jq-format (beg end)
  "Set region from BEG to END where to be formatted."
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;; YaTeX (melpa)
(use-package yatex
  :mode ("\\.tex\\'" . yatex-mode)
  :init
  :config
  (defun delete-indent (&optional arg)
    "delete indent of this line."
    (interactive "*P")
    (beginning-of-line)
    (if arg (forward-line 1))
    (if (eq (preceding-char) ?\n)
        (progn
          (delete-region (point) (1- (point)))
          (newline)
          (fixup-whitespace))))
  (let (
        (prefix "docker run --rm -v $PWD:/workdir paperist/alpine-texlive-ja ")
        (cmds '(
                bibtex-command
                dvi2-command
                makeindex-command
                tex-command
                YaTeX-dvipdf-command
                )))
    (cl-loop for cmd in cmds collect (set cmd (concat prefix (eval cmd)))))
  :bind(:map YaTeX-mode-map
        ("TAB" . 'delete-indent)
        ("RET" . 'newline))
  )

;; M-x align で自動で整形する設定 (align-regexp ではない!)
(defmacro lazyload (func lib docstring &rest body)
  "遅延ロード．funcにオートロードさせたい関数を並べる．
例：\(lazyload \(func1 func2\) \"hogehoge\"\)"
  (declare (indent 3))
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib ,docstring t)) func)
     (eval-after-load ,lib
       `(funcall #',(lambda () ,@body)))))
(defmacro append-to-list (to list)
  " list に append する際に要素を複数指定"
  (declare (indent 1))
  `(setq ,to (append ,list ,to)))

;;; yatex-mode で　table を align
(lazyload (align align-regexp align-newline-and-indent) "align" nil
  (append-to-list align-rules-list
    (list '(yatex-tabular
            (regexp . "\\(\\s-*\\)&")
            (modes . '(yatex-mode))
            (repeat . t))
          '(yatex-tabular2
            (regexp . "\\(\\s-+\\)\\\\\\\\")
            (modes . '(yatex-mode))))))


;; web mode setting
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.gs\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)      
         ("\\.php\\'" . web-mode)      
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.ctp\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.xml\\'" . web-mode))
  :custom
  (web-mode-enable-current-element-highlight t)
  :config
  (defun web-mode-hook ()
    (setq web-mode-markup-indent-offset 2) ; HTML の Indent
    (setq web-mode-css-indent-offset 2)  ; CSS の Indent
    (setq web-mode-code-indent-offset 2) ; JS, PHP, Ruby などの Indent
    (setq web-mode-style-padding 1)      ; <style>内の Indent
    (setq web-mode-script-padding 1))    ; <script>内の Indent
  (add-hook 'web-mode-hook 'web-mode-hook)
  (setq web-mode-content-types-alist
        '(("javascript" . "\\.gs\\'")  ; google app scripts file
          ))
  )


(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("C-:" .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   ("M-g f" . avy-goto-line))
  )


;;; Helm
(use-package helm-config
  :custom
  (helm-completion-style 'emacs)
  :bind
  (("M-y" . helm-show-kill-ring) ; helm-kill-ring への keybind の割当
   ("C-x b" . helm-for-files)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x))
  )

(use-package helm-descbinds
  :config
  (helm-descbinds-mode) ; C-h b (keybind display list) をhelmで表示
  )

(use-package helm-elisp)
(use-package helm-man
  :custom
  (helm-for-document-sources '(helm-source-info-elisp
                               helm-source-info-cl
                               helm-source-info-pages
                               helm-source-man-pages))
  :config
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
  )


;; lsp configuration begin
(use-package lsp-mode
  :custom
  ;; (lsp-log-io t)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-idle-delay 0.500)
  :hook
  ((prog-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-ui
  ;; :config
  ;; (setq-default lsp-ui-sideline-show-hover t)
  :custom
  ;; (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-delay 1.0)
  (lsp-ui-doc-show nil)
  )

(use-package lsp-go)
(use-package lsp-html)

(use-package yasnippet
  :config
  (yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("C-x i i" . yas-insert-snippet) ;; 既存スニペットを挿入
        ("C-x i n" .  'yas-new-snippet) ;; スニペットを作成するバッファを用意
        ("C-x i v" . 'yas-visit-snippet-file) ;; 既存スニペットを閲覧・編集
        )
  (:map yas-keymap
        ("<tab>" . nil)) ;; because of avoiding conflict with company keymap
  :after lsp-mode)

(use-package company
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  :bind
  (("C-M-i" . company-complete)) ; C-M-iで補完
  (:map company-active-map
        ("C-n" . company-select-next) ; 次の候補を選択
        ("C-p" . company-select-previous) ; 前の候補を選択
        ("C-s" . company-filter-candidates)  ; C-sで絞り込む
        ("C-i" . company-complete-selection) 
        ([tab] . company-complete-selection)) ; TABで候補を設定
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init
  (global-company-mode t)
  )
 
(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-lsp-async t)
  (company-lsp-enable-recompletion t)
  :after
  (:all lsp-mode lsp-ui company yasnippet)
  :init
  (push 'company-lsp company-backends)
  )

(use-package ccls
  :custom
  (ccls-initialization-options (list :compilationDatabaseDirectory "build"))
  )
;; lsp configuration end

;; dap-mode setting
;; (dap-mode 1)
;; (dap-ui-mode 1)
;; ;; enables mouse hover support
;; (dap-tooltip-mode 1)
;; ;; use tooltips for mouse hover
;; ;; if it is not enabled `dap-mode' will use the minibuffer.
;; (tooltip-mode 1)
;; (use-package dap-mode
;;   :config
;;   (dap-gdb-lldb-setup)
;;   )
;; (require 'dap-gdb-lldb)
;; ;; (use-package dap-python)
;; ;; (use-package dap-lldb)
