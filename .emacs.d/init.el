;;; init.el --- emacs configuration file    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords: settings

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
;;
(setq package-enable-at-startup nil)

;;; straight.el setting
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq straight-use-package-by-default t)

;;; install use-package
(straight-use-package 'use-package)

;;; ELPAなどで自動で追加される設定をcustom.elに書き込む
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; これがないと use-package の Error (Symbol’s value as variable is void: personal-keybindings)が発生
;; (use-package bind-key
;;   :config
;;   (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

;;; enable my utility functions
(use-package yk-util
  :straight (yk-util :type git :host github :repo "y-kitamu/yk_elisp"))

;;; Increase a bit garbage collection threshold:
(setq gc-cons-threshold 3200000)

;;; Make sure we can debug init errors more easily:
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;; emacs internal shell path
(when (equal system-type 'gnu/linux)
  (yk/add-to-list-multiple 'exec-path (list (expand-file-name "~/.local/bin")
                                            (expand-file-name "~/.cargo/bin"))))
(when (equal system-type 'windows-nt)
  ;; windows上でhelm-locate を使うためにはscoop経由でEverythingをインストール、
  ;; さらに、es.exeをダウンロードして~/../../scoop/shims/以下にes.exeを配置する。
  ;; es.exe : https://www.voidtools.com
  (add-to-list 'exec-path (expand-file-name "~/../../scoop/shims/")))

;;; inits 以下の設定ファイルを読み込む
(use-package init-loader)
(if (not (file-directory-p "~/.emacs.d/inits/"))
    (make-directory "~/.emacs.d/inits/"))
(setq init-loader-show-log-after-init 'error-only)
(let ((inits-dir (format "%s/inits" (file-name-directory load-file-name))))
  (init-loader-load inits-dir))

;;; encoding
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8-unix) ; nil

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
(use-package clang-format+
  :straight t
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

;; enable Semantic mode (helm-semantic-or-imenu)
(add-hook 'prog-mode (lambda ()
                       (semantic-mode 1)
                       (imenu--rescan-item)))

;; (use-package smartparens
;;   :straight t
;;   :commands smartparens-mode
;;   :hook (prog-mode . smartparens-mode))
;;; insert parenthesis/brackets by pair
(electric-pair-mode 1)
;; ;; ;; 対応する括弧を強調表示
(setq show-paren-delay 0)                    ; 表示するまでの秒数
(show-paren-mode t)                          ; 有効化
(setq show-paren-style 'expression)          ; expression は括弧内も強調表示
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 行末の white space を削除して保存
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; markdown mode のときは無効
(defvar delete-trailing-whitespece-before-save t)
(make-variable-buffer-local 'delete-trailing-whitespece-before-save)
(advice-add 'delete-trailing-whitespace :before-while
            (lambda () delete-trailing-whitespece-before-save))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))

;; fileが!#で初まる場合、+x を付けて保存
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; goto matching parenthesis, Vi style. The last statement is a bit conked;
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise do nothing"
  (interactive "p")
  (cond
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
   (t (digit-argument (or arg 1)))))

;; built-in packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquity-min-dir-content 2)

(require 'abbrev)
(my/hide-minor-mode-from-mode-line 'abbrev-mode)

;; gdb
(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gdb-use-separate-io-buffer t)

;;; 矩形編集 (+ C-v, M-v で一番下、一番上まで移動できるようになる)
(cua-mode t)
(setq cua-enable-cua-keys nil)

(use-package flyspell
  :custom
  (flyspell-mode-line-string nil)
  :hook
  ((prog-mode . flyspell-prog-mode)
       (yatex-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (text-mode . flyspell-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Package Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag
  :straight t
  :config
  (setq ag-arguments (append '("--follow" "--all-types") ag-arguments)))

(use-package wgrep-ag :straight t)

;; wgrep setting
(use-package wgrep
  :straight t
  :commands wgrep-ag-setup
  :config
  (setq wgrep-auto-save-buffer t)
  :hook
  (ag-mode . wgrep-ag-setup)
  :after wgrep-ag)

;; undo tree setting.  C-x u visualize undo tree
(use-package undo-tree
  :straight t
  :custom
  (undo-tree-mode-lighter nil)
  :config
  (global-undo-tree-mode))

;;; buffer-move setting
(use-package buffer-move
  :straight t
  :bind (("C-c C-l" . buf-move-left)
         ("C-c C-r" . buf-move-right)))

;;; fast screen
(use-package fast-scroll
  :straight t
  :demand t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1)
  (my/hide-minor-mode-from-mode-line 'fast-scroll-mode))

;;; chrome の text area を emacs で編集する
(use-package edit-server
  :straight t)

(defun term-send-ctrl-z ()
  "Send ctrl-z in term mode."
  (interactive)
  (term-send-raw-string ""))

(defun term-send-ctrl-x ()
  "Send ctrl-z in term mode."
  (interactive)
  (term-send-raw-string ""))

;;; multi-term
;; .bashrc に $TERM が eterm-color の場合にも color-prompt にするように設定を追記する
(use-package multi-term
  :straight t
  :config
  (add-to-list 'term-unbind-key-list "C-t")
  (add-to-list 'term-unbind-key-list "C-o")
  (add-to-list 'term-bind-key-alist '("C-c z" . term-send-ctrl-z))
  (add-to-list 'term-bind-key-alist '("C-c x" . term-send-ctrl-x))
  :hook
  (term-mode .
   (lambda ()
     (define-key term-raw-map "\C-y" 'term-paste)           ; char-mode でペースト
     (define-key term-raw-map "\C-c\C-j" 'term-line-mode))))  ; line-mode へ切り替え

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
  :straight t
  :config
  (add-to-list 'xref-backend-functions 'gxref-xref-backend))


(use-package eldoc
  :straight t
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-minor-mode-string "")
  :hook (prog-mode . eldoc-mode))

(find-function-setup-keys)

;;; ein.el setting (emacs で jupyter notebook を使えるようにしたもの)
;;; 参考 : https://pod.hatenablog.com/entry/2017/08/06/220817
(use-package ein
  :straight t
  :config
  (setq ein:worksheet-enable-undo t)
  (custom-set-faces
   '(ein:basecell-input-area-face ((t (:background "black" :underline "green yellow" :))))))

;;;; json をフォーマットする
;;;; sudo apt install jq
(defun jq-format (beg end)
  "Set region from BEG to END where to be formatted."
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;; PDF tool
(when (equal system-type 'gnu/linux)
  (use-package pdf-tools
    :straight t
    :init
    (pdf-tools-install t)))

;;; YaTeX (melpa)
(use-package yatex
  :straight t
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
  (let ((prefix "docker run --rm -v $PWD:/workdir paperist/alpine-texlive-ja ")
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
        ("RET" . 'newline)))

;; M-x align で自動で整形する設定 (align-regexp ではない)
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

;;; yatex-mode で　table を alignする
(lazyload (align align-regexp align-newline-and-indent) "align" nil
  (append-to-list align-rules-list
    (list '(yatex-tabular
            (regexp . "\\(\\s-*\\)&")
            (modes . '(yatex-mode))
            (repeat . t))
          '(yatex-tabular2
            (regexp . "\\(\\s-+\\)\\\\\\\\")
            (modes . '(yatex-mode))))))

;;; カラーコードの色をbackgroundに表示する
(use-package rainbow-mode
  :straight t
  :config
  (my/hide-minor-mode-from-mode-line 'rainbow-mode)
  :hook
  (emacs-lisp-mode . rainbow-mode))

(use-package mozc
  ;; sudo apt-get install emacs-mozc-bin
  :straight t
  :custom
  (default-input-method "japanese-mozc"))

;; migemo (日本語のローマ字検索。とりあえずlinuxだけ)
;; sudo apt-get instal -y cmigemo
(when (equal system-type 'gnu/linux)
  (use-package migemo
    :straight t
    :config
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (if (file-exists-p migemo-dictionary)
        (progn
          (setq migemo-user-dictionary nil)
          (setq migemo-coding-system 'utf-8-unix)
          (load-library "migemo")
          (migemo-init)))))

(use-package google-translate
  :straight t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :bind
  (("C-c g" . google-translate-at-point)))
(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))

(use-package avy
  :straight t
  :config
  (avy-setup-default)
  :custom
  (avy-background t)
  :bind
  (("C-:" .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   ("M-g f" . avy-goto-line)))

;;; Helm
(use-package helm
  :straight t
  :custom
  (helm-completion-style 'emacs)
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
  :bind
  (("M-y" . helm-show-kill-ring) ; helm-kill-ring への keybind の割当
   ("C-x b" . helm-for-files)
   ("C-x C-f" . helm-find-files)
   ("C-h a" . helm-apropos)
   ("M-x" . helm-M-x)))

(use-package helm-descbinds
  :straight t
  :config
  ; C-h b (keybind display list) をhelmで表示
  (helm-descbinds-mode))

(use-package helm-tramp
  :straight t
  :custom
  (tramp-default-method "ssh")
  :bind
  (("C-c s" . helm-tramp))
  :config
  (defun helm-tramp-open (path)
    "Tramp open with PATH."
    (helm-find-files-1 path)))

(use-package helm-ag :straight t)

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package hydra :straight t)

(use-package yapfify
  :straight t
  :config
  (setcar (cdr (assq 'yapf-mode minor-mode-alist)) nil)
  :hook
  (python-mode . yapf-mode))

(defun around-yapfify-call-bin (original-func input-buffer output-buffer start-line end-line)
  "Support docker command."
  (let ((command-args (split-string yapfify-executable)))
    (if (= (length command-args) 1)
        (apply original-func (list input-buffer output-buffer start-line end-line))
      (with-current-buffer input-buffer
        (setq res (apply 'call-process-region
                (append (list (point-min) (point-max) (car command-args) nil output-buffer nil)
                        (cdr command-args)
                        (list "-l" (concat (number-to-string start-line) "-"
                                           (number-to-string end-line))))))))))
(advice-add 'yapfify-call-bin :around 'around-yapfify-call-bin)

(defun around-yapfify-region (original-func &rest args)
  "Wrap `yapfify-region` to catch error and make sure to kill *yapfify* buffer"
     (-if-let (tmp-buffer (get-buffer "*yapfify*"))
         (kill-buffer tmp-buffer))
     (apply original-func args))
(advice-add 'yapfify-region :around 'around-yapfify-region)

(use-package blackify
  :straight (blackify :type git :host github :repo "y-kitamu/blackify"))

(use-package py-isort
  :demand t
  :hook
  (before-save . py-isort-before-save))

(use-package flycheck
  :straight t
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15)))
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 0.50)
  (setq-default flycheck-flake8-maximum-line-length 105)
  :config
  ;; (push 'rustic-clippy flycheck-checkers)
  :bind
  (("C-c f" . flycheck-list-errors))
  :hook
  (after-init . global-flycheck-mode))


(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package which-key
  :straight t
  :custom
  (which-key-lighter nil)
  :config
  (which-key-mode))

(use-package package-lint
  :straight t)

;; lsp configuration begin
(use-package lsp-mode
  :straight t
  :custom
  (lsp-log-io nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-idle-delay 0.50)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 2000)
  (lsp-enable-xref t)
  ;; python
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  ;; rust
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :config
  (custom-set-faces
   '(lsp-face-highlight-read
     ((t (:background "#F0DFAF" :foreground "#000000" :weight bold)))) ; zenburn-yellow
   '(lsp-face-highlight-write
     ((t (:background "#DFAF8F" :foreground "#000000" :weight bold))))) ; zenburn-orange
  ;; modeline の表示をなくす
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil)
  (setq warning-minimum-log-level :error)
  ;; lspの探索から除外するdirectory
  (yk/add-to-list-multiple 'lsp-file-watch-ignored-directories
                           '("[/\\\\]\\.cache"
                             "[/\\\\]build"
                             "[/\\\\]edk2"
                             "[/\\\\]__pycache__"
                             "[/\\\\]\\.ccls"))
  (yk/add-to-list-multiple 'lsp-file-watch-ignored-files
                           '("[/\\\\][^/\\\\]+\\.o"
                             "[/\\\\][^/\\\\]+\\.a"
                             "[/\\\\]\\.[/\\\\]+"))
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (python-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)))

(use-package helm-lsp
  :straight t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  :bind
  (("C-c h a" . helm-lsp-code-actions))
  (("C-c h s" . helm-lsp-workspace-symbol))
  (("C-c h d" . helm-lsp-diagnostics)))

(use-package lsp-docker+
  :straight (lsp-docker+ :type git :host github :repo "y-kitamu/emacs-lsp-docker-plus")
  :init
  (lsp-docker+-enable)
  ;; register default lsp server
  (let ((lsp-docker+-image-id "arumatik/common-language-servers")
        (lsp-docker+-client-configs
         (list
          (list :server-id 'bash-ls :docker-server-id 'bashls-docker
                :server-command "bash-langauge-server start")
          (list :server-id 'css-ls :docker-server-id 'cssls-docker
                :server-command "css-languageserver --stdio")
          (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
          (list :server-id 'html-ls :docker-server-id 'htmlls-docker
                :server-command "html-languageserver --stdio")
          (list :server-id 'ts-ls :docker-server-id 'tsls-docker
                :server-command "typescript-language-server --stdio"))))
    (lsp-docker+-init-clients :client-configs lsp-docker+-client-configs))
  (message (lsp-docker+-format "Finish nitializing lsp-docker+"))
  :after lsp-mode)

(use-package lsp-ui
  :straight t
  :init
  ;; lsp-ui-doc
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-doc-use-childframe t)
  (defun disable-tab-bar-in-lsp-ui-doc-frame (frame window)
    "lsp-ui-docで作られるchild frameにtab-barを表示しない"
    (set-frame-parameter frame 'tab-bar-lines 0))
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-hover nil)
  :hook
  (lsp-ui-doc-frame . disable-tab-bar-in-lsp-ui-doc-frame)
  :bind
  (("s-l f" . lsp-ui-doc-focus-frame)
   ("s-l u" . lsp-ui-doc-unfocus-frame)))

;; (use-package lsp-go :straight t)
;; (use-package lsp-html :straight t)
;; (use-package lsp-csharp :straight t)
;; (use-package lsp-rust
;;   :straight t)
(use-package lsp-pyright :straight t)

(use-package ccls
  :straight t
  :custom
  (ccls-initialization-options (list :compilationDatabaseDirectory "build")))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode)
  (my/hide-minor-mode-from-mode-line 'yas-minor-mode)
  :diminish yas-minor-mode
  :bind
  (:map yas-minor-mode-map
        ("C-c y i" . yas-insert-snippet) ;; 既存スニペットを挿入
        ("C-c y n" . yas-new-snippet) ;; スニペットを作成するバッファを用意
        ("C-c y v" . yas-visit-snippet-file) ;; 既存スニペットを閲覧・編集
        )
  (:map yas-keymap
        ("<tab>" . nil)) ;; because of avoiding conflict with company keymap
  :after lsp-mode)

(use-package company
  :straight t
  :demand t
  :init
  (setq company-lighter-base nil)
  (setq company-lighter nil)
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 3)
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
  :hook
  (emacs-lisp-mode . company-mode))

;; lsp configuration end

(use-package company-tabnine
  :straight t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (setq company-tabnine--disabled t)
  (defun toggle-tabnine ()
    "tabnineのenable, disableの切り替え"
    (interactive)
    (cond (company-tabnine--disabled
           (setq company-tabnine--disabled nil)
           (message "TabNine enabled"))
          (t
           (setq company-tabnine--disabled t)
           (message "TabNine disabled")))))

;; dap-mode setting
(use-package dap-mode
  :straight t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	     :gdbpath "rust-lldb"
         :target nil
         :cwd nil))

  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil
    nil
    (make-sparse-keymap)
    ;; (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
    ;; The following code adds to the dap-terminated-hook
    ;; so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook '+dap-running-session-mode)

  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook '+dap-running-session-mode)

  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1)))))

(use-package srefactor
  :straight t
  :config
  (require 'srefactor-lisp)
  (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
  (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
  (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
  (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer))

;;; ファイル作成時に生成するテンプレートの設定 (autoinsert)
;;; init.elの一番最後におくこと。
;;; (straight.elがpackageをinstallするときに、elispのtemplateのinteractiveな設定を要求されるので)
(use-package autoinsert
  :straight t
  :init
  (setq user-full-name "Yusuke Kitamura")
  (setq user-mail-address "ymyk6602@gmail.com")
  :custom
  ;; テンプレートのディレクトリ
  (auto-insert-directory "~/.emacs.d/template")
  ;; 各ファイルによってテンプレートを切り替える
  :config
  (yk/add-to-list-multiple 'auto-insert-alist
                           '(("\\.cpp$" . ["template.cpp" my-template])
                             ("test_.*\\.cpp$" . ["test_template.cpp" my-template])
                             ("\\.hpp$" . ["template.hpp" my-template])
                             ("CMakeLists.txt$" . ["template.CMakeLists.txt" my-template])
                             ("\\.py$" . ["template.py" my-template])))
  (defvar template-replacements-alists
    '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%file-without-ext%" .
       (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ("%date%" . (lambda () (format-time-string "%Y-%m-%d %H:%M:%S")))
      ("%mail%" . (lambda () (identity user-mail-address)))
      ("%name%" . (lambda () (identity user-full-name)))
      ("%include-guard%" .
       (lambda () (format "%s_HPP__" (upcase (file-name-sans-extension
                                              (file-name-nondirectory buffer-file-name))))))))
  (defun my-template ()
    (time-stamp)
    (mapc #'(lambda(c)
              (progn
                (goto-char (point-min))
                (replace-string (car c) (funcall (cdr c)) nil)))
          template-replacements-alists)
    (goto-char (point-max))
    (message "Successfully insert template."))
  (add-hook 'find-file-not-found-hooks 'auto-insert))

(message "!!!Finish load init.el Successfully!!!")
;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
