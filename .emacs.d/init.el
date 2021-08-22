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
(setq package-selected-packages
      '(use-package
	     init-loader
         posframe
         popwin))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("MELPA Stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;;; ELPAなどで自動で追加される設定をcustom.elに書き込む
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(package-initialize)
(package-refresh-contents)
(package-install-selected-packages)

;; use-package settings
;; This is only needed once, near the top of the file
(require 'use-package)

;; これがないと use-package の Error (Symbol’s value as variable is void: personal-keybindings)が発生
(use-package bind-key
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

;;; enable my utility functions
(use-package yk-util :load-path "./packages/yk")

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
(if (not (file-directory-p "~/.emacs.d/inits/"))
    (make-directory "~/.emacs.d/inits/"))
(setq init-loader-show-log-after-init 'error-only)
(init-loader-load)

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

(use-package tab-bar
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-show 1)
  :config
  (tab-bar-mode t)
  ;; screenと似たkeybindに設定
  (global-unset-key (kbd "C-z"))
  (define-key global-map (kbd "C-z C-z") 'suspend-frame)
  (global-set-key (kbd "C-z SPC") 'tab-next)
  (global-set-key (kbd "C-z C-SPC") 'tab-next)
  (global-set-key (kbd "C-z p") 'tab-previous)
  (global-set-key (kbd "C-z c") 'tab-new)
  (global-set-key (kbd "C-z k") 'tab-close)
  ;; 見た目をおしゃれに
  (custom-set-faces
   '(tab-bar ((t :background "#34495E")))
   '(tab-bar-tab ((t (:background "#93E0E3" :foreground "#2B2B2B" :box (:style pressed-button)))))
   '(tab-bar-tab-inactive ((t (:background "#34495E" :inverse-video nil))))))

;; maximize window
(set-frame-parameter nil 'fullscreen 'maximized)

;; buffer が作られるときに frame を 縦に分割しない
(setq-default split-height-threshold 200)

;; disable beep sound
(setq visible-bell t)

;; hide bars
(tool-bar-mode 0)   ; tool bar を非表示
(menu-bar-mode 0)   ; menu bar を非表示
(if window-system
    (scroll-bar-mode 0)) ; scroll bar を非表示

;; standard mode line setting
(column-number-mode t)             ; column 番号も表示
;; (size-indication-mode t)           ; file size を表示
;; (setq display-time-day-and-date t) ; 曜日,月,日を表示
;; (setq display-time-24hr-format t)  ; 24時間表示
;; (setq display-time-format "%Y/%m/%d %H:%M")
;; (setq display-time-default-load-average nil)
;; (display-time-mode t)

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
(use-package clang-format+
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

;; enable Semantic mode (helm-semantic-or-imenu)
(add-hook 'prog-mode (lambda ()
                       (semantic-mode 1)
                       (imenu--rescan-item)))

;; (use-package smartparens
;;   :ensure t
;;   :commands smartparens-mode
;;   :hook (prog-mode . smartparens-mode))
;;; insert parenthesis/brackets by pair
(electric-pair-mode 1)
;; ;; ;; 対応する括弧を強調表示
(setq show-paren-delay 0)                    ; 表示するまでの秒数
(show-paren-mode t)                          ; 有効化
(setq show-paren-style 'expression)          ; expression は括弧内も強調表示
(use-package rainbow-delimiters
  :ensure t
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


;;; ファイル作成時に生成するテンプレートの設定 (autoinsert)
(use-package autoinsert
  :ensure t
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

;; region選択時に行数と文字数を表示する
(defun count-lines-and-chars ()
  (if mark-active
      (format "(%d lines,%d chars) "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-hook 'activate-mark-hook (lambda ()
                            (add-to-list 'mode-line-format
                                         '(:eval (count-lines-and-chars)))))

;;; goto matching parenthesis, Vi style. The last statement is a bit conked;
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise do nothing"
  (interactive "p")
  (cond
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
   (t (digit-argument (or arg 1)))))

;;
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquity-min-dir-content 2))

;; gdb
(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gdb-use-separate-io-buffer t)

(use-package gud-lldb
  :load-path "./packages/gud-lldb"
  :config
  (add-hook 'lldb-mode-hook '(lambda () (gud-tooltip-mode t))))

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

(defmacro my/hide-minor-mode-from-mode-line (mode)
  "指定したMODEをmode-lineに表示しないようにする。
MODEはsymbolを指定する。
ex. (my/hide-minor-mode-from-mode-line 'rainbow-mode)"
  `(setq minor-mode-alist (cons (list ,mode "") (assq-delete-all ,mode minor-mode-alist))))

(use-package abbrev
  :config
  (my/hide-minor-mode-from-mode-line 'abbrev-mode))

;;; `message` の出力の先頭に日時を付け足す
(defadvice message (before before-message activate)
  (let ((original (ad-get-arg 0))
        (now (decode-time (current-time))))
    (ad-set-arg 0 (format "[%04d/%02d/%02d %02d:%02d:%02d] %s"
                          (nth 5 now) (nth 4 now) (nth 3 now) (nth 2 now) (nth 1 now) (nth 0 now)
                          original))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Package Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-hl-line-mode t)

;;; Theme
(use-package zenburn-theme
  :ensure t
  :custom
  (zenburn-add-font-lock-keywords t)
  :config
  (load-theme 'zenburn t)
  (zenburn-with-color-variables
    (custom-set-faces
     `(hl-line ((t (:background ,zenburn-bg+1))))
     `(hl-line-face ((t (:background ,zenburn-bg+1))))
     '(isearch ((t (:background "green yellow" :foreground "#D0BF8F" :weight bold))))
     '(lazy-highlight ((t (:background "SeaGreen3" :foreground "#D0BF8F" :weight bold))))
     )))

(set-face-foreground 'font-lock-regexp-grouping-backslash "green3")
(set-face-foreground 'font-lock-regexp-grouping-construct "green")

(use-package vline :load-path "./packages")

(use-package col-highlight
  :load-path "./packages"
  :config
  (toggle-highlight-column-when-idle 0)
  :after vline)

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (sml/setup)
;;   (zenburn-with-color-variables
;;     (set-face-attribute 'sml/minor-modes nil :foreground zenburn-cyan))
;;   :after zenburn-theme)

;;; mode-lineの見た目をおしゃれにする
(use-package powerline
  :ensure t
  :config
  (custom-set-faces
   '(powerline-active1 ((t (:background  "#93E0E3" :foreground "#2B2B2B"))))
   '(powerline-active2 ((t (:background  "#303030"))))
   '(powerline-inactive2 ((t (:background  "#383838"))))
   )
  (powerline-default-theme))

(use-package total-lines
  :ensure t
  :config
  (global-total-lines-mode)
  (defun my-set-line-numbers ()
    (setq-default mode-line-front-space
                  (append mode-line-front-space
                          '((:eval (format " (%d)" (- total-lines 1)))))))
  (add-hook 'after-init-hook 'my-set-line-numbers))

(use-package cursor-chg
  :load-path "./packages"
  :custom
  (curchg-default-cursor-type 'box)
  (curchg-change-cursor-on-input-method-flag t)
  (curchg-input-method-cursor-color "green")
  (curchg-overwrite/read-only-cursor-type 'hollow)
  :config
  (change-cursor-mode 1))

(use-package ag
  :ensure t
  :config
  (setq ag-arguments (append '("--follow" "--all-types") ag-arguments)))

(use-package wgrep-ag :ensure t)

;; wgrep setting
(use-package wgrep
  :ensure t
  :commands wgrep-ag-setup
  :config
  (setq wgrep-auto-save-buffer t)
  :hook
  (ag-mode . wgrep-ag-setup)
  :after wgrep-ag)

;; undo tree setting.  C-x u visualize undo tree
(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-mode-lighter nil)
  :config
  (global-undo-tree-mode))

;;; buffer-move setting
(use-package buffer-move
  :ensure t
  :bind (("C-c C-l" . buf-move-left)
         ("C-c C-r" . buf-move-right)))

;;; fast screen
(use-package fast-scroll
  :ensure t
  :demand t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1)
  (my/hide-minor-mode-from-mode-line 'fast-scroll-mode))

;;; chrome の text area を emacs で編集する
(use-package edit-server
  :ensure t)

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
  :ensure t
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
  :ensure t
  :config
  (add-to-list 'xref-backend-functions 'gxref-xref-backend))


(use-package eldoc
  :ensure t
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-minor-mode-string "")
  :hook (prog-mode . eldoc-mode))


(find-function-setup-keys)

;;; ein.el setting (emacs で jupyter notebook を使えるようにしたもの)
;;; 参考 : https://pod.hatenablog.com/entry/2017/08/06/220817
(use-package ein
  :ensure t
  :config
  (setq ein:worksheet-enable-undo t)
  (custom-set-faces
   '(ein:cell-input-area ((t (:background "black" :underline "green yellow" :))))
   '(ein:cell-output-area ((t (:background "blue"))))))

;;;; json をフォーマットする
;;;; sudo apt install jq
(defun jq-format (beg end)
  "Set region from BEG to END where to be formatted."
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;; PDF tool
(when (equal system-type 'gnu/linux)
  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)))

;;; YaTeX (melpa)
(use-package yatex
  :ensure t
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
  :ensure t
  :config
  (my/hide-minor-mode-from-mode-line 'rainbow-mode)
  :hook
  (emacs-lisp-mode . rainbow-mode))

;; (use-package mozc
;;   ;; sudo apt-get install emacs-mozc-bin
;;   :ensure t
;;   :custom
;;   (default-input-method "japanese-mozc"))

;; migemo (日本語のローマ字検索。とりあえずlinuxだけ)
;; sudo apt-get instal -y cmigemo
(when (equal system-type 'gnu/linux)
  (use-package migemo
    :ensure t
    :config
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)))

(use-package google-translate
  :ensure t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :bind
  (("C-c g" . google-translate-at-point)))
(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))

(use-package avy
  :ensure t
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
  :ensure t
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
  :ensure t
  :config
  ; C-h b (keybind display list) をhelmで表示
  (helm-descbinds-mode))

(use-package helm-tramp
  :ensure t
  :custom
  (tramp-default-method "ssh")
  :bind
  (("C-c s" . helm-tramp))
  :config
  (defun helm-tramp-open (path)
    "Tramp open with PATH."
    (helm-find-files-1 path)))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package hydra :ensure t)

(use-package yapfify
  :ensure t
  :config
  (setcar (cdr (assq 'yapf-mode minor-mode-alist)) nil)
  :hook
  (python-mode . yapf-mode))

(defun around-yapfify-call-bin (original-func input-buffer output-buffer start-line end-line)
  "Support docker command."
  (let ((command-args (split-string yapfify-executable)))
    (if (= (length command-args) 1)
        (original-func input-buffer output-buffer start-line end-line)
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

(use-package flycheck
  :ensure t
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
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package which-key
  :ensure t
  :custom
  (which-key-lighter nil)
  :config
  (which-key-mode))

(use-package package-lint
  :ensure t)

;; lsp configuration begin
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-log-io nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-idle-delay 0.50)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  (lsp-file-watch-threshold 2000)
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
  (yk/add-to-list-multiple 'lsp-file-watch-ignored-directories
                           '("[/\\\\]\\.cache"
                             "[/\\\\]build"
                             "[/\\\\]edk2"
                             "[/\\\\]\\.ccls"))
  (yk/add-to-list-multiple 'lsp-file-watch-ignored-files
                           '("[/\\\\][^/\\\\]+\\.o"
                             "[/\\\\][^/\\\\]+\\.a"
                             "[/\\\\]\\.[/\\\\]+"))
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (python-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (rust-mode . lsp-deferred)))

(use-package helm-lsp
  :ensure t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  :bind
  (("C-c h a" . helm-lsp-code-actions))
  (("C-c h s" . helm-lsp-workspace-symbol))
  (("C-c h d" . helm-lsp-diagnostics)))

(use-package lsp-docker+
  :load-path "./packages/lsp-docker+"
  :ensure t
  :config
  (lsp-docker+-enable)
  ;; register default lsp server
  (let ((lsp-docker+-image-id "arumatik/common-language-servers")
        (client-packages (list lsp-bash lsp-css lsp-dockerfile lsp-go lsp-html lsp-javascript
                               lsp-cmake))
        (lsp-docker+-client-configs
         (list
          (list :server-id 'bash-ls :docker-server-id 'bashls-docker
                :server-command "bash-langauge-server start")
          (list :server-id 'css-ls :docker-server-id 'cssls-docker
                :server-command "css-languageserver --stdio")
          (list :server-id 'dockefile-ls :server-id 'dockerfilels-docker
                :server-command "docker-langserver --stdio")
          (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
          (list :server-id 'html-ls :docker-server-id 'htmlls-docker
                :server-command "html-languageserver --stdio")
          (list :server-id 'ts-ls :docker-server-id 'tsls-docker
                :server-command "typescript-language-server --stdio")
          (list :server-id 'cmakels :docker-server-id 'cmakels-docker
                :server-command "cmake-language-server"))))
    (lsp-docker+-init-clients :client-configs lsp-docker+-client-configs)))

(use-package lsp-ui
  :ensure t
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
  (lsp-ui-doc-frame . disable-tab-bar-in-lsp-ui-doc-frame))

(use-package lsp-go)
(use-package lsp-html)
(use-package lsp-csharp)
(use-package lsp-rust
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"))
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright))))

(use-package ccls
  :ensure t
  :custom
  (ccls-initialization-options (list :compilationDatabaseDirectory "build")))

(use-package yasnippet
  :ensure t
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
  :ensure t
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

;; dap-mode setting
(use-package dap-mode
  :ensure t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package dap-lldb
  :config
  (setq dap-lldb-debug-program
        `(,(expand-file-name "~/.vscode/extensions/ms-vscode.cpptools-1.0.1/bin/cpptools-srv"))))

(use-package dap-python)

(use-package company-tabnine
  :ensure t
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

;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
