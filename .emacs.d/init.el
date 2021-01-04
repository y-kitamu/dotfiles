;; -*-no-byte-compile: t; -*-
(setq package-selected-packages
      '(use-package
	init-loader
        posframe
        popwin
        ))

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
  (add-to-list 'exec-path (expand-file-name "~/.local/bin")))
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

; window の切替
(define-key global-map (kbd "C-t") 'other-window)
(global-set-key (kbd "C-o") (lambda ()
                              (interactive)
                              (other-window -1)))

(use-package tab-bar
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
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
   '(tab-bar-tab-inactive ((t (:background "#34495E" :inverse-video nil))))
   )
  )

;; maximize window
(set-frame-parameter nil 'fullscreen 'maximized)

;; buffer が作られるときに frame を 縦に分割しない
(setq-default split-height-threshold 200)

;; disable beep sound
(setq visible-bell t)

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
;; (size-indication-mode t)           ; file size を表示
(setq display-time-day-and-date t) ; 曜日,月,日を表示
(setq display-time-24hr-format t)  ; 24時間表示
(setq display-time-format "%Y/%m/%d %H:%M")
(setq display-time-default-load-average nil)
(display-time-mode t)

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
  :init
  (setq user-full-name "Yusuke Kitamura")
  (setq user-mail-address "ymyk6602@gmail.com")
  :custom
  ;; テンプレートのディレクトリ
  (auto-insert-directory "~/.emacs.d/insert")
  ;; 各ファイルによってテンプレートを切り替える
  (add-to-list 'auto-insert-alist (nconc '(("test_.*\.cpp$" . ["test_template.cpp" my-template])
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

;; region選択時に行数と文字数を表示する
(defun count-lines-and-chars ()
  (if mark-active
      (format "(%d lines,%d chars) "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-hook 'activate-mark-hook (lambda ()
                            (add-to-list 'mode-line-format
                                         '(:eval (count-lines-and-chars)))
                            ))

;;; goto matching parenthesis, Vi style. The last statement is a bit conked;
;;;###autoload
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
  (add-hook 'lldb-mode-hook '(lambda () (gud-tooltip-mode t)))
  )

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
         (text-mode . flyspell-mode))
  )

(defmacro my/hide-minor-mode-from-mode-line (mode)
  "指定したMODEをmode-lineに表示しないようにする。
MODEはsymbolを指定する。
ex. (my/hide-minor-mode-from-mode-line 'rainbow-mode)"
  `(setq minor-mode-alist (cons (list ,mode "") (assq-delete-all ,mode minor-mode-alist))))

(use-package abbrev
  :config
  (my/hide-minor-mode-from-mode-line 'abbrev-mode)
  )

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
     )
    ;; (set-face-attribute 'mode-line nil :background zenburn-bg+3 :foreground zenburn-fg+2)
    ))

(set-face-foreground 'font-lock-regexp-grouping-backslash "green3")
(set-face-foreground 'font-lock-regexp-grouping-construct "green")

(use-package vline
  :load-path "./packages"
  )

(use-package col-highlight
  :load-path "./packages"
  :config
  (toggle-highlight-column-when-idle 0)
  :after vline
  )

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

(use-package wgrep-ag
  :ensure t
)

;; wgrep setting
(use-package wgrep
  :ensure t
  :commands wgrep-ag-setup
  :config
  (setq wgrep-auto-save-buffer t)
  :hook
  (ag-mode . wgrep-ag-setup)
  :after wgrep-ag
  )

;; undo tree setting.  C-x u visualize undo tree
(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-mode-lighter nil)
  :config
  (global-undo-tree-mode)
  )

;;; buffer-move setting
(use-package buffer-move
  :ensure t
  :bind (("C-c C-l" . buf-move-left)
         ("C-c C-r" . buf-move-right))
  )

;;; fast screen
(use-package fast-scroll
  :ensure t
  :demand t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1)
  (my/hide-minor-mode-from-mode-line 'fast-scroll-mode)
  )

;;; magit
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  ;; (when (equal system-type 'windows-nt) ;; windows の場合、git の .exe file の場所を指定
  ;;   (setq magit-git-executable "c:/Users/y-kitamura/AppData/Local/Programs/Git/bin/git.exe"))
  (defun unpackaged/magit-log--add-date-headers (&rest _ignore)
    "Add date headers to Magit log buffers."
    (when (derived-mode-p 'magit-log-mode)
      (save-excursion
        (ov-clear 'date-header t)
        (goto-char (point-min))
        (cl-loop with last-age
                 for this-age =
                 (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                   car
                   (overlay-get it 'before-string)
                   (get-text-property 0 'display it)
                   cadr
                   (s-match (rx (group (1+ digit) ; number
                                       " " (1+ (not blank))) ; unit
                                (1+ blank) eos) it)
                   cadr)
                 do (when (and this-age (not (equal this-age last-age)))
                      (ov (line-beginning-position) (line-beginning-position)
                          'after-string (propertize (concat " " this-age "\n")
                                                    'face 'magit-section-heading)
                          'date-header t)
                      (setq last-age this-age))
                 do (forward-line 1)
                 until (eobp)))))

  (define-minor-mode unpackaged/magit-log-date-headers-mode
    "Display date/time headers in `magit-log' buffers."
    :global t
    (if unpackaged/magit-log-date-headers-mode
        (progn
          ;; Enable mode
          (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
          (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
      ;; Disable mode
      (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
      (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers)))
  :bind
  ("C-x g" . magit-status)
  )

;; reference : https://emacs-jp.github.io/packages/git-gutter
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:lighter "")
  (global-git-gutter-mode t)
  )

;;; chrome の text area を emacs で編集する
(use-package edit-server
  :ensure t)

;;; multi-term
;; .bashrc に $TERM が eterm-color の場合にも color-prompt にするように設定を追記する
(use-package multi-term
  :ensure t
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
  :ensure t
  :config
  (add-to-list 'xref-backend-functions 'gxref-xref-backend)
  )

;;; org mode setting
(use-package org
  :ensure t
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
  ("C-c e X" . 'org-publish-project)
  )

(defun override-org-html-src-block (src-block _contents info)
  "Jekyll ブログエクスポート用のコードブロック生成。
rougeでソースコードをhighlight、#+name: <filename> で指定したファイル名を表示する。
TODO:  roughのlangとemacs (org)のlangの表記の対応表の作成"
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	       (code (org-html-format-code src-block info))
	       (label (let ((lbl (and (org-element-property :name src-block)
				                  (org-export-get-reference src-block info))))
		            (if lbl (format " id=\"%s\"" lbl) "")))
	       (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
					                       "ruby" "scheme" "clojure" "php" "html"))))
           (name (org-element-property :name src-block))
           (prefix
            (format "<figure><figcaption class=\"figcaption\">%s</figcaption>\n{%% highlight %s %%}\n"
                    name lang))
           (suffix "{% endhighlight %}\n</figure>"))
      (concat prefix code suffix))))
(advice-add 'org-html-src-block :override 'override-org-html-src-block)


;;; open-junk-file setting
(use-package open-junk-file
  :ensure t
  :config
  (setq org-archive-location (concat "~/.emacs.d/junk/"
                                     (format-time-string "%Y_%m_%d" (current-time))
                                     ".org::"))
  (setq open-junk-file-format "~/.emacs.d/junk/%Y_%m_%d.org")
  :bind
  ("C-x j" . open-junk-file)
  ("C-x C-j" . (lambda() (interactive) (find-file "~/.emacs.d/junk/main.org")))
  )


(setq org-publish-project-alist
      '(
        ("org-blog"
         :base-directory "~/Documents/kitamura.github.io/org/"
         :base-extension "org"
         :publishing-directory "~/Documents/kitamura.github.io/docs"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )
        ("org-blog-static"
         :base-directory "~/Documents/kitamura.github.io/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/Documents/kitamura.github.io/docs"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog" :components ("org-blog" "org-blog-static"))
        ))

;; lisp の評価結果を注釈する
(use-package lispxmp
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d" . 'lispxmp))
  )

;; カッコの対応を保持して編集する設定
(use-package paredit
  :ensure t
  :custom
  (paredit-lighter nil)
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (ielm-mode . enable-paredit-mode))
  )

(use-package eldoc
  :ensure t
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-minor-mode-string "")
  :hook (prog-mode . eldoc-mode)
  )

(use-package auto-async-byte-compile
  :ensure t
  :custom
  (auto-async-byte-compile-exclude-files-regexp ".emacs.d/junk/*")
  :hook
  ((emacs-lisp-mode . enable-auto-async-byte-compile-mode)
   (emacs-lisp-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (ielm-mode . turn-on-eldoc-mode))
  :after eldoc
  )

(find-function-setup-keys)

;;; python の仮想環境の設定
;;; cd [project_root] && python -m venv [venv name] で仮想環境作成
;;; dotfiles/requirements.txt をインストール
;;; M-x pyvenv-activate [project_root]/[venv name] で activate
;;; project のファイルを開くと、自動で仮想環境の lsp が立ち上がる。
;;; すでにファイルが開いている場合は、 pyvenv-activate のあと、lsp-workspace-restart とする
;;; -> lsp-dockerに移行するので基本的に使用しない
;; (use-package pyvenv
;;   :ensure t
;;   :diminish
;;   :config
;;   (setq pyvenv-mode-line-indicator
;;         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;;   (pyvenv-mode +1)
;;   (defun pyvenv-auto-activate ()
;;     "Automatically activate python virtual enviroment by searching venv directory."
;;     (interactive)
;;     (let ((dirname (file-name-directory (directory-file-name buffer-file-name))))
;;       (while (and (not (file-exists-p (format "%s/venv" dirname))) (not (equal dirname "/")))
;;         (setq dirname (file-name-directory (directory-file-name dirname))))
;;       (if (equal dirname "/")
;;           (message "pyvenv :: Failed to auto activate to venv. No venv directory is found.")
;;         (pyvenv-activate (format "%s/venv" dirname))
;;         (message (format "pyvenv :: Activate %s/venv" dirname)))
;;       ))
;;   ;; :hook
;;   ;; (python-mode . pyvenv-auto-activate)
;;   )

;;; ein.el setting (emacs で jupyter notebook を使えるようにしたもの)
;;; 参考 : https://pod.hatenablog.com/entry/2017/08/06/220817
(use-package ein
  :ensure t
  :config
  (setq ein:worksheet-enable-undo t)
  (custom-set-faces
   '(ein:cell-input-area ((t (:background "black" :underline "green yellow" :))))
   '(ein:cell-output-area ((t (:background "blue")))))
  )

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
    (pdf-tools-install)
    )
  )

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

;;; programing language major modes
(use-package rust-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package docker-compose-mode :ensure t)
(use-package go-mode :ensure t)
(use-package typescript-mode :ensure t)
(use-package cmake-mode :ensure t)

;;; for windows
(use-package powershell
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.psl\\'" . powershell-mode))
  )

;; web mode setting
(use-package web-mode
  :ensure t
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
  :after rainbow-mode
  :hook (web-mode . rainbow-mode)
  )

(add-to-list 'auto-mode-alist '("\\.bash.*\\'" . sh-mode))

(define-key global-map (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(use-package mozc
  ;; sudo apt-get install emacs-mozc-bin
  :ensure t
  :custom
  (default-input-method "japanese-mozc"))

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
    (migemo-init)
    )
  )

(use-package google-translate
  :ensure t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :bind
  (("C-c g" . google-translate-at-point))
  )

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :custom
  (avy-background t)
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
   ("C-h a" . helm-apropos)
   ("M-x" . helm-M-x))
  )

(use-package helm-descbinds
  :ensure t
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

(use-package helm-tramp
  :ensure t
  :custom
  (tramp-default-method "ssh")
  :bind
  (("C-c s" . helm-tramp))
  :config
  (defun helm-tramp-open (path)
    "Tramp open with PATH."
    (helm-find-files-1 path))
  )

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package hydra :ensure t)

(use-package yapfify
  :ensure t
  :config
  (setcar (cdr (assq 'yapf-mode minor-mode-alist)) nil)
  :hook
  (python-mode . yapf-mode)
  )

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
                                           (number-to-string end-line))))))
))))
(advice-add 'yapfify-call-bin :around 'around-yapfify-call-bin)

(defun around-yapfify-region (original-func &rest args)
  "Wrap `yapfify-region` to catch error and make sure to kill *yapfify* buffer"
     (-if-let (tmp-buffer (get-buffer "*yapfify*"))
         (kill-buffer tmp-buffer))
     (apply original-func args)
     )
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
  :bind
  (("C-c f" . flycheck-list-errors))
  :hook
  (after-init . global-flycheck-mode))


(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package which-key
  :ensure t
  :custom
  (which-key-lighter nil)
  :config
  (which-key-mode))


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
  :hook
  ((prog-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  )

(use-package helm-lsp
  :ensure t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
)

(defcustom dir-local-docker-config-alists nil
  "List of lsp docker configuration alist.
Alist contains keys named :
DOCKER-IMAGE-ID : Docker image name.
DOCKER-CONTAINER-NAME : Name of docker container to be launched.
LSP-DOCKER-CLIENT-CONFIGS : Client-config list.

- c++の場合の設定
プロジェクト固有のINCLUDE_PATHにパスを通すためには
1. compile_commands.jsonのリンクをプロジェクトルートに作成する
2. CMakeLists.txt のinclude_directoriesで指定するパスを絶対パスにする
"
  :safe (lambda (alists)
          (-all? (lambda (it)
                   (and (stringp (assoc-default 'docker-image-id it))
                        (stringp (assoc-default 'docker-container-name it))
                        (listp (assoc-default 'lsp-docker-client-configs it))))
                 alists)))

(defmacro my-lsp-docker-init-clients
    (docker-image-id docker-container-name lsp-docker-client-configs)
  "lsp docker initialization"
  (declare (indent 2))
  ;; 'lsp-docker-init-clients 呼び出しマクロ
  ;; TODO : 'path-mapping 指定の部分のハードコーディング解消
  `(lsp-docker-init-clients
    :path-mappings '(("/home/kitamura/work/" . "/home/kitamura/work/"))
    :docker-image-id ,docker-image-id
    :docker-container-name ,docker-container-name
    :client-configs ,lsp-docker-client-configs)
  )

(defun start-local-lsp-docker (local-docker-config-alist)
  "docker環境のlsp server立ち上げ関数。
.dir-locals.el内の変数dir-local-docker-config-alistsで定義されたlsp用dockerを立ち上がる。
dir-local-docker-config-alistsの各要素では以下のkeyを定義する
DOCKER-IMAGE-ID, DOCKER-CONTAINER-NAME and LSP-DOCKER-CLIENT-CONFIGS"
  (-when-let* (((&alist 'docker-image-id docker-image-id
                        'docker-container-name docker-container-name
                        'lsp-docker-client-configs lsp-docker-client-configs)
                local-docker-config-alist))
    (my-lsp-docker-init-clients
        docker-image-id docker-container-name lsp-docker-client-configs)
    (message (format "Start local lsp docker container '%s' from image '%s'"
                     docker-container-name docker-image-id))
    ))

(defun before-lsp (&rest rest)
  "lspのadvice関数。
.dir-locals.elの変数を取得して関数start-local-lsp-dockerを呼び出してdockerのlsp serverを立ち上げる。"
  (hack-dir-local-variables-non-file-buffer)
  (mapc 'start-local-lsp-docker dir-local-docker-config-alists)
  )

(use-package lsp-docker
  :ensure t
  :config
  (advice-add 'lsp :before 'before-lsp)
  (setq lsp-docker-default-client-packages
    '(lsp-bash lsp-clients lsp-css lsp-go lsp-dockerfile
               lsp-html lsp-javascript lsp-json lsp-yaml)
    )
  (setq lsp-docker-default-client-configs
        (list
         (list :server-id 'bash-ls
               :docker-server-id 'bashls-docker
               :server-command "bash-language-server start")
         (list :server-id 'css-ls
               :docker-server-id 'cssls-docker
               :server-command "css-languageserver --stdio")
         (list :server-id 'dockerfile-ls
               :docker-server-id 'dockerfilels-docker
               :server-command "docker-langserver --stdio")
         (list :server-id 'gopls
               :docker-server-id 'gopls-docker
               :server-command "gopls")
         (list :server-id 'html-ls
               :docker-server-id 'htmls-docker
               :server-command "html-languageserver --stdio")
         (list :server-id 'ts-ls
               :docker-server-id 'tsls-docker
               :server-command "typescript-language-server --stdio")
         (list :server-id 'json-ls
               :docker-server-id 'jsonls-docker
               :server-command "json-language-server --stdio")
         (list :server-id 'yamlls
               :docker-server-id 'yamlls-docker
               :server-command "yaml-language-server --stdio")
         )
        )
  (lsp-docker-init-clients
   :path-mappings '(("/home/kitamura" . "/work/"))
   :docker-image-id "arumatik/common-language-servers"
   :docker-container-name "common-lsp-container"
   :priority 10
   :client-packages lsp-docker-default-client-packages
   :client-configs lsp-docker-default-client-configs)
  (message "Finish register defualt docker lsp clients")
)

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
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-hover nil)
  )

(use-package lsp-go)
(use-package lsp-html)
(use-package lsp-csharp)

(use-package lsp-pyright :ensure t)

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
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  (company-lighter nil)
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

(use-package ccls
  :ensure t
  :custom
  (ccls-initialization-options (list :compilationDatabaseDirectory "build"))
  )

;; lsp configuration end

;; dap-mode setting
(use-package dap-mode
  :ensure t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  )

(use-package dap-lldb
  :config
  (setq dap-lldb-debug-program `(,(expand-file-name "~/.vscode/extensions/ms-vscode.cpptools-1.0.1/bin/cpptools-srv"))))

(use-package dap-python)

(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (defun toggle-tabnine ()
    "tabnineのenable, disableの切り替え"
    (interactive)
    (if company-tabnine--disabled
        (setq company-tabnine--disabled nil)
      (setq company-tabnine--disabled t)))
  )
