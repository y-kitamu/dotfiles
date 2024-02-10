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

;;; Make sure we can debug init errors more easily:
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;; straight.el setting
;; To be compatible with upstream Emacs
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

;;; 自動で追加される設定をcustom.elに書き込む
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;;; enable my utility functions
(use-package yk-util
  :straight (yk-util :type git :host github :repo "y-kitamu/yk_elisp"))

;;; Performance improvement
;;; Increase a bit garbage collection threshold:
(setq gc-cons-threshold 51200000)
;;; Avoid performance issues in files with very long lines
(global-so-long-mode 1)

;;; emacs internal shell path
(when (equal system-type 'gnu/linux)
  (yk/add-to-list-multiple 'exec-path (list (expand-file-name "~/.local/bin")
                                            (expand-file-name "~/.cargo/bin")
                                            (expand-file-name "~/.deno/bin"))))

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
(setq auto-revert-remote-files t)
(global-auto-revert-mode t)

;;; 再起動時に前回開いていたファイルを開く
(desktop-save-mode 1)

;;; Indent settings
(setq-default tab-width 4)          ; default の tab の表示幅
(setq-default indent-tabs-mode nil) ; indent に tab文字を使用しない

;;; paren settings
;;; insert parenthesis/brackets by pair
(electric-pair-mode 1)
;; ;; ;; 対応する括弧を強調表示
(setq show-paren-delay 0)                    ; 表示するまでの秒数
(show-paren-mode t)                          ; 有効化
(setq show-paren-style 'expression)          ; expression は括弧内も強調表示
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; markdown mode 以外では行末の white space を削除して保存
(add-hook 'before-save-hook (lambda () (unless (eq major-mode 'markdown-mode)
                                         (delete-trailing-whitespace))))

;; fileが!#で初まる場合、+x を付けて保存
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; built-in packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquity-min-dir-content 2)

(require 'abbrev)
(my/hide-minor-mode-from-mode-line 'abbrev-mode)

(require 'hexl)
(define-key hexl-mode-map (kbd "C-t") 'other-window)
(define-key hexl-mode-map (kbd "C-o") (lambda ()
                                     (interactive)
                                     (other-window -1)))

;; gdb
(setq gdb-many-windows t)
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
(setq gdb-use-separate-io-buffer t)

;;; 矩形編集 (+ C-v, M-v で一番下、一番上まで移動できるようになる)
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;; spellチェック
(use-package flyspell
  :custom
  (flyspell-mode-line-string nil)
  :hook
  ((prog-mode . flyspell-prog-mode)
   (yatex-mode . flyspell-mode)
   (org-mode . flyspell-mode)
   (text-mode . flyspell-mode)))

(defun rename-file-with-buffer (old-filename new-filename &optional _)
  "Rename file and the corresponding buffer.  Rename from `OLD-FILENAME' to `NEW-FILENAME'."
  (interactive "fRename file: \nGRename %s to file: \np")
  (rename-file old-filename new-filename)
  (let ((buf (get-file-buffer old-filename)))
    (if buf
        (with-current-buffer buf
          (set-visited-file-name new-filename nil t)
          (set-buffer-modified-p nil)))))

(defun delete-file-with-buffer (filename)
  "Delete file of `FILENAME' and the corresponding buffer.")

;;; custom key binding
(global-set-key (kbd "C-M-s") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
(global-unset-key (kbd "C-x d"))

(defun yk/kill-unbind-buffers ()
  "Kill all buffers which are unbinded to a file."
  (interactive)
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (if-let ((file-name (buffer-file-name)))
           (if (null (file-exists-p file-name))
               (progn
                 (message "Killed buffer %s of %s" buf file-name)
                 (kill-buffer buf))
             ))))
   (buffer-list)))

(defun yk/copy-buffer-filename-as-kill ()
  "Copy the current directory into the kill ring."
  (interactive)
  (if (null (buffer-file-name))
      (message "No file is associated with this buffer")
    (kill-new (buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Third Party Package Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; completion packages (vertico)
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-resize t)
  ;; (setq vertico-cycle t)
  (defface yk-vertico-alert '((t :foreground "red")) "Face used to alert on vertico")
  (defface yk-vertico-warn '((t :foreground "cyan")) "Face used to warn on vertico")

  (defun yk-vertico-format-multi-category  (cand buffer-name)
    "Highlight buffer of which file is deleted or buffer is modified"
    (if (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (if-let ((file-name (buffer-file-name)))
              (cond ((null (file-exists-p file-name))
                     (add-face-text-property 0 (length cand) 'yk-vertico-alert 'append cand))
                    ((buffer-modified-p)
                     (add-face-text-property 0 (length cand) 'yk-vertico-warn 'append cand)))
            t))))

  (defun yk-vertico-format-candidates (original-func cand &rest rest)
    (let* ((formatted (apply original-func cand rest)))
      (cond ((eq 'multi-category (vertico--metadata-get 'category))
             (yk-vertico-format-multi-category formatted (vertico--display-string cand))))
      formatted))
  (advice-remove #'vertico--format-candidate #'yk-vertico-format-candidates)
  (advice-add #'vertico--format-candidate :around #'yk-vertico-format-candidates)

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (sort (seq-filter (lambda (x) (string-suffix-p "/" x)) files) 'string<)
           (sort (seq-remove (lambda (x) (string-suffix-p "/" x)) files) 'string<)))
  :bind
  (:map vertico-map
        ("C-v" . vertico-scroll-up)
        ("M-v" . vertico-scroll-down)))

(use-package vertico-buffer
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/")

(use-package vertico-flat
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/")

(use-package vertico-grid
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/")

(use-package vertico-unobtrusive
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/")

(use-package vertico-directory
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("C-l" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico vertico-buffer
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :config
  (vertico-multiform-mode))

;; Change the default sorting function.
;; See `vertico-sort-function' and `vertico-sort-override-function'.
(setq vertico-multiform-commands
      '((describe-symbol (vertico-sort-function . vertico-sort-alpha))))

(setq vertico-multiform-categories
      '((symbol (vertico-sort-function . vertico-sort-alpha))
        (file grid (vertico-sort-function . sort-directories-first))))


(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (custom-set-faces
   '(marginalia-documentation ((t (:foreground "#D0BF8F"))))))

(use-package embark
  :ensure t
  :after vertico
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h b" . embark-bindings)
   :map embark-file-map
   ("r" . rename-file-with-buffer)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-indicators '(embark-minimal-indicator))
  (setq embark-prompter 'embark-completing-read-prompter)

  ;; find file in directory


  ;; helm like tab
  (define-key vertico-map (kbd "<tab>") 'embark-act)
  (defun with-minibuffer-keymap (keymap)
    (lambda (fn &rest args)
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map
             (make-composed-keymap keymap (current-local-map))))
        (apply fn args))))
  (defvar embark-completing-read-prompter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<tab>") 'abort-recursive-edit)
      map))
  (advice-add 'embark-completing-read-prompter :around
              (with-minibuffer-keymap embark-completing-read-prompter-map))
  ;; (define-key embark-collect-mode-map "" #'+embark-unmark)
  :config
  )


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("C-S-f" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  (defun yk/find-file-command-builder (dir ignores &optional files)
    (require 'find-dired)
    (require 'xref)
    (defvar find-name-arg)
    (let* ((default-directory dir)
           ;; Make sure ~/ etc. in local directory name is
           ;; expanded and not left for the shell command
           ;; to interpret.
           (localdir (file-name-unquote (file-local-name (expand-file-name dir))))
           )
      (format "%s -H %s %s -type f %s -print0"
              find-program
              (shell-quote-argument
               (directory-file-name localdir)) ; Bug#48471
              (xref--find-ignores-arguments ignores localdir)
              (if files
                  (concat (shell-quote-argument "(")
                          " " find-name-arg " "
                          (mapconcat
                           #'shell-quote-argument
                           (split-string files)
                           (concat " -o " find-name-arg " "))
                          " "
                          (shell-quote-argument ")"))
                ""))))

  (defun yk/find-file-in-directory (directory)
    (interactive "DDirectory: ")
    (message "command = %s" (yk/find-file-command-builder directory nil))
    (consult--read
     (project--files-in-directory directory nil)
     ;; (consult--async-command
     ;;     (lambda ()
     ;;       (message "directory = %s" directory)
     ;;       (let ((command yk/find-file-command-builder directory nil))
     ;;         (message "command = %s" command)
     ;;         command))
     ;;   )
     :prompt (format "Find file in %s: " directory)
     :category 'file
     :require-match t
     :sort nil
     )
    )
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :straight t
  :ensure t
  :config
  ;; enable ssh
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  ;; enable docker
  (defun consult-dir--tramp-docker-hosts ()
    "Get a list of hosts from docker."
    (when (require 'docker-tramp nil t)
      (let ((hosts)
            (docker-tramp-use-names t))
        (dolist (cand (docker-tramp--parse-running-containers))
          (let ((user (unless (string-empty-p (car cand))
                        (concat (car cand) "@")))
                (host (car (cdr cand))))
            (push (concat "/docker:" user host ":/") hosts)))
        hosts)))
  (defvar consult-dir--source-tramp-docker
    `(:name     "Docker"
      :narrow   ?d
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'consult-dir--tramp-docker-hosts)
    "Docker candiadate source for `consult-dir'.")
;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t)
  :after docker-tramp
  :bind
  (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; When reading a file name, completion ignores case.
(setq read-file-name-completion-ignore-case t)

(use-package tramp :ensure t)

;;; enable recentf
(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 100
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)
        )
  (recentf-mode 1))

;;; coding metrics
(use-package wakatime-mode
  :ensure t
  :init
  ;; run `make install-wakatime'
  (if (eq system-type 'gnu/linux)
      (setq wakatime-cli-path (expand-file-name "~/.local/bin/wakatime-cli-linux-amd64")))

  (if (boundp 'wakatime-cli-path)
      (if (file-exists-p wakatime-cli-path)
          (progn
            (setq wakatime-api-key "4c9b2c0a-0b0d-4630-bd29-d3af45e3a2e6")
            (global-wakatime-mode)))))

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
  (setq undo-tree-auto-save-history nil)
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

;;; vterm
(use-package vterm
  :straight t
  :ensure t
  :init
  (setq vterm-always-compile-module t)
  :config
  (defun yk/vterm-enable-copy-mode ()
    (interactive)
    (unless (bound-and-true-p vterm-copy-mode)
      (vterm-copy-mode)))
  (defun yk/vterm-disable-copy-mode ()
    (interactive)
    (if (bound-and-true-p vterm-copy-mode)
        (vterm-copy-mode -1)))

  (yk/add-to-list-multiple 'vterm-keymap-exceptions '("C-t" "C-o" "C-z" "C-d"))
  (vterm--exclude-keys vterm-mode-map vterm-keymap-exceptions)

  (defun yk/vterm--read-from-kill-ring ()
    (current-kill 0)
    (consult--lookup-member
     (consult--read
      (consult--remove-dups
       (or kill-ring (user-error "Kill ring is empty")))
      :prompt "Yank from kill-ring: "
      :history t ;; disable history
      :sort nil
      :category 'kill-ring
      :require-match t)
     kill-ring))

  (defun yk/vterm-yank-from-kill-ring (string &optional arg)
    (interactive (list (yk/vterm--read-from-kill-ring) current-prefix-arg))
    (when string
      (deactivate-mark)
      (vterm-goto-char (point))
      (message "string = %s" string)
      (let ((inhibit-read-only t))
        (vterm-insert string))))

  (define-key vterm-mode-map [escape] nil)
  (define-key vterm-mode-map (kbd "C-d") #'vterm-send-delete)
  (define-key vterm-mode-map (kbd "C-c C-c") #'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "C-c z") #'vterm-send-C-z)
  (define-key vterm-mode-map (kbd "M-y") #'yk/vterm-yank-from-kill-ring)
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-c C-j") #'yk/vterm-enable-copy-mode)
  (define-key vterm-copy-mode-map (kbd "C-c C-k") #'yk/vterm-disable-copy-mode)
  )

(use-package multi-vterm
  :straight t
  :ensure t
  :after vterm)  ; line-mode へ切り替え

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
  (default-input-method "japanese-mozc")

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
            (migemo-init))))))

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
   ("M-g f" . avy-goto-line)))

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package hydra :straight t)

;;; 日本語の文章チェックはtextlintをインストールする
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

(use-package doxygen
  :straight t)

;;; Yasnippet configurations
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode)
  (my/hide-minor-mode-from-mode-line 'yas-minor-mode)
  :diminish yas-minor-mode
  :bind
  (:map yas-keymap
        ("<tab>" . nil)) ;; because of avoiding conflict with company keymap
  )

(use-package yasnippet-snippets
  :straight t
  :init
  (yasnippet-snippets-initialize))

(use-package consult-yasnippet
  :after yasnippet
  :straight t
  :bind
  (:map yas-minor-mode-map
        ("C-c y" . consult-yasnippet)))

(use-package poetry
  :straight t
  :config
  (poetry-tracking-mode))


(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*.py" "acm/*" "core/*") :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :bind
  (("M-." . lsp-bridge-find-def)
   ("M-," . lsp-bridge-find-def-return))
  :after (yasnippet markdown-mode))

;;; Chat AI configurations
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :init
  (define-key python-mode-map (kbd "<backtab>") 'copilot-accept-completion)
  :bind
  ("<backtab>" . copilot-accept-completion)
  :hook
  ((prog-mode . copilot-mode))
  :after python)

(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq python-interpreter
    (cond ((executable-find "python3") "python3")
          ((executable-find "python") "python")
          (t "python3")))
  ;; openapiの環境変数を設定
  (let* ((secretdir (file-name-directory (file-truename (expand-file-name "~/.emacs.d"))))
         (secretfile (concat (file-name-as-directory secretdir) "secrets.sh")))
    (when (file-exists-p secretfile)
      (message "Load secret file")
      (yk/read_shell_file_to_set_envvar secretfile)))
  (let ((path (getenv "PATH")))
    (setenv "PATH" (concat path ":" (expand-file-name "~/.local/bin"))))
  (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
  :bind ("C-c q" . chatgpt-query))


;;;;;;;;;;;;;;;;;;;; File Footer ;;;;;;;;;;;;;;;;;;;;

;;; ここから下はinit.elの一番最後におくこと。
;;; ファイル作成時に生成するテンプレートの設定 (autoinsert)
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
                             ("\\.py$" . ["template.py" my-template])
                             ("\\.mdx$" . ["template.mdx" my-template])
                             ("\\.md$" . ["template.mdx" my-template])
                             ("[0-9]+\\(_\\w+\\)+\\.org$" . ["template_paper.org" my-template])
                             ("[0-9]+_[0-9]+_[0-9]+\\.org$" . ["template_todo.org" my-template])))
  (defvar template-replacements-alists
    '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%file-without-ext%" .
       (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ("%date%" . (lambda () (format-time-string "%Y-%m-%d")))
      ("%datetime%" . (lambda () (format-time-string "%Y-%m-%d %H:%M:%S")))
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
  (add-to-list 'find-file-not-found-functions 'auto-insert))

(message "!!!!! Finish loading init.el successfully !!!!!")
;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
