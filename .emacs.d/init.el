;;;;;;;;;;  Emacs Setting File  ;;;;;;;;;;
;;; Emacs version 25.2.1 or higher is required.

;;
(require 'cl)


;;;;; ELPA settings ;;;;;
;; add reference list  of ELPA package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;;;;; use-package library configuration
;; if use-package dosen't find, nothing is done.
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;;;;; Launch setting ;;;;;
;; answer the emacs's question by y/n
;;(fset 'yes-or-no-p 'y-or-n-p)
;; color theme configuration
(load-theme 'manoj-dark t)

;; window size
(setq initial-frame-alist
      (append (list
               '(width . 80)
               '(height . 59))))

;; hide start-up message
(setq inhibit-startup-screen t)

;; if using emacs23 or before 
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; define function of adding load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add arguments' directory & subdirectory to the load-path
(add-to-load-path "elisp" "elpa")

;; if you want to load elisp file init-name.el, do below, or use init-loader.el
;;(load "init-name")

;; avoid "Symbolic link to SVN-controlled source file; follow link? (yes or no)"
(setq vc-follow-symlinks t)

;; システムのクリップボードとemacsのクリップボードを統合
(setq select-enable-clipboard t) 

;; ビープ音を消す
(setq ring-bell-function 'ignore)


;; Don't show log buffer
(setq init-loader-show-log-after-init nil)

;; shellのPATHを読み込めるようにする
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "PYTHONPATH")

;;;;; major mode setting ;;;;;
;; .cu and .h file is opened with C++ major mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Docker file mode
(require 'dockerfile-mode)
(require 'docker-compose-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "cc-mode")))


;;;;; window appearance setting ;;;;;

;; hide tool-bar and scroll-bar when emacs is on GUI (i.e. not on terminal)
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
;; hide menu-bar
(menu-bar-mode 0)


;;;;; key-bind seting ;;;;;

;; insert newline and indent
(global-set-key (kbd "C-m") 'newline-and-indent)
;; 
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; change window
(define-key global-map (kbd "C-t") 'other-window)


;;;;; set $PATH (for Mac) ;;;;;
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")


;;;;;; set character code ;;;;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


;;;;;; Frame Appearance Setting ;;;;;

;; set window to 80 columns
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-120-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-frame-width (selected-frame) 120))

(defun set-2-windows ()
  (interactive)
  (set-frame-width (selected-frame) 242)
  (split-window-horizontally))

(global-set-key "\C-x~" 'set-120-columns)
(global-set-key "\C-x3" 'set-2-windows)


;; window
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))


;; display column number
;;(column-number-mode t)

;; display file size 
(size-indication-mode t)

;; display file path on title-bar
(setq frame-title-format "%f")

;; display line number on the left of the window
;; (global-linum-mode t)

;; display clock
;;(setq display-time-day-and-date t) ; week, month, day
;;(setq display-time-24hr-format t) ; 24h 
;;(display-time-mode t)

;; display battery charge
;;(display-battery-mode t)

;; display line count and word count of the region on mode-line
;; (only when region is designated)
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))

(add-to-list 'mode-line-format
             '(:eval (count-lines-and-chars)))

;; TAB width. default is 8
(setq-default tab-width 4)
;; not use tab character when indent
(setq-default indent-tabs-mode nil)

;; Indent of C、C++、JAVA、PHP, etc.
;;(add-hook 'c-mode-common-hook
;;          '(lambda ()
;;             (c-set-style "bsd")))

;; set region's background color
;(set-face-background 'region "darkgreen")

;; Setting display theme
;; http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz
(when (require 'color-theme nil t)
  ;; setting for reading theme
  (color-theme-initialize)
  ;; change color-theme to 'hober'
  (color-theme-hober))

;; Font setting ( for Mac and Windows)
(when (eq window-system 'ns) ; for Mac
  ;; ascii font to Menlo font
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 120)
  ;; set Japanese font as Hiragino Mincyo Pro
  (set-fontset-font
   nil 'japanese-jisx0208
   ;; in the case of  English name
   (font-spec :family "Hiragino Mincho Pro"))
   ;;(font-spec :family "ヒラギノ明朝 Pro"))
  ;; Hiragana and Katakana font as MotoyaCedar
  ;; U+3000-303F	CJK's punctuations
  ;; U+3040-309F	Hiragana
  ;; U+30A0-30FF	Katakana
  (set-fontset-font
   nil '(#x3040 . #x30ff)
   (font-spec :family "NfMotoyaCedar"))
  ;; arrange font width
  (setq face-font-rescale-alist
        '((".*Menlo.*" . 1.0)
          (".*Hiragino_Mincho_Pro.*" . 1.2)
          (".*nfmotoyacedar-bold.*" . 1.2)
          (".*nfmotoyacedar-medium.*" . 1.2)
          ("-cdac$" . 1.3))))

(when (eq system-type 'windows-nt) ; for Windows
  ;; ascii font to Consolas
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 120)
  ;; Japanese font
  (set-fontset-font
   nil
   'japanese-jisx0208
   (font-spec :family "メイリオ"))
  ;; arrange font width
  (setq face-font-rescale-alist
        '((".*Consolas.*" . 1.0)
          (".*メイリオ.*" . 1.15)
          ("-cdac$" . 1.3))))



;;;;; Syntax highlight setting ;;;;;
;;; highlight current line 
(defface my-hl-line-face
  ;; if background is dark, highlight color is NavyBlue
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; if background is light, highlight color is NavyBlue
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode：display corresponde parentheses with emphasis
(setq show-paren-delay 0) ; time that elapses before display. default is 0.125
(show-paren-mode t) ; activate
;; paren's style: emphasis expression even if in parentheses
(setq show-paren-style 'expression)
;; change face
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified
                    :underline "yellow")



;;;;; Back-up & auto save ;;;;;
;; not making back-up file
;; (setq make-backup-files nil) ; default value is 't'
;; not making auto-save file 
;; (setq auto-save-default nil) ; default value is 't'

;; change directory of back-up file to system temp directory
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; change directory of auto-save file to system temp directory
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; not make the list of auto-save file
(setq auto-save-list-file-prefix nil)

;; collect back-up file and auto-save file to ~/.emacs.d/backups/
(add-to-list 'backup-directory-alist
             (cons "." "~/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/backups/") t)))

;; interval of making auto-save file (seconds)
(setq auto-save-timeout 15)
;; interval of making auto-save file (number of types)
(setq auto-save-interval 60)


;;;;; Hook ;;;;;
;; if file start with #!, add +x when saving file
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; define function foe emacs-lisp-mode-hook
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

;; set emacs-lisp-mode's hook
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)


;;;;; Install Elisp & Setting ;;;;;

;; setting of auto-install
(when (require 'auto-install nil t)
  ;; set install directory
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; get elisp's name which is registered at EmacsWiki
  (auto-install-update-emacswiki-package-name t)
  ;; be available to use function of install-elisp
  (auto-install-compatibility-setup))

;; install redo+.el by using auto-install
;; source downloaded from https://www.emacswiki.org/emacs/download/redo+.el
(when (require 'redo+ nil t)
  ;; set C-. as redo
  (global-set-key (kbd "C-.") 'redo)
  )

;; cua-mode setting
(cua-mode t) ; activate cua-mode
(setq cua-enable-cua-keys nil) ; deactivate CUA key-bind

;;;;; setting of memo & todo list - howm 
;; howm memo save directory
;;(setq howm-directory (concat user-emacs-directory "howm"))
;; set language of howm-menu Japanese
;;(setq howm-menu-lang 'ja)
;; make howm memo 1 file per day
;(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
;; read howm-mode
;(when (require 'howm-mode nil t)
;  ;; start up howm menu by C-c,,
;  (define-key global-map (kbd "C-c m") 'howm-menu))
;;; close howm memo at the same time of saving
;(defun howm-save-buffer-and-kill ()
;  "close howm memo at the same time of saving."
;  (interactive)
;  (when (and (buffer-file-name)
;             (string-match "\\.howm" (buffer-file-name)))
;    (save-buffer)
;    (kill-buffer nil)))
;;; close buffer simultaneously by C-c C-c when memo is saved
;(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-google-cpplint-command
   "~/.emacs.d/elpa/flymake-google-cpplint-20140205.525/cpplint.py")
 '(flymake-googlecpp-lint-linelength "80")
 '(google-translate-default-source-language "ja")
 '(google-translate-default-target-language "en")
 '(package-selected-packages
   (quote
    (dockerfile-mode docker-compose-mode yaml-mode exec-path-from-shell jedi cmake-ide flycheck-irony irony-eldoc company-irony irony cmake-mode slack google-translate flymake-google-cpplint helm magit flycheck auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;; setting of autopep8
;; autopep8 automatically correct the python code according to pep8 coding rule.
;; To use autopep8, running command "pip install autopep8" is needed.
;; Elisp file was downloaded at
;;       https://github.com/fujimisakari/py-autopep8.el.git
(require 'python)
(require 'py-autopep8)
(define-key python-mode-map (kbd "C-c F") 'py-autopep8)
(define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)
(add-hook 'before-save-hook 'py-autopep8-before-save)


;;;;; Insert parenthesis/brackets by pair 
(electric-pair-mode 1)


;;;;; Enable flycheck and set keybind
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(global-set-key (kbd "C-c C-f") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)


;;;;; flycheck-google-cpplint
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint))))

;;;;; google-c-style package
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;;;;;  setting of auto-complete
;(ac-config-default)
;;; select candidate with C-n/C-p
;(setq ac-use-menu-map t)
;(define-key ac-menu-map "\C-n" 'ac-next)
;(define-key ac-menu-map "\C-p" 'ac-previous)
;;; setting of auto-complete dictionary(cache) file save directory
;(setq ac-comphist-file "~/.emacs.d/cache/auto-complete/ac-comphist.dat")


;;;;;  magit keybind configuration
(global-set-key (kbd "C-x g") 'magit-status)


;;;;; cpplint
;; hook config
(require 'flymake-google-cpplint)
(add-hook 'c-mode-hook 'flymake-google-cpplint-load)
(add-hook 'c++-mode-hook 'flymake-google-cpplint-load)

;; cpplint config


;; path to cpplint.py



;;;;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "C-x b") 'helm-for-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)


;;;;;;; popwin configuration (to use google-translate)
;;(require 'popwin)
;;(setq display-buffer-function 'popwin:display-buffer)
;;(setq popwin:popup-window-position 'bottom)
;; 
;;;;;;; google translate
;;(require 'google-translate)
;;(global-set-key "\C-ct" 'google-translate-at-point)
;;(global-set-key "\C-cT" 'google-translate-query-translate)

;; set default source language as Japanese, and target language as English.


;; display pop-up translate buffer.
;;(push '("*Google Translate*") popwin:special-display-config)


(put 'downcase-region 'disabled nil)


;;;; C++ environment configuration : https://qiita.com/MitsutakaTakeda/items/2f526a85ad39424a8363
(use-package flycheck
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    )
  )

(use-package cmake-ide
  :bind
  (("<f9>" . cmake-ide-compile))
  :config
  (progn
    (setq 
     ; rdm & rcコマンドへのパス。コマンドはRTagsのインストール・ディレクトリ下。
     cmake-ide-rdm-executable "/home/kitamura/Downloads/build_rtag/bin/rdm"
     cmake-ide-rc-executable  "/home/kitamura/Downloads/build_rtag/bin/rc"
     )
    )
  )

(use-package rtags
  :config
  (progn
    (rtags-enable-standard-keybindings c-mode-base-map)
    ; 関数cmake-ide-setupを呼ぶのはrtagsをrequireしてから。
    (cmake-ide-setup)
    )
  )

;(defun my-irony-mode-hook ()
;  (define-key irony-mode-map
;    [remap completion-at-point]
;    'irony-completion-at-point-async)
;  (define-key irony-mode-map
;    [remap complete-symbol]
;    'irony-completion-at-point-async)
;  )
; 
;(use-package irony
;  :config
;  (progn
;    ; ironyのビルド&インストール時にCMAKE_INSTALL_PREFIXで指定したディレクトリへのパス。
;    (setq irony-server-install-prefix "/home/kitamura/.emacs.d/irony")
;    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;    (add-hook 'irony-mode-hook 'irony-eldoc)
;    (add-to-list 'company-backends 'company-irony)
;    )
;  )

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;;;;
(setq irony-lang-compile-option-alist
      (quote ((c++-mode . "c++ -std=c++11 -lstdc++")
              (c-mode . "c")
              (objc-mode . "objective-c"))))
;;; アドバイスによって関数を再定義。
;;; split-string によって文字列をスペース区切りでリストに変換
;;; (24.4以降 新アドバイス使用)
(defun ad-irony--lang-compile-option ()
  (defvar irony-lang-compile-option-alist)
  (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
    (when it (append '("-x") (split-string it "\s")))))
(advice-add 'irony--lang-compile-option :override #'ad-irony--lang-compile-option)
;; (24.3以前 旧アドバイス使用)
(defadvice irony--lang-compile-option (around ad-irony--lang-compile-option activate)
  (defvar irony-lang-compile-option-alist)
  (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
    (when it (append '("-x") (split-string it "\s")))))


(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c++-mode-hook      'irony-mode)
(add-hook 'after-init-hook 'global-company-mode)

;;; jedi (python omni completion)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

