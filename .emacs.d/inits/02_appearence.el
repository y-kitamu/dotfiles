;;; 02_appearence.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords:

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

(use-package tab-bar
  :straight t
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

(defmacro my/hide-minor-mode-from-mode-line (mode)
  "指定したMODEをmode-lineに表示しないようにする。
MODEはsymbolを指定する。
ex. (my/hide-minor-mode-from-mode-line 'rainbow-mode)"
  `(setq minor-mode-alist (cons (list ,mode "") (assq-delete-all ,mode minor-mode-alist))))

;;; `message` の出力の先頭に日時を付け足す
(defadvice message (before before-message activate)
  (let ((original (ad-get-arg 0))
        (now (decode-time (current-time))))
    (ad-set-arg 0 (format "[%04d/%02d/%02d %02d:%02d:%02d] %s"
                          (nth 5 now) (nth 4 now) (nth 3 now) (nth 2 now) (nth 1 now) (nth 0 now)
                          original))))


(global-hl-line-mode t)

;;; Theme
(use-package zenburn-theme
  :straight t
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

(use-package col-highlight
  :straight (col-highlight :type git :host github :repo "emacsmirror/col-highlight")
  :config
  (toggle-highlight-column-when-idle 0))

;;; mode-lineの見た目をおしゃれにする
(use-package powerline
  :straight t
  :config
  (custom-set-faces
   '(powerline-active1 ((t (:background  "#93E0E3" :foreground "#2B2B2B"))))
   '(powerline-active2 ((t (:background  "#303030"))))
   '(powerline-inactive2 ((t (:background  "#383838"))))
   )
  (powerline-default-theme))

(use-package total-lines
  :straight t
  :config
  (global-total-lines-mode)
  (defun my-set-line-numbers ()
    (setq-default mode-line-front-space
                  (append mode-line-front-space
                          '((:eval (format " (%d)" (- total-lines 1)))))))
  (add-hook 'after-init-hook 'my-set-line-numbers))

(use-package cursor-chg
  :straight (cursor-chg :type git :host github :repo "emacsmirror/cursor-chg")
  :custom
  (curchg-default-cursor-type 'box)
  (curchg-change-cursor-on-input-method-flag t)
  (curchg-input-method-cursor-color "green")
  (curchg-overwrite/read-only-cursor-type 'hollow)
  :config
  (change-cursor-mode 1))

;;; 02_appearence.el ends here
