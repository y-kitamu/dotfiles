;;; 20_prog-mode.el --- programming language modes settings            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords: extensions

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

(add-to-list 'auto-mode-alist '("\\.bash.*\\'" . sh-mode))
;;; for windows
(use-package powershell
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.psl\\'" . powershell-mode)))

(use-package dockerfile-mode :straight t)
(use-package docker-compose-mode :straight t)
(use-package cmake-mode :straight t)
(use-package protobuf-mode :straight t)
(use-package csv-mode :straight t)
(use-package go-mode :straight t)

(use-package cuda-mode :straight t)

;;; programing language major modes
(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  :after rust-mode)

(use-package flycheck-rust
  :straight t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;;; typescript-modeの設定
;;; typescript-modeでlsp-dockerを使用する場合はhost側でnpm install -g typescriptする必要あり
(use-package typescript-ts-mode
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package treesit-auto
  :straight t
  :init
  (require 'treesit-auto)
  (global-treesit-auto-mode)
  :config
  (setq treesit-auto-install t))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-ts-mode . tree-sitter-hl-mode)
         (tsx-ts-mode . tree-sitter-hl-mode))
  :config
  (setq treesit-font-lock-level 4)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode)))

;; web mode setting
(use-package web-mode
  :straight t
  :mode
  (  ("\\.html\\'" . web-mode)
  ("\\.css\\'" . web-mode)
  ;; ("\\.js\\'" . web-mode)
  ;; ("\\.ts\\'" . web-mode)
  ("\\.gs\\'" . web-mode)
  ;; ("\\.jsx\\'" . web-mode)
  ;; ("\\.tsx\\'" . web-mode)
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
  (setq web-mode-markup-indent-offset 2) ; HTML の Indent
  (setq web-mode-css-indent-offset 2)  ; CSS の Indent
  (setq web-mode-code-indent-offset 2) ; JS, PHP, Ruby などの Indent
  (setq web-mode-style-padding 1)      ; <style>内の Indent
  (setq web-mode-script-padding 1)    ; <script>内の Indent
  ;; (add-hook 'web-mode-hook 'web-mode-hook)
  ;; (setq web-mode-content-types-alist
  ;;       '(("jsx" . "\\.js[x]?\\'")
  ;;         ("jsx" . "\\.ts[x]?\\'")))    ; 拡張子 .js でもJSX編集モードにする
  ;; (setq web-mode-content-types-alist '(("javascript" . "\\.gs\\'"))) ; google app scripts file
  :after (rainbow-mode)
  :hook (web-mode . rainbow-mode))

;; js formatter
(use-package prettier-js
  :straight t
  :hook ((web-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (javascript-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)))

;; glsl mode
(use-package glsl-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.gs\\'" . glsl-mode)))
  (define-key glsl-mode-map (kbd "<backtab>") 'copilot-accept-completion)
  (define-key glsl-mode-map (kbd "S-<iso-lefttab>") 'copilot-accept-completion)

;;; python
(setq-default python-indent-offset 4)

(use-package blackify
  :straight (blackify :type git :host github :repo "y-kitamu/blackify")
  :hook (python-mode . blackify-mode))

(use-package py-isort
  :demand t
  :init
  (defun toggle-py-isort-before-save ()
    (interactive)
    (if (memq 'py-isort-before-save before-save-hook)
        (progn
          (remove-hook 'before-save-hook 'py-isort-before-save)
          (message "py-isort-before-save disabled"))
      (add-hook 'before-save-hook 'py-isort-before-save)
      (message "py-isort-before-save enabled")))
  :hook
  (before-save . py-isort-before-save))

;;; c/c++
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

(use-package srefactor
  :straight t
  :config
  (require 'srefactor-lisp)
  (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
  (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
  (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
  (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer))

;;; Latex
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



(provide '20_prog-mode)
;;; 20_prog-mode.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
