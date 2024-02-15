;;; 11_emacs-lisp-mode.el --- emacs-lisp-mode settings  -*- lexical-binding: t; -*-

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

;; lisp の評価結果を注釈する
(use-package lispxmp
  :straight t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d" . 'lispxmp)))

;; カッコの対応を保持して編集する設定
(use-package smartparens
  :straight t
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (lisp-interaction-mode . smartparens-strict-mode)
   (ielm-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (setf (cdr (assoc "C-<right>" sp-smartparens-bindings)) 'sp-forward-sexp)
  (setf (cdr (assoc "C-<left>" sp-smartparens-bindings)) 'sp-backward-sexp)
  (sp-use-smartparens-bindings)
)

(use-package auto-async-byte-compile
  :straight t
  :custom
  (auto-async-byte-compile-exclude-files-regexp ".dir-locals.el\\|/junk/\\|init.el\\|/elpa/")
  :hook
  ((emacs-lisp-mode . enable-auto-async-byte-compile-mode)
   (emacs-lisp-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (ielm-mode . turn-on-eldoc-mode))
  :after eldoc)

(use-package dash
  :straight t
  :custom
  (global-dash-fontify-mode)
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

(provide '11_emacs-lisp-mode)
;;; 11_emacs-lisp-mode.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
