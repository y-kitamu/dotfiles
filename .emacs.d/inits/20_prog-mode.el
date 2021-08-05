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
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.psl\\'" . powershell-mode)))

(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package protobuf-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package go-mode :ensure t)
(use-package typescript-mode :ensure t)

(use-package cuda-mode :ensure t)

;;; programing language major modes
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t))

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
  (setq web-mode-content-types-alist '(("javascript" . "\\.gs\\'"))) ; google app scripts file
  :after rainbow-mode
  :hook (web-mode . rainbow-mode))

;; js formatter
(use-package prettier-js
  :ensure t
  :hook (web-mode .))

;; glsl mode
(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode)))

(provide '20_prog-mode)
;;; 20_prog-mode.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
