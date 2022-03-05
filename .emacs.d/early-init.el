;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Yusuke Kitamura

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

;; (setq ng-straight-bootstrap-at-startup t) ; enable emacs-ng's builtin use-package and straight

;; hide bars
(tool-bar-mode 0)   ; tool bar を非表示
(menu-bar-mode 0)   ; menu bar を非表示
(if window-system
    (scroll-bar-mode 0)) ; scroll bar を非表示

;;; early-init.el ends here
