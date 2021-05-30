;;; 01_keybindings.el --- global keybind settings    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords: key-bind

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

;; global keybindings

;;; Code:

;;; text size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;; text input
(define-key global-map (kbd "<zenkaku-hankaku>") 'toggle-input-method)

;;; window の切替
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-o") (lambda ()
                                     (interactive)
                                     (other-window -1)))


;;; 01_keybindings.el ends here
