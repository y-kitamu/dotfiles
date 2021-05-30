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

;;; text input (Japanese)
(define-key global-map (kbd "<zenkaku-hankaku>") 'toggle-input-method)

;;; toggle window
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-o") (lambda ()
                                     (interactive)
                                     (other-window -1)))

;;; background alpha
;;; TODO: move function definitions to proper place.
(defun set-alpha (alpha-num)
  "Set frame parameter 'alpha to ALPHA-NUM."
  (interactive "nAlpha : ")
  (set-frame-parameter nil 'alpha alpha-num))

(defun alpha-increase ()
  "Increase background alpha."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-alpha (min 1.0 (+ alpha 0.1)))))

(defun alpha-decrease ()
  "Decrease background alpha."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-alpha (max 0.0 (- alpha 0.1)))))

(set-alpha 0.9)
(define-key global-map (kbd "C-M-+") #'alpha-increase)
(define-key global-map (kbd "C-M-=") #'alpha-decrease)

;;; 01_keybindings.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
