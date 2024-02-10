;;; 10_org-mode.el --- org-mode settings                  -*- lexical-binding: t; -*-

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

;; org-mode settings

;;; Code:

(use-package dash :straight t)

(use-package org
  :straight t
  :custom
  (org-log-done 'time) ; DONE の時間を記録
)

;;; open-junk-file setting
(use-package open-junk-file
  :straight t
  :config
  (setq open-junk-file-format "~/.emacs.d/documents/junk/%Y/%Y_%m_%d.org")
  :bind
  ("C-x j" . open-junk-file))

;;; 10_org-mode.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
