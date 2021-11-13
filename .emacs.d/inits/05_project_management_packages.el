;;; 05_project_management_packages.el --- settings for project management packages  -*- lexical-binding: t; -*-

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

;;; magit
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  ;; (when (equal system-type 'windows-nt) ;; windows の場合、git の .exe file の場所を指定
  ;;   (setq magit-git-executable "c:/Users/y-kitamura/AppData/Local/Programs/Git/bin/git.exe"))
  (defun unpackaged/magit-log--add-date-headers (&rest _ignore)
    "Add date headers to Magit log buffers."
    (when (derived-mode-p 'magit-log-mode)
      (save-excursion
        (ov-clear 'date-header t)
        (goto-char (point-min))
        (cl-loop with last-age
                 for this-age =
                 (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                   car
                   (overlay-get it 'before-string)
                   (get-text-property 0 'display it)
                   cadr
                   (s-match (rx (group (1+ digit) ; number
                                       " " (1+ (not blank))) ; unit
                                (1+ blank) eos) it)
                   cadr)
                 do (when (and this-age (not (equal this-age last-age)))
                      (ov (line-beginning-position) (line-beginning-position)
                          'after-string (propertize (concat " " this-age "\n")
                                                    'face 'magit-section-heading)
                          'date-header t)
                      (setq last-age this-age))
                 do (forward-line 1)
                 until (eobp)))))

  (define-minor-mode unpackaged/magit-log-date-headers-mode
    "Display date/time headers in `magit-log' buffers."
    :global t
    (if unpackaged/magit-log-date-headers-mode
        (progn
          ;; Enable mode
          (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
          (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
      ;; Disable mode
      (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
      (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers)))
  :bind
  ("C-x g" . magit-status))

(use-package helm-ls-git
  :ensure t)

;;; reference : https://emacs-jp.github.io/packages/git-gutter
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:lighter "")
  (global-git-gutter-mode t))

;; (use-package forge
;;   :ensure t
;;   :after magit)

(use-package projectile
  :ensure t
  :init
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-mode-line-prefix nil)
  (setq projectile-completion-system 'helm)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p f") 'helm-projectile)
  :after projectile)

(provide '05_project_management_packages)
;;; 05_project_management_packages.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
