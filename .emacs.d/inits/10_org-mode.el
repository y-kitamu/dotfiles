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
  (org-directory "~/.emacs.d/documents/projects")
  (org-startup-indented t)
  (org-indent-indentation-per-level 2)
  (org-startup-folded 'showall)
  (org-confirm-babel-evaluate nil) ; 評価時に確認メッセージをださない
  ;; (org-agenda-files (directory-files org-directory))
  (org-log-done 'time) ; DONE の時間を記録
  ;; org-agenda settings
  (org-agenda-skip-scheduled-if-done nil) ; agenda に DONE を表示しない
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s %b")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  :config
  (defun update-org-agenda-files ()
    (interactive)
    (setq org-agenda-files (-filter (lambda (file) (string= "org" (file-name-extension file)))
                                    (directory-files org-directory))))
  (update-org-agenda-files)
  ; python コードブロックを評価できるようにする
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  :bind
  ("C-c a" . 'org-agenda)
  ("C-c l" . 'org-store-link)
  (:map org-read-date-minibuffer-local-map
        ("C-f" . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-day 1))))
        ("C-b" . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-backward-day 1))))
        ("C-n" . (lambda () (interactive)
                   (org-eval-in-calendar '(calendar-forward-week 1))))
        ("C-p" . (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-week 1)))))
  ;; ("C-c e X" . 'org-publish-project)
  )

(defun override-org-html-src-block (src-block _contents info)
  "Jekyll ブログエクスポート用のコードブロック生成。
rougeでソースコードをhighlight、#+name: <filename> で指定したファイル名を表示する。
TODO:  roughのlangとemacs (org)のlangの表記の対応表の作成"
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	       (code (org-html-format-code src-block info))
	       (label (let ((lbl (and (org-element-property :name src-block)
				                  (org-export-get-reference src-block info))))
		            (if lbl (format " id=\"%s\"" lbl) "")))
	       (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
					                       "ruby" "scheme" "clojure" "php" "html"))))
           (name (org-element-property :name src-block))
           (prefix
            (format "<figure><figcaption class=\"figcaption\">%s</figcaption>\n{%% highlight %s %%}\n"
                    name lang))
           (suffix "{% endhighlight %}\n</figure>"))
      (concat prefix code suffix))))
(advice-add 'org-html-src-block :override 'override-org-html-src-block)

;;; open-junk-file setting
(use-package open-junk-file
  :straight t
  :config
  (setq open-junk-file-format "~/.emacs.d/documents/junk/%Y/%Y_%m_%d.org")
  :bind
  ("C-x j" . open-junk-file))

(setq org-publish-project-alist
      '(
        ("org-blog"
         :base-directory "~/Documents/kitamura.github.io/org/"
         :base-extension "org"
         :publishing-directory "~/Documents/kitamura.github.io/docs"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )
        ("org-blog-static"
         :base-directory "~/Documents/kitamura.github.io/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/Documents/kitamura.github.io/docs"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog" :components ("org-blog" "org-blog-static"))))

;;; 10_org-mode.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
