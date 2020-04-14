;;; gnus-mylist-ivy.el --- select mylistly read Gnus articles with ivy -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; Version: 0.3.0
;; URL: https://github.com/deusmax/gnus-mylist
;; Package-Requires: ((emacs "26.0.0"))
;; Keywords: convenience, mail

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gnus-recent)
(require 'ivy)

(defun gnus-recent-ivy ()
  "Select a recent Gnus article to open with `ivy'."
  (interactive)
  (ivy-read "Recent article: "
            gnus-recent--articles-list
            :action #'gnus-recent--open-article
            :require-match t))

(ivy-add-actions #'gnus-recent-ivy
                 '(("l" gnus-recent-insert-org-link "insert org link")
                   ("c" gnus-recent-kill-new-org-link "copy org link")
                   ("k" gnus-recent-forget "forget")
                   ("K" gnus-recent-forget-all "forget all")))


(provide 'gnus-recent-ivy)
;;; gnus-recent-ivy.el ends here
