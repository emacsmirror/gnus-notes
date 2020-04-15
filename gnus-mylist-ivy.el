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

(require 'gnus-mylist)
(require 'ivy)

(defun gnus-mylist-ivy ()
  "Select a mylist Gnus article to open with `ivy'."
  (interactive)
  (ivy-read "Mylist article: "
            gnus-mylist--articles-list
            :action #'gnus-mylist--open-article
            :require-match t))

(ivy-add-actions #'gnus-mylist-ivy
                 '(("l" gnus-mylist-insert-org-link "insert org link")
                   ("c" gnus-mylist-kill-new-org-link "copy org link")
                   ("k" gnus-mylist-forget "forget")
                   ("K" gnus-mylist-forget-all "forget all")))


(provide 'gnus-mylist-ivy)
;;; gnus-mylist-ivy.el ends here
