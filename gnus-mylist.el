;;; gnus-mylist.el --- User tracking of read Gnus articles with helm  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-mylist
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1.0") (bbdb "3.1") (helm "3.1") (org "8.3") (s "0.0"))
;; Keywords: convenience, mail, gnus, helm, org

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;;
;; Gnus-mylist tracks your read Gnus articles to view with helm.
;;
;; This is "my list", so I can keep only the articles that are
;; important to me. The rest I can simply remove from mylist,
;; without affecting Gnus. If an article is removed, by accident or
;; I want it back for whatever reason, no problem. All I have to do
;; is view the article in gnus, and it is back on the list !
;;
;; Gnus mylist works in the background silently, keeping track of
;; the articles read with gnus. When an article is read, it adds it
;; to mylist. Simply, that's all. It removes deleted articles or
;; the ones expunged by gnus.
;;
;; Gnus-mylist is similar to the Gnus registry, but whereas the
;; registry tries to catch everything, gnus-mylist is light-weight.
;; It doesn't try to keep everything. Only the articles "I have"
;; read. Its job is much simpler. "My read" articles are the only
;; really important "to me" articles, isn't is so ?
;;
;; This simplicity allows the user to add and remove articles to
;; gnus-mylist, stress free.
;;
;; Viewing gnus-mylist with the powerful helm interface brings great
;; search capabilities and all the other helm goodness.
;; Gnus-mylist has been built around helm.
;;
;; To start using gnus-mylist, use the helm command 'helm-gnus-mylist'.
;; For quick access assign to a global key:
;;
;;     (global-set-key (kbd "C-c m") #'helm-gnus-mylist)
;;
;; Or add an option to your favorite hydra.
;;
;; Additional integration provided, or planned, with:
;; - org-mode, with gnus-mylist-org
;; - BBDB built-in with gnus (gnus-insinuate)
;; - EBDB (todo)
;;
;; Gnus is not limited to email, that is why gnus uses the term "articles".
;; Gnus-mylist follows the Gnus general philosophy, it also uses the term
;; "articles". Most testing has been done on email (and IMAP in particular) and RSS.
;;
;; This package is a fork of gnus-recent with additional inspiration by gnorb.

;;; To use, require:
;;
;; (require 'gnus-mylist)
;;

;;; Code:

(require 'gnus-mylist-lib)
(require 'helm-gnus-mylist)
(require 'gnus-mylist-org)

(defgroup gnus-mylist nil
  "Article breadcrumbs for gnus."
  :tag "Gnus Mylist"
  :group 'gnus)

(defcustom gnus-mylist-top-dir (concat user-emacs-directory "gnus-mylist/")
  "The parent directory for gnus-mylist files."
  :group 'gnus-mylist
  :type 'directory)

(defcustom gnus-mylist-file  (concat gnus-mylist-top-dir "articles.el")
  "A file to save the gnus mylist articles list data.
Set to nil, for no article tracking between gnus sessions.
Otherwise, best to keep this file under `gnus-mylist-top-dir'."
  :group 'gnus-mylist
  :type 'file)

(defcustom gnus-mylist-breadcrumbs-dir (concat gnus-mylist-top-dir "crumbs/")
  "A directory for keeping article breadcrumbs in between saves.
Used only when `gnus-mylist-file' is non-nil."
  :group 'gnus-mylist
  :type 'directory)

(defcustom gnus-mylist-format-time-string "%F %a %T"
  "A string for formating the article date.
The format is used by `format-time-string'. See its documentation
for details on format specifiers. For example, to produce a full
ISO 8601 format, use \"%FT%T%z\", for org style use \"%F %a %T\".
Changing this variable affects only new entries. Previous entries
keep the old format."
  :group 'gnus-mylist
  :type 'string)

;; start gnus-mylist session
(gnus-mylist-start)

(provide 'gnus-mylist)
;;; gnus-mylist.el ends here
