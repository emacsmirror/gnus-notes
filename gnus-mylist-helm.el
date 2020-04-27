;;; gnus-mylist-helm.el --- Gnus mylist articles with helm -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-mylist-helm
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1.0") (bbdb "3.1") (helm "3.1") (hydra "0.13.0") (org "8.3") (s "0.0") (lv "0.0") (async "1.9.1"))
;; Keywords: convenience, mail, gnus, helm, org, hydra

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Keep handy notes of your read Gnus articles with helm and org.
;;
;; Viewing gnus-mylist with the powerfull helm interface brings powerful
;; search features and all the other goodness provided by helm.
;; This file provides the helm interface for viewing gnus-mylist.
;;
;; This is "my list", so I can keep only the articles that are
;; important to me. The rest I can simply remove from mylist,
;; without affecting Gnus. If an article is removed, by accident or
;; I want it back for whatever reason, no problem. All I have to do
;; is view the article in gnus, and it is back on the list !
;;
;; Gnus mylist works in the background silently, keeping track of the
;; articles read with gnus. When an article is read, it adds a quick
;; note of it to mylist. Simply, that's all. It removes notes of
;; deleted articles or the ones expunged by gnus.
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
;; Additional integration provided, or planned, with:
;; - org-mode, built-in
;; - BBDB built-in with gnus (gnus-insinuate 'bbdb 'message)
;; - EBDB (todo)
;;
;; Gnus is not limited to email, that is why gnus uses the term "articles".
;; Gnus-mylist follows the Gnus general philosophy, it also uses the term
;; "articles". Most testing has been done on email (and IMAP in particular) and RSS.
;;
;; This package is a fork of gnus-recent with additional inspiration by gnorb.
;;
;;; To use, require:
;;
;; (require 'gnus-mylist-helm)
;; (gnus-mylist-init)
;;
;; For quick access assign the helm starting-point to a global key:
;;
;;     (global-set-key (kbd "C-c m") #'gnus-mylist-helm)
;;
;; Or add an option to your favorite hydra.
;; To start using gnus-mylist, use the helm command 'gnus-mylist-helm'.
;; For quick access assign to a global key:
;;
;;     (require 'gnus-mylist-helm)
;;     (gnus-mylist-init)
;;     (global-set-key (kbd "C-c m") #'gnus-mylist-helm)
;;
;; Or add an option to your favorite hydra.
;;
;; For org-mode integration, activate the key bindings:
;;     (gnus-mylist-org-define-key)       ; default "C-c t"
;;

;;; Code:

(require 'gnus)
(require 'helm)
(require 'hydra)
(require 'async)
(require 'lv)
(require 'gnus-mylist)
(require 'gnus-mylist-org)

(defvar gnus-mylist-helm-display-extra nil
  "Display extra article info.")

(defvar gnus-mylist-helm-display-levels '(To Cc Msgid Orgid nil)
  "Display levels for extra article info.")

(defvar gnus-mylist-helm-current-data-pa nil
  "Keeps the article for the persistent action Hydra.")

(defvar gnus-mylist-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<up>") 'gnus-mylist-helm-display-cycle)
    map)
  "Keymap for a `helm' source.")

(defun gnus-mylist-helm-display-cycle ()
  "Cycle the levels of article info to display.
The user interactively changes the article infromation display
level. Currently only the \"default\", \"To\" and \"Cc\" levels
are implemented. The function will refresh the `helm' buffer to
display the new level."
  (interactive)
  (setq gnus-mylist-helm-display-extra (pop gnus-mylist-helm-display-levels))
  (nconc gnus-mylist-helm-display-levels (list gnus-mylist-helm-display-extra))
  (helm-refresh))

(defun gnus-mylist-helm-display-select ()
  "Select the level of article info to display.
The user selects the article infromation display level. Currently only the
  \"default\", \"To\" and \"Cc\" levels are implemented.
The function will refresh the `helm' buffer to display the new level."
  (interactive)
  (let ((display (read-char "Show level: default (d) | To (t) |  Cc (c): ")))
    (cond
     ((eq display ?t) (setq gnus-mylist-helm-display-extra 'To))
     ((eq display ?c) (setq gnus-mylist-helm-display-extra 'Cc))
     (t (setq gnus-mylist-helm-display-extra nil))))
  (helm-refresh))

(defun gnus-mylist-helm-candidate-transformer (candidates source)
  "Transform the `helm' data for the display level selected.
This function acts on the filtered data, to show the selected
display inforrmation. CANDIDATES is the list of filtered data.
SOURCE is the `helm' source. Both argumente are passed by `helm',
when the function is called. Also the `helm' multiline feature is
turned on and off, as needed."
  (pcase gnus-mylist-helm-display-extra
    ('To (helm-attrset 'multiline nil source)
         (mapcar #'gnus-mylist-helm-candidates-display-to candidates))
    ('Cc (helm-attrset 'multiline nil source)
         (mapcar #'gnus-mylist-helm-candidates-display-cc candidates))
    ('Msgid (helm-attrset 'multiline nil source)
            (mapcar #'gnus-mylist-helm-candidates-display-msgid candidates))
    ('Orgid (helm-attrset 'multiline nil source)
            (mapcar #'gnus-mylist-helm-candidates-display-orgid candidates))
    (_ (assq-delete-all 'multiline source)
       candidates)))

;; FIXME: should be using gnus-group-decoded-name
(defun gnus-mylist-helm-candidates-display-default (item)
  "The default text to display for each article.
Is the function argument to `mapcar' for establishing the
candidates default text. `Helm' uses this text to filter the
articles list.
For each article data ITEM in `gnus-mylist--articles-list' it
returns a cons cell (name . item), where name is the article
display text."
  (cons (concat (car item)
                " ["
                (propertize (gnus-mylist-decode-utf8 (alist-get 'group item))
                            'face 'gnus-mylist-group-face)
                "]")
        item))

(defun gnus-mylist-helm-candidates-display-to (item)
  "Display the To field for each article on a separate line.
ITEM is the article data in `gnus-mylist--articles-list'. Used as the function
  argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item)))
        item))

(defun gnus-mylist-helm-candidates-display-cc (item)
  "Display the To and Cc fields for each article on separate lines.
ITEM is the article data in `gnus-mylist--articles-list'. Used as the function
argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipients item))
                    (concat "\n    Cc: " it)))
        item))

(defun gnus-mylist-helm-candidates-display-msgid (item)
  "Display the To, Cc and message-id fields for each article on separate lines.
ITEM is the article data in `gnus-mylist--articles-list'. Used as the function
argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipients item))
                    (concat "\n    Cc: " it))
                "\n    MsgID: "
                (alist-get 'message-id item))
        item))

(defun gnus-mylist-helm-candidates-display-orgid (item)
  "Rotate the display of article fields.
The displayed fields are the To, Cc, message-id and org-id for
each article. Each is shown on a separate line. ITEM is the
article data in `gnus-mylist--articles-list'. Used as the
function argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipients item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipients item))
                    (concat "\n    Cc: " it))
                "\n    MsgID: "
                (alist-get 'message-id item)
                (helm-aif (alist-get 'org-id item)
                    (concat "\n    orgid: "
                            (symbol-name it)))) ; FIXME: org-id should be text, not symbols
        item))


(defun gnus-mylist-helm-candidates (articles-list)
  "Initialize the `helm' candidates data.
ARTICLES-LIST is the data list of arcticles for helm."
  (mapcar #'gnus-mylist-helm-candidates-display-default articles-list))

(defun gnus-mylist-helm-forget (_artlistitem)
  "Remove Gnus articles from `gnus-mylist--articles-list' using `helm'.
Helm allows for marked articles or current selection.  See
function `helm-marked-candidates'.  Argument _artlistitem is not used."
  (let* ((cand (helm-marked-candidates))
         (l1-cand (length cand))
         (l1-gral (length gnus-mylist--articles-list)))
    (dolist (article cand)
      (gnus-mylist-forget article t))
    (gnus-message 4 "Removed %d of %d article(s) from gnus-mylist done."
                  (- l1-gral (length gnus-mylist--articles-list))
                  l1-cand)))

(defun gnus-mylist-helm-forget-pa (artdata)
  "Forget current or marked articles without quiting `helm'.
This is the persistent action defined for the helm session.
Argument ARTDATA is the article data."
  (gnus-mylist-helm-forget artdata)
  (helm-delete-current-selection)
  (helm-refresh))

(defhydra hydra-gnus-mylist-helm (:columns 3 :exit nil)
  "Persistent actions"
  ("c" (gnus-mylist-kill-new-org-link gnus-mylist-helm-current-data-pa) "Copy Org link")
  ("b" (gnus-mylist-bbdb-display-all gnus-mylist-helm-current-data-pa) "BBDB entries")
  ("K" (gnus-mylist-helm-forget-pa gnus-mylist-helm-current-data-pa) "Forget current")
  ("{" helm-enlarge-window "helm window enlarge")
  ("}" helm-narrow-window "helm window narrow")
  (">" helm-toggle-truncate-line "helm toggle truncate lines")
  ("_" helm-toggle-full-frame "helm toggle full frame")
  ("Y" helm-yank-selection "helm yank selection")
  ("U" helm-refresh "helm update source")
  ("q" nil "quit" :exit t))

(defun gnus-mylist-helm-hydra-pa (artdata)
  "Persistent action activates a Hydra.
ARTDATA is the current article in the helm buffer."
  (setq gnus-mylist-helm-current-data-pa artdata)
  (hydra-gnus-mylist-helm/body))

;;;###autoload
(defun gnus-mylist-helm ()
  "Use `helm' to filter the Gnus mylist articles.
Also a number of possible actions are defined."
  (interactive)
  (helm :sources (helm-build-sync-source "Gnus mylist articles"
                   :keymap gnus-mylist-helm-map
                   :candidates (lambda () (gnus-mylist-helm-candidates gnus-mylist--articles-list))
                   :filtered-candidate-transformer  'gnus-mylist-helm-candidate-transformer
                   :persistent-action 'gnus-mylist-helm-hydra-pa
                   :persistent-help "quick actions"
                   :action '(("Open article"               . gnus-mylist--open-article)
                             ("Reply article"              . gnus-mylist--reply-article-wide-yank)
                             ("Show thread"                . gnus-mylist--show-article-thread)
                             ("Copy org link to kill ring" . gnus-mylist-kill-new-org-link)
                             ("Insert org link"            . gnus-mylist-insert-org-link)
                             ("Remove marked article(s)"   . gnus-mylist-helm-forget)
                             ("Display BBDB entries"       . gnus-mylist-bbdb-display-all)
                             ("Clear all"                  . gnus-mylist-forget-all)))
        :buffer "*helm gnus mylist*"
        :truncate-lines t))

(provide 'gnus-mylist-helm)
;;; gnus-mylist-helm.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
