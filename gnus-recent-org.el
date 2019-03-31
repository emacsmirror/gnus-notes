;;; gnus-recent-org.el --- Gnus Recent -*- lexical-binding: t -*-

;; Copyright (C) 2019 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; Version: 0.1.0
;; URL: https://github.com/deusmax/gnus-recent
;; Package-Requires: ((emacs "25.3.2"))
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

;;; Keep track of your seen messages, using a helm interface. Guns-recent provides
;;; an interface with minimun configuration. It should "just work".
;;; This file provides integration with your Org-mode TODO headings. It is inspired
;;; by Gnorb.

;;; Code:

(require 's)
(require 'gnus-recent)
(require 'org-gnus)

(defvar gnus-recent--current-org-id nil
  "Internal variable; for temporarily placing the current heading org-id.")

(defvar gnus-recent-org--current-heading-alist nil
  "Internal variable; for temporarily placing the current heading info.")

;; (org-defkey org-mode-map (kbd "C-c t") #'hydra-gnus-recent-org-handle-mail-top)
;; (org-defkey org-mode-map (kbd "C-c v") #'gnus-recent-org-view)
;; (eval-after-load "org-agenda"
;;   '(progn (org-defkey org-agenda-mode-map (kbd "C-c t") #'gnus-recent-org-handle-mail)
;;           (org-defkey org-agenda-mode-map (kbd "C-c v") #'gnus-recent-org-view)))

(defhydra gnus-recent-org-trigger-actions (:color blue :columns 2)
  "List of actions on org-headings."
  ("t" gnus-recent-org-todo "change Todo")
  ("n" gnus-recent-org-note-add "add Note")
  ("a" gnus-recent-org-associate  "only Associate")
  ("c" gnus-recent-org-capture-child "capture to Child")
  ("s" gnus-recent-org-capture-sibling "capture to Sibling")
  ("q" nil "Quit, don't associate"))

(defun gnus-recent-org-handle-mail-top ()
  "Reply to the top email message on the current org headline.
The body of the org heading must have at least one gnus link to
reply to."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (let* ((entry-text (gnus-string-remove-all-properties (org-get-entry)))
         (org-links-gnus (gnus-recent-org-search-string-org-links-gnus entry-text)))
    (message-box "Number of links: %d\nTop link: %s\nSender: %s\n"
                 (length entry-links)
                 (car-safe entry-links)
                 (if (> (length entry-links) 0)
                     "Somebody"         ; (alist-get 'sender (car entry-links))
                   "Nobody"))))

(defun gnus-recent-org-handle-mail-view ()
  "Show available gnus messages on the current org headline.
The messages will be shown in a Gnus ephemeral group."
  (interactive)
  (let ((org-links-gnus (gnus-recent-org-heading-get-org-links-gnus)))
    (message-box "Number of links: %d\nTop link: %s\nSender: %s\n"
                 (length org-links-gnus)
                 (car-safe org-links-gnus)
                 (if (> (length org-links-gnus) 0)
                     "Somebody"         ; (alist-get 'sender org-links-gnus)
                   "Nobody"))))

(defun gnus-recent-org-get-heading-alist ()
  "Get the text of a org heading and extract the info needed."
  (let* ((hd-txt (gnus-recent-org-get-entry))
         (org-links-gnus (gnus-recent-org-search-string-org-links-gnus hd-txt))
         (orgids (gnus-recent-org-get-orgids hd-txt))
         (articles-msgid (mapcar 'cdr
                                 (mapcar 'gnus-recent-split-org-link-gnus
                                         org-links-gnus)))
         (articles-msgid-by-org-ids (gnus-recent-org-get-heading-message-ids
                                     orgids)))
  (list (cons 'entry-text hd-txt)
        (cons 'org-links-gnus org-links-gnus)
        (cons 'orgids orgids)
        (cons 'orgid-top (car orgids))
        (cons 'articles-msgid articles-msgid)
        (cons 'articles-msgid-by-org-ids articles-msgid-by-org-ids))))

(defun gnus-recent-org-set-heading-alist ()
  "Save the heading info needed to `gnus-recent-org--current-heading-alist'."
  (interactive)
  (setq gnus-recent-org--current-heading-alist
        (gnus-recent-org-get-heading-alist)))

(defun gnus-recent-org-clear-heading-alist ()
  "Clear all data from variable `gnus-recent-org--current-heading-alist'."
  (interactive)
  (setq gnus-recent-org--current-heading-alist nil))

// FIXME: Exploratory code, need major overhaul.
(defun gnus-recent-org-message-sent-actions ()
  "Tidy up after an outgoing message is sent."
  (org-with-point-at my-point-marker
    (org-back-to-heading)
    (message-box "org-ID: %s\n" (org-id-get-create))
    (org-add-log-setup 'note nil nil nil "[[gnus:INBOX#mylifeasadog@home.gr][This is a message email]]"))
  (gnus-recent-org-clear-heading-alist))

(add-hook 'message-sent-hook 'gnus-recent-org-message-sent-actions t)

(defun gnus-recent-org-handle-mail-crumbs ()
  "Show available gnus messages from the current org headline in helm."
  (interactive)
  (helm
   :sources (helm-build-sync-source "Heading articles"
              :keymap gnus-recent-helm-map
              :candidates (lambda ()
                            (gnus-recent-helm-candidates
                             (alist-get 'articles-msgid-by-org-ids
                                        gnus-recent-org--current-heading-alist)))
              :filtered-candidate-transformer  'gnus-recent-helm-candidate-transformer
              :persistent-action 'gnus-recent-org-helm-hydra-pa
              :persistent-help "view hydra"
              :action '(("Open article"               . gnus-recent--open-article)
                        ("Wide reply and yank"        . gnus-recent--reply-article-wide-yank)
                        ("Show thread"                . gnus-recent--show-article-thread)
                        ("Copy org link to kill ring" . gnus-recent-kill-new-org-link)
                        ("Display BBDB entries"       . gnus-recent-bbdb-display-all)))
   :buffer "*helm org heading articles*"
   :truncate-lines t))

;; (org-get-entry)
;; (setq x (gnus-string-remove-all-properties x))
;; (s-match-strings-all "\\[\\[gnus:.+\\]\\]" x)
;; s-match-strings-all "\\[\\[gnus:.+\\]\\]"
"\\[\\[\\(\\(gnus\\)\\|\\(bbdb\\)\\):.+\\]\\]"

;; <HE1PR0702MB374007926A69A5BA9F469833B7700@HE1PR0702MB3740.eurprd07.prod.outlook.com> OR <1859946832.1551532226619.JavaMail.root@7e5afac02a08> OR <87wolg9jyd.fsf@aia00054aia.gr> OR <871s3ob518.fsf@aia00054aia.gr> OR <43fc0c0fce9292d8bed09ca27.b1ed948b21.20190302133324.73f362d72f.17307e8e@mail197.sea51.mcsv.net>

(defun gnus-recent-org-handle-mail ()
  "Handle mail in org heading"
  (interactive)
  (gnus-recent-org-set-heading-alist)
  (hydra-gnus-recent-org-handle-mail/body))

(defhydra hydra-gnus-recent-org-handle-mail (:color blue
                                             :pre (message-box "Org Handle mail-Pre\n")
                                             :post (message-box "Org Handle Mail-Post\n"))
  "Reply to email from current task"
  ("t" gnus-recent-org-handle-mail-top "Reply to top")
  ("v" gnus-recent-org-handle-mail-view "View emails")
  ("h" gnus-recent-org-handle-mail-crumbs "View in helm")
  ("q" gnus-recent-org-clear-heading-alist "quit"))

(define-key org-mode-map (kbd "C-c t") 'gnus-recent-org-handle-mail)

(defun gnus-recent-org-get-entry (&optional keep-properties)
  "Get the entry text"
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (if keep-properties
      (org-get-entry)
    (gnus-string-remove-all-properties (org-get-entry))))

(defun gnus-recent-org-heading-get-org-links-gnus ()
  "Get the org-links under the current heading."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (gnus-recent-org-search-string-org-links-gnus (org-get-entry)))

(defun gnus-recent-org-search-string-org-links-gnus (txt)
  "Search text TXT for org-links, having protocol \"gnus:\".
Returns a list of org-links, that point to gnus articles."
  (mapcar 'car
          (s-match-strings-all "\\[\\[gnus:.+\\]\\]"
                               (gnus-string-remove-all-properties txt))))

(defun gnus-recent-org-get-orgids (txt)
  "Find the org-ids in text TXT."
  (mapcar (lambda (x) (cadr (split-string x ":" t " +")))
          (mapcar 'car
                  (s-match-strings-all "^ +:ID:.+" txt))))

(defun gnus-recent-org-get-heading-message-ids (id-list)
  "Get the article message-id that have any of the given org-ids.
ID-LIST is a list of org-ids to search in `gnus-recent--articles-list'. Returns a
combined list of all article message-ids found."
  ;; FIXME: use equal for the test
  (mapcan '(lambda (id) (gnus-recent-filter-prop 'org-id id #'string=))
          id-list))

(defun gnus-recent-org-message-add-header (header value)
  "Add a HEADER when composing a new message.
VALUE is the value for the header."
  (when (and value (derived-mode-p 'message-mode 'mail-mode))
    (save-excursion
      (save-restriction
        (message-narrow-to-headers-or-head)
        (open-line 1)
        (message-insert-header header value)))))

(defun gnus-recent-org-message-add-header-orgid (&optional orgid)
  "Add an X-Org-Id header to an outgoing message.
When the optional argument ORGID is missing, will get the orgid
value from `gnus-recent-org-get-heading-alist'."
  (gnus-recent-org-message-add-header
   'X-Org-Id (or orgid
                 (car (alist-get 'orgids gnus-recent-org--current-heading-alist)))))

(defhydra hydra-gnus-org-helm (:columns 4 :exit nil
                               :pre  (message-box "Org-helm-PA-Pre\n")
                               :post (message-box "Org-helm-PA-Post\n"))
  "Persistent actions"
  ("c" (gnus-recent-kill-new-org-link gnus-recent-helm-current-data-pa) "Copy Org link")
  ("b" (gnus-recent-bbdb-display-all  gnus-recent-helm-current-data-pa) "BBDB entries")
  ("{" helm-enlarge-window "enlarge")
  ("}" helm-narrow-window "narrow")
  (">" helm-toggle-truncate-line "wrap lines")
  ("_" helm-toggle-full-frame "full frame")
  ("Y" helm-yank-selection "yank entry")
  ("U" helm-refresh "update data")
  ("q" nil "quit" :exit t))

(defun gnus-recent-org-helm-hydra-pa (recent)
  "Persistent action activates a Hydra.
RECENT is the current article in the helm buffer."
  (setq gnus-recent-helm-current-data-pa recent)
  (hydra-gnus-org-helm/body))



(provide 'gnus-recent-org)
;;; gnus-recent-org.el ends here
