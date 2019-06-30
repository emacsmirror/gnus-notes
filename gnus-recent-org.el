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

(defvar gnus-recent-org--current-org-id nil
  "Internal variable; for temporarily placing the current heading org-id.")

(defvar gnus-recent-org--current-heading-alist nil
  "Internal variable; for temporarily placing the current heading info.")

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
    (message-box "Mail Top\nNumber of links: %d\nTop link: %s\nSender: %s\n"
                 (length entry-links)
                 (car-safe entry-links)
                 (if (> (length entry-links) 0)
                     "Somebody"         ; (alist-get 'sender (car entry-links))
                   "Nobody"))))

(defun gnus-recent-org-handle-mail-view ()
  "Show available gnus messages on the current org headline.
The messages will be shown in a Gnus ephemeral group."
  (interactive)
  (let* ((entry-text (gnus-string-remove-all-properties (org-get-entry)))
         (org-links-gnus (gnus-recent-org-search-string-org-links-gnus entry-text)))
    (message-box "Mail-View\nNumber of links: %d\nTop link: %s\nSender: %s\n"
                 (length org-links-gnus)
                 (car-safe org-links-gnus)
                 (if (> (length org-links-gnus) 0)
                     "Somebody"         ; (alist-get 'sender org-links-gnus)
                   "Nobody"))))

(defun gnus-recent-org-get-heading-alist ()
  "Get the text of a org heading and extract the info needed."
  (interactive)
  (save-excursion
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-goto))
    (org-back-to-heading t)
    (let* ((org-hd-marker (point-marker))
           (uid (org-id-get-create))
           (hd-txt (save-excursion
                     (buffer-substring-no-properties (point-at-bol 2)
                                                     (org-end-of-subtree t))))
           (org-links-gnus (gnus-recent-org-search-string-org-links-gnus hd-txt))
           (articles-msgid (mapcar 'cdr
                                   (mapcar 'gnus-recent-split-org-link-gnus
                                           org-links-gnus)))
           (articles (gnus-recent-org-filter-message-ids-list articles-msgid)))
      (list
       (cons 'uid uid)
       (cons 'org-hd-marker org-hd-marker)
       (cons 'entry-text hd-txt)
       (cons 'org-links-gnus org-links-gnus)
       (cons 'orgids (gnus-recent-org-get-orgids hd-txt))
       (cons 'articles-msgid articles-msgid)
       (cons 'articles-crumbs articles)))))

(defun gnus-recent-org-set-heading-alist ()
  "Save the heading info needed to `gnus-recent-org--current-heading-alist'."
  (interactive)
  (setq gnus-recent-org--current-heading-alist (gnus-recent-org-get-heading-alist)))

(defun gnus-recent-org-clear-heading-alist ()
  "Clear all data from variable `gnus-recent-org--current-heading-alist'."
  (interactive)
  (setq gnus-recent-org--current-heading-alist nil))

(defun gnus-recent-org-message-add-hooks ()
  "Add the hooks for an outgoing message."
  (add-hook 'message-sent-hook 'gnus-recent-org-message-sent-actions t)
  (add-hook 'message-cancel-hook 'gnus-recent-org-message-remove-hooks))

(defun gnus-recent-org-message-remove-hooks ()
  "Remove the hooks set by `gnus-recent-org-message-add-hooks'."
  (remove-hook 'message-sent-hook 'gnus-recent-org-message-sent-actions)
  (remove-hook 'message-cancel-hook 'gnus-recent-org-message-remove-hooks))

;; FIXME: Exploratory code, need to handle cancelling and aborting.
;; FIXME: Message-send (C-c C-s) results in empty group field.
(defun gnus-recent-org-message-sent-actions ()
  "Tidy up after an outgoing message is sent.
Add a gnus-link to the org entry as a log-note, then tidy up."
  (when gnus-recent-org--current-heading-alist
    (let* ((recent-out (car gnus-recent--articles-list))
           (root-marker (alist-get 'org-hd-marker
                                   gnus-recent-org--current-heading-alist))
           (org-link (gnus-recent--create-org-link recent-out)))
      ;; confirm the last item was an outgoing message
      (when (gnus-recent-outgoing-message-p recent-out)
        ;; FIXME: after exiting the log note, a user input for "comment syntax"
        ;;        appears. This leads to an error, which doesn't seem to affect
        ;;        the outgoing message nor the note taking.
        (org-with-point-at root-marker
          (org-add-log-setup 'note nil nil nil org-link))
        (gnus-recent-org-clear-heading-alist)
        (gnus-recent-org-message-remove-hooks)))))

(defun gnus-recent-org-handle-mail-crumbs ()
  "Show available gnus messages from the current org headline in helm."
  (interactive)
  (helm
   :sources (helm-build-sync-source "Heading articles"
              :keymap gnus-recent-helm-map
              :candidates (lambda ()
                            (gnus-recent-helm-candidates
                             (alist-get 'articles-crumbs
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
  "Handle mail in org heading.
First, this function sets the variable
`gnus-recent--current-heading-alist' with the the current heading
info. Second, it activates a hook to run after sending a message,
that will take care of the org stuff. Then it call a hydra to
select the action on the email articles."
  (interactive)
  (gnus-recent-org-set-heading-alist)
  (gnus-recent-org-message-add-hooks)
  (hydra-gnus-recent-org-handle-mail/body))

(defhydra hydra-gnus-recent-org-handle-mail (:color blue)
  "Reply to email from current task"
  ("h" gnus-recent-org-handle-mail-crumbs "View in helm")
  ("t" gnus-recent-org-handle-mail-top "Reply to top")
  ("v" gnus-recent-org-handle-mail-view "View emails")
  ("q" gnus-recent-org-clear-heading-alist "quit"))

(defun gnus-recent-org-incoming-mail ()
  "Associate an email with an existing org heading.
While viewing emails in gnus, in a summary or artile buffer,
associate the message under point with an existing org header. To
associate with a new org header use the org capture mechanism
for emails."
  (interactive)
  (when (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
    (user-error "Not in a gnus summary or article buffer"))
  (let ((windc (and (eq major-mode 'gnus-article-mode)
                    (current-window-configuration))))
    (when windc
      (gnus-article-show-summary))
    (gnus-recent--track-article)
    (when (window-configuration-p windc)
      (set-window-configuration windc))
    (org-capture nil "e")))             ; FIXME: hardcoded template key

(defun gnus-recent-org-outgoing-mail ()
  "Associate a message being written with an existing org heading."
  (interactive)
  ;; 1. get message heading id
  ;; 2. save to temp variable
  ;; 3. setup appropriate hooks
  ;;    - message-sent-hook
  ;; 4. use org capture to take a note and add it to heading
  )

(defun gnus-recent-org-get-entry (&optional keep-properties)
  "Get the org entry text.
With the optional KEEP-PROPERTIES non-nil keep the text
properties. By default, text properties are removed."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (if keep-properties
      (org-get-entry)
    (gnus-string-remove-all-properties (org-get-entry))))

(defun gnus-recent-org-search-string-org-links-gnus (txt)
  "Search text TXT for org-links, having protocol \"gnus:\".
Returns a list of org-links, that point to gnus articles."
  (mapcar 'car
          (s-match-strings-all "\\[\\[gnus:.+\\]\\]"
                               (gnus-string-remove-all-properties txt))))

(defun gnus-recent-org-get-orgids (txt)
  "Find the org-ids in org entry text TXT."
  (mapcar (lambda (x) (cadr (split-string x ":" t " +")))
          (mapcar 'car
                  (s-match-strings-all "^ +:ID:.+" txt))))

(defun gnus-recent-org-filter-message-ids-list (id-list)
  "Get the article message-id that have any of the given org-ids.
ID-LIST is a list of org-ids to search in `gnus-recent--articles-list'. Returns a
combined list of all article message-ids found."
  ;; FIXME: use equal for the test
  (mapcan '(lambda (id) (gnus-recent-filter-prop 'message-id id #'string=))
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
                  (alist-get 'uid gnus-recent-org--current-heading-alist))))

(defhydra hydra-gnus-org-helm (:columns 4 :exit nil)
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

;; keybindings
;; (org-defkey org-mode-map (kbd "C-c t") #'hydra-gnus-recent-org-handle-mail-top)
;; (org-defkey org-mode-map (kbd "C-c v") #'gnus-recent-org-view)
;; (eval-after-load "org-agenda"
;;   '(progn (org-defkey org-agenda-mode-map (kbd "C-c t") #'gnus-recent-org-handle-mail)
;;           (org-defkey org-agenda-mode-map (kbd "C-c v") #'gnus-recent-org-view)))
(define-key org-mode-map (kbd "C-c t") 'gnus-recent-org-handle-mail)
(org-defkey org-agenda-keymap (kbd "C-c t") 'gnus-recent-org-handle-mail)
(define-key gnus-summary-mode-map (kbd "C-c t") 'gnus-recent-org-incoming-mail)
(define-key gnus-article-mode-map (kbd "C-c t") 'gnus-recent-org-incoming-mail)
(define-key message-mode-map (kbd "C-c t") 'gnus-recent-org-outgoing-mail)

(provide 'gnus-recent-org)
;;; gnus-recent-org.el ends here
