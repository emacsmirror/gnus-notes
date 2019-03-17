;;; gnus-recent-org.el --- Gnus Recent O -*- lexical-binding: t -*-

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
;;; an interface with minimun configuration. It want to "just work".
;;; This file provides integration with your Org-mode TODO headings. It is inspired
;;; by Gnorb.

;;; Code:

(require 'gnus-recent)
(require 'org-gnus)

;; (org-defkey org-mode-map (kbd "C-c t") #'hydra-gnus-recent-org-handle-mail-top)
;; (org-defkey org-mode-map (kbd "C-c v") #'gnus-recent-org-view)
;; (eval-after-load "org-agenda"
;;   '(progn (org-defkey org-agenda-mode-map (kbd "C-c t") #'gnus-recent-org-handle-mail)
;;           (org-defkey org-agenda-mode-map (kbd "C-c v") #'gnus-recent-org-view)))

(defun gnus-recent-org-handle-mail-top ()
  "Reply to the top email message on the current org headline.
The body of the org heading must have at least one gnus link to
reply to."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (let* ((entry-text (gnus-string-remove-all-properties (org-get-entry)))
         (entry-links (gnus-recent-org-find-org-links-gnus entry-text)))
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
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (let* ((entry-text (gnus-string-remove-all-properties (org-get-entry)))
         (entry-links (gnus-recent-org-find-org-links-gnus entry-text)))
    (message-box "Number of links: %d\nTop link: %s\nSender: %s\n"
                 (length entry-links)
                 (car-safe entry-links)
                 (if (> (length entry-links) 0)
                     "Somebody"         ; (alist-get 'sender (car entry-links))
                   "Nobody")))
  )

;; (org-get-entry)
;; (setq x (gnus-string-remove-all-properties x))
;; (s-match-strings-all "\\[\\[gnus:.+\\]\\]" x)
;; s-match-strings-all "\\[\\[gnus:.+\\]\\]"
"\\[\\[\\(\\(gnus\\)\\|\\(bbdb\\)\\):.+\\]\\]"


;; <HE1PR0702MB374007926A69A5BA9F469833B7700@HE1PR0702MB3740.eurprd07.prod.outlook.com> OR <1859946832.1551532226619.JavaMail.root@7e5afac02a08> OR <87wolg9jyd.fsf@aia00054aia.gr> OR <871s3ob518.fsf@aia00054aia.gr> OR <43fc0c0fce9292d8bed09ca27.b1ed948b21.20190302133324.73f362d72f.17307e8e@mail197.sea51.mcsv.net>


(defhydra hydra-gnus-recent-org-handle-mail (:color blue)
  "Create an email from the current org headline."
  ("t" gnus-recent-org-handle-mail-top "Reply to top")
  ("v" gnus-recent-org-handle-mail-view "View emails")
  ("q" nil "quit"))

(define-key org-mode-map (kbd "C-c t") 'hydra-gnus-recent-org-handle-mail/body)


(defun gnus-recent-org-find-org-links-gnus (txt)
  "Search text TXT for org-links, having protocol \"gnus:\".
Returns a list of org-links, that point to gnus articles."
  (let (res)
    (dolist (l (s-match-strings-all "\\[\\[gnus:.+\\]\\]" txt))
      (push (car l) res))
    (nreverse res)))
