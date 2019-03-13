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

(org-defkey org-mode-map (kbd "C-c t") #'gnus-recent-org-handle-mail)
(org-defkey org-mode-map (kbd "C-c v") #'gnus-recent-org-view)
(eval-after-load "org-agenda"
  '(progn (org-defkey org-agenda-mode-map (kbd "C-c t") #'gnus-recent-org-handle-mail)
          (org-defkey org-agenda-mode-map (kbd "C-c v") #'gnus-recent-org-view)))

(defun gnus-recent-org-handle-mail ()
  "Create a email message based on the current headline.
The body of the org heading element must have at least on gnus
mail link to reply to."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-goto))
  (let* ((entry-text (gnus-string-remove-all-properties (org-get-entry)))
         (entry-links (s-match-strings-all "\\[\\[gnus:.+\\]\\]" entry-text)))
    (message-box "Number of links: %d\nTop link: %s\n"
                 (length entry-links) (caar entry-links))))

;; (org-get-entry)
;; (setq x (gnus-string-remove-all-properties x))
;; (s-match-strings-all "\\[\\[gnus:.+\\]\\]" x)
