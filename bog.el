;;; bog.el --- Extensions for research notes in Org mode

;; Copyright (C) 2013 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/bog
;; Keywords: BibTeX, org-mode
;; Version: 0.5.0
;; Package-Requires: ((org "8.0.0") (dash "2.5.0"))

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Bog provides a few convenience functions for taking research notes in
;; Org mode.

;;; Code:

(require 'dash)
(require 'org)


;;; Customization

(defgroup bog nil
  "Extensions for research notes in Org mode"
  :group 'tools
  :group 'convenience)

(defcustom bog-notes-directory "~/bib"
  "The name of the directory that Org note are stored in."
  :group 'bog
  :type 'string)

(defcustom bog-pdf-directory
  (expand-file-name "pdfs" bog-notes-directory)
  "The name of the directory that PDF files are stored in."
  :group 'bog
  :type 'string)

(defcustom bog-read-file-name 'read-file-name
  "A function that will be used to promtp for file name.
The function should accept one arguments, a string to use for the
prompt. A good alternative is `ido-read-file-name'."
  :group 'bog
  :type 'function)

(defcustom bog-completing-read 'completing-read
  "A function that will be used for completion prompts.
The function should accept two arguments, a string to use for the
prompt and a list of strings to offer as choices. A good
alternative is `ido-completing-read'."
  :group 'bog
  :type 'function)

(defcustom bog-pdf-opener "xdg-open"
  "Program to open PDF files with."
  :group 'bog
  :type 'string)

(defcustom bog-citekey-format
  "\\([0-9]*[a-z]+[-a-z]*\\)\\([0-9]\\{4\\}\\)\\([a-z]+\\)"
  "Regex used to match study citekey.
By default, this matches any sequence of lower case
letters (allowing hyphenation) that is followed by 4 digits and
then lower case letters."
  :type 'string
  :group 'bog)


;;; General utilities

(defun bog-citekey-action (action ask-func ask-citekey)
  "Perform ACTION on file associtated with a citekey.

ASK-FUNC should be a function that queries the user for a citekey
when ASK-CITEKEY is non-nil. Otherwise, the citekey will be taken
from the text under point if it matches `bog-citekey-format' or
from the first parent heading that matches `bog-citekey-format'.

ACTION will be called with the resulting citekey as an argument."
  (let* ((citekey (and ask-citekey (funcall ask-func)))
         (citekey (or (bog-citekey-at-point)
                      (bog-citekey-heading))))
    (funcall action citekey)))

(defun bog-select-citekey (citekeys)
  "Prompt for citekey from CITEKEYS"
  (funcall bog-completing-read "Select citekey: " citekeys))

(defun bog-citekey-at-point ()
  (let ((maybe-citekey (thing-at-point 'word)))
    (when (and maybe-citekey
               (bog-citekey-only-p maybe-citekey))
      maybe-citekey)))

(defun bog-citekey-heading ()
  "Return first org heading that matches `bog-citekey-format'."
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading)
      (let ((heading (org-element-property :raw-value (org-element-at-point))))
        (while (and (not (bog-citekey-only-p heading))
                    (org-up-heading-safe))
          (setq heading
                (org-element-property :raw-value (org-element-at-point))))
        (when (not (bog-citekey-only-p heading))
          (error "Citekey not found"))
        heading))))

(defun bog-citekey-p (text)
  "Indicate if TEXT matches `bog-citekey-format'."
  (when (string-match bog-citekey-format text)
    t))

(defun bog-citekey-only-p (text)
  "Indicate if all of TEXT matches `bog-citekey-format'."
  (string-match bog-citekey-format text)
  (when (equal (length text) (match-end 0))
    t))


;;; PDF-related

;;;###autoload
(defun bog-find-citekey-pdf (arg)
  "Open PDF file for a citekey.
If a prefix argument is given, a prompt will open to select from
available citekeys. Otherwise, the citekey will be taken from the
text under point if it matches `bog-citekey-format' or from the
first parent heading that matches `bog-citekey-format'."
  (interactive "P")
  (bog-citekey-action 'bog-open-citekey-pdf
                      '(lambda () (bog-select-citekey (bog-pdf-citekeys)))
                      arg))

(defun bog-open-citekey-pdf (citekey)
  (let ((pdf-file (bog-citekey-as-pdf citekey)))
    (unless (file-exists-p pdf-file)
      (error "%s does not exist" pdf-file))
    (start-process "bog-pdf" nil bog-pdf-opener pdf-file)))

(defun bog-citekey-as-pdf (citekey)
  (expand-file-name (concat citekey ".pdf") bog-pdf-directory))

(defun bog-pdf-citekeys ()
  "Return a list citekeys for all pdf files in
`bog-pdf-directory'."
  (-map 'file-name-base
        (file-expand-wildcards (concat
                                (file-name-as-directory bog-pdf-directory)
                                "*.pdf"))))

(provide 'bog)

;; bog.el ends here
