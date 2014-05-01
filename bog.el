;;; bog.el --- Extensions for research notes in Org mode

;; Copyright (C) 2013 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/kyleam/bog
;; Keywords: BibTeX, org-mode
;; Version: 0.6.0
;; Package-Requires: ((dash "2.5.0"))

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
;; Org mode. Many of these commands center around a citekey, the unique
;; identifier for a study. Some of the main commands are listed below.
;;
;; - `bog-find-citekey-file'
;; - `bog-find-citekey-bib'
;; - `bog-search-citekey-on-web'
;; - `bog-rename-staged-file-to-citekey'
;; - `bog-goto-citekey-heading-in-buffer'
;; - `bog-goto-citekey-heading-in-notes'
;; - `bog-search-notes-for-citekey'
;; - `bog-clean-and-rename-staged-bibs'
;; - `bog-refile'
;; - `bog-search-notes'
;; - `bog-sort-topic-headings-in-buffer'
;; - `bog-sort-topic-headings-in-notes'
;;
;; See README for more information.

;;; Code:

(require 'dash)
(require 'org)


;;; Customization

(defgroup bog nil
  "Extensions for research notes in Org mode"
  :group 'org)

(defcustom bog-citekey-format
  "\\([0-9]*[a-z]+[-a-z]*\\)\\([0-9]\\{4\\}\\)\\([a-z][a-z0-9]*\\)"
  "Regex used to match study citekey.

By default, this matches any sequence of lower case
letters (allowing hyphenation) that is followed by 4 digits and
then lower case letters.

The default format corresponds to the following BibTeX autokey
settings:

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-titleword-length nil
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-titlewords 1
        bibtex-autokey-year-title-separator \"\")"
  :group 'bog
  :type 'string)

(defcustom bog-citekey-func 'bog-citekey-from-heading-title
  "Function used to get citekey from study notes.

By default, this is `bog-citekey-from-heading-title', which
selects the citekey from the first parent heading whose title
matches `bog-citekey-format'.

The other option is `bog-citekey-from-property', which selects
the citekey from the first parent that has the property
`bog-citekey-property'."
  :group 'bog
  :type 'function)

(defcustom bog-citekey-property "CUSTOM_ID"
  "Property name used to store citekey.
This is only used if `bog-citekey-func' is set to
`bog-citekey-from-property'. The default corresponds to the
default value of `org-bibtex-key-property'."
  :group 'bog
  :type 'string)

(defcustom bog-notes-directory "~/bib"
  "Directory with Org research notes."
  :group 'bog
  :type 'string)

(defcustom bog-file-directory
  (expand-file-name "citekey-files" bog-notes-directory)
  "Directory with citekey-associated files."
  :group 'bog
  :type 'string)

(defcustom bog-stage-directory
  (expand-file-name "stage" bog-notes-directory)
  "Directory to search for new files.
`bog-rename-staged-file-to-citekey' and
`bog-rename-staged-bib-to-citekey' will search here for files to
rename."
  :group 'bog
  :type 'string)

(defcustom bog-find-citekey-bib-func 'bog-find-citekey-bib-file
  "Function used to find BibTeX entry for citekey.

Default is `bog-find-citekey-bib-file' that locates single entry
BibTeX files in `bog-bib-directory'.

The other option is `bog-find-citekey-entry' that searches within
a single BibTeX file, `bog-bib-file', for the citekey entry."
  :group 'bog
  :type 'function)

(defcustom bog-bib-directory
  (expand-file-name "bibs" bog-notes-directory)
  "The name of the directory that BibTeX files are stored in.
This is only meaningful if `bog-find-citekey-bib-func' set to
`bog-find-citekey-bib-file'."
  :group 'bog
  :type 'string)

(defcustom bog-bib-file nil
  "BibTeX file name.
This is only meaningful if `bog-find-citekey-bib-func' set to
`bog-find-citekey-entry'."
  :group 'bog
  :type 'string)

(defcustom bog-citekey-file-name-separators '("-" "_")
  "Characters allowed to follow the citekey in file names.
When `bog-find-citekey-file' is run on <citekey>, it will find
files with the format <citekey>.* and <citekey><sep>*.<ext>,
where <sep> is one of the characters in
`bog-citekey-file-name-separators'.")

(defcustom bog-file-renaming-func 'bog-file-ask-on-conflict
  "Function used to rename staged files.
This function should accept a file name and a citekey as
arguments and return the name of the final file. Currently the
only built-in function is `bog-file-ask-on-conflict'.")

(defcustom bog-file-secondary-name "-supplement"
  "Modification to make to file name on renaming confict.
When a staged file is being renamed with
`bog-file-ask-on-conflict', the user will be prompted if
<citekey>.<ext> already exists.
<citekey>`bog-file-secondary-name'.<ext> will be the default
value for the prompt.")

(defcustom bog-web-search-url
  "http://scholar.google.com/scholar?q=%s"
  "URL to use for CITEKEY search.
It should contain the placeholder \"%s\" for the query."
  :group 'bog
  :type 'string)

(defcustom  bog-topic-heading-level 1
  "Consider headings at this level to be topic headings.
Topic headings for studies may be at any level, but
`bog-sort-topic-headings' uses this variable to determine what
level to operate on."
  :group 'bog
  :type 'integer)

(defcustom  bog-refile-maxlevel bog-topic-heading-level
  "Consider up to this level when refiling with `bog-refile'."
  :group 'bog
  :type 'integer)

(defcustom bog-keymap-prefix (kbd "C-c \"")
  "Bog keymap prefix."
  :group 'bog
  :type 'string)

(defcustom bog-agenda-custom-command-key "b"
  "Key to use for Bog notes search key in agenda dispatch.
If nil, a custom command will not be added to Org agenda
dispatch, but searching Bog notes through the agenda interface
will still be available through `bog-search-notes' and
`bog-search-notes-for-citekey'."
  :group 'bog
  :type '(choice
          (const :tag "Don't display in agenda dispatch" nil)
          (string :tag "Key for agenda dispatch")))


;;; General utilities

(defun bog-select-citekey (citekeys)
  "Prompt for citekey from CITEKEYS"
  (org-icompleting-read "Select citekey: " citekeys))

(defun bog-citekey-groups-with-delim (citekey &optional delim groups)
  "Return groups of `bog-citekey-format', seperated by DELIM.

If DELIM is nil, space is used.

If GROUPS is nil, groups 1, 2, and 3 are selected (which
corresponds to the last name of the first author, the publication
year, and the first meaningful word in the title)."
  (let ((groups (or groups '(1 2 3)))
        (delim (or delim " ")))
    (string-match bog-citekey-format citekey)
    (mapconcat #'(lambda (g) (match-string-no-properties g citekey))
               groups delim)))

(defun bog-citekey-at-point ()
  (let ((maybe-citekey (thing-at-point 'word)))
    (when (and maybe-citekey
               (bog-citekey-only-p maybe-citekey))
      (substring-no-properties maybe-citekey))))

(defun bog-citekey-from-notes ()
  "Get the citekey from the context of the Org file."
  (or (bog-citekey-at-point)
      (funcall bog-citekey-func)))

(defun bog-citekey-from-heading-title ()
  "Retrieve citekey from first parent heading that matches
`bog-citekey-format'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((heading (org-no-properties (org-get-heading t t))))
        (while (and (not (bog-citekey-only-p heading))
                    (org-up-heading-safe))
          (setq heading (org-no-properties (org-get-heading t t))))
        (when (not (bog-citekey-only-p heading))
          (error "Citekey not found"))
        heading))))

(defun bog-citekey-from-property ()
  "Retrieve citekey from first parent heading that has the
 property `bog-citekey-property'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((citekey (org-entry-get (point) bog-citekey-property)))
        (while (and (not citekey)
                    (org-up-heading-safe))
          (setq citekey (org-entry-get (point) bog-citekey-property)))
        (when (not citekey)
          (error "Citekey not found"))
        citekey))))

(defun bog-citekey-heading-p ()
  (let ((heading (org-no-properties (org-get-heading t t))))
    (or (bog-citekey-only-p heading)
        (org-entry-get (point) bog-citekey-property))))

(defun bog-citekey-p (text)
  "Indicate if TEXT matches `bog-citekey-format'."
  (when (string-match bog-citekey-format text)
    t))

(defun bog-citekey-only-p (text)
  "Indicate if all of TEXT matches `bog-citekey-format'."
  (string-match bog-citekey-format text)
  (when (equal (length text) (match-end 0))
    t))


;;; Citekey-associated files

;;;###autoload
(defun bog-find-citekey-file (arg)
  "Open citekey-associated file.
If a prefix argument is given, a prompt will open to select from
available citekeys. Otherwise, the citekey will be taken from the
text under point if it matches `bog-citekey-format' or using
`bog-citekey-func'."
  (interactive "P")
  (let ((citekey (or (and arg (bog-select-citekey (bog-all-file-citekeys)))
                     (bog-citekey-from-notes))))
    (bog-open-citekey-file citekey)))

(defun bog-open-citekey-file (citekey)
  (let* (citekey-file
         (citekey-files (bog-citekey-files citekey))
         (citekey-file-names (-map 'file-name-nondirectory citekey-files))
         (num-choices (length citekey-file-names)))
    (cond
     ((= 0 num-choices)
      (error "No file found for %s" citekey))
     ((= 1 num-choices)
      (setq citekey-file (car citekey-files)))
     (t
      (setq citekey-file
            (expand-file-name (org-icompleting-read "Select file: "
                                                    citekey-file-names)
                              bog-file-directory))))
    (org-open-file citekey-file)))

(defun bog-citekey-files (citekey)
  (let* ((patterns (--map (concat it "*") bog-citekey-file-name-separators))
         (patterns (cons ".*" patterns)))
    (--mapcat (file-expand-wildcards
               (concat (file-name-as-directory bog-file-directory)
                       citekey it))
              patterns)))

;;;###autoload
(defun bog-rename-staged-file-to-citekey ()
  "Rename citekey file in `bog-stage-directory' with `bog-file-renaming-func'.
The citekey will be taken from the text under point if it matches
`bog-citekey-format' or using `bog-citekey-func'."
  (interactive)
  (let ((citekey (bog-citekey-from-notes)))
    (bog-rename-staged-file citekey)))

(defun bog-rename-staged-file (citekey)
  (let* ((staged-files (bog-staged-files))
         (staged-file-names (-map 'file-name-nondirectory staged-files))
         (num-choices (length staged-file-names))
         staged-file)
    (message "choices: %s" num-choices)
    (cond
     ((= 0 num-choices)
      (setq staged-file (org-iread-file-name "Select file to rename: ")))
     ((= 1 num-choices)
      (setq staged-file (car staged-files)))
     (t
      (setq staged-file
            (expand-file-name
             (org-icompleting-read "Select file to rename: "
                                   staged-file-names)
             bog-stage-directory))))
    (message "Renamed %s to %s" staged-file
             (funcall bog-file-renaming-func staged-file citekey))))

(defun bog-file-ask-on-conflict (staged-file citekey)
  "Rename citekey file, prompting for a new name if it already exists.
STAGED-FILE will be renamed to <citekey>.<ext> within
`bog-file-directory'. If this file already exists, the user will
be prompted for another name. `bog-file-secondary-name' can be
used to control the default string used in the prompt."
  (let* ((ext (file-name-extension staged-file))
         (citekey-file (bog-citekey-as-file citekey ext)))
    (condition-case nil
        (rename-file staged-file citekey-file)
      (file-error
       (let ((new-file-name
              (file-name-nondirectory
               (bog-citekey-as-file (concat citekey bog-file-secondary-name)
                                    ext))))
         (setq new-file-name
               (read-string
                (format "File %s already exists. Name to use instead: "
                        citekey-file)
                new-file-name nil nil '(new-file-name)))
         (setq citekey-file (expand-file-name new-file-name bog-file-directory))
         (rename-file staged-file citekey-file))))
    citekey-file))

(defun bog-citekey-as-file (citekey ext)
  (expand-file-name (concat citekey "." ext) bog-file-directory))

(defun bog-all-file-citekeys ()
  "Return a list of citekeys for files in `bog-file-directory'."
  (-distinct (-keep 'bog-file-citekey (bog-all-citekey-files))))

(defun bog-file-citekey (file)
  (let ((fname (file-name-base file)))
    (when (string-match (concat "^" bog-citekey-format) fname)
      (match-string 0 fname))))

(defun bog-all-citekey-files ()
  (-remove 'file-directory-p
           (directory-files bog-file-directory
                            t directory-files-no-dot-files-regexp)))

(defun bog-staged-files ()
  (-remove 'file-directory-p
           (directory-files bog-stage-directory
                            t directory-files-no-dot-files-regexp)))


;;; BibTeX-related

;;;###autoload
(defun bog-find-citekey-bib (arg)
  "Open BibTeX file for a citekey.
If a prefix argument is given, a prompt will open to select from
available citekeys. Otherwise, the citekey will be taken from the
text under point if it matches `bog-citekey-format' or using
`bog-citekey-func'."
  (interactive "P")
  (let ((citekey (or (and arg (bog-select-citekey (bog-bib-citekeys)))
                     (bog-citekey-from-notes))))
    (funcall bog-find-citekey-bib-func citekey)))

(defun bog-find-citekey-bib-file (citekey)
  "Open BibTeX file of CITEKEY contained in `bog-bib-directory'."
  (let ((bib-file (bog-citekey-as-bib citekey)))
    (unless (file-exists-p bib-file)
      (error "%s does not exist" bib-file))
    (find-file-other-window bib-file)))

(defun bog-find-citekey-entry (citekey)
  "Search for CITEKEY in `bog-bib-file'."
  (find-file-other-window bog-bib-file)
  (bibtex-search-entry citekey))

;;;###autoload
(defun bog-clean-and-rename-staged-bibs ()
  "Clean and rename BibTeX files in `bog-stage-directory'.

New BibTeX files are searched for in `bog-stage-directory', and
`bog-prepare-bib-file' will be run one each file before it is
moved to `bog-bib-directory'/<citekey>.bib.

This function is only useful if you use the non-standard setup of
one entry per BibTeX file."
  (interactive)
  (let ((staged
         (file-expand-wildcards
          (concat (file-name-as-directory bog-stage-directory) "*.bib"))))
    (--each staged
      (bog-prepare-bib-file it t bog-bib-directory))))

(defun bog-prepare-bib-file (file &optional new-key new-directory)
  (save-excursion
    (let ((was-open (get-file-buffer file))
          (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (bibtex-skip-to-valid-entry)
        (bibtex-clean-entry new-key)
        (let* ((citekey (bibtex-key-in-head))
               (bib-file
                (expand-file-name (concat citekey ".bib") new-directory)))
          (when (get-buffer bib-file)
            (error "Buffer for %s already exists" bib-file))
          (rename-file file bib-file)
          (rename-buffer bib-file)
          (set-visited-file-name bib-file)
          (save-buffer)))
      (unless was-open
        (kill-buffer buffer)))))

;;;###autoload
(defun bog-create-combined-bib ()
  "Create buffer that has entries for all citekeys in buffer."
  (interactive)
  (let ((bib-buffer (get-buffer-create "*Bib*"))
        (refs (-map 'bog-citekey-as-bib (bog-collect-references))))
    (--each refs (unless (file-exists-p it) (error "%s does not exist" it)))
    (switch-to-buffer-other-window bib-buffer)
    (--each refs
      (insert "\n")
      (insert-file-contents it)
      (goto-char (point-max)))
    (bibtex-mode)
    (goto-char (point-min))))

(defun bog-collect-references (&optional no-sort)
  "Return names in buffer that match `bog-citekey-format'.
If NO-SORT, citekeys are returned in reverse order that they
occur in buffer instead of alphabetical order."
  (let (refs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward bog-citekey-format nil t)
        (add-to-list 'refs (match-string-no-properties 0)))
      (if no-sort
          refs
        (--sort (string-lessp it other) refs)))))

(defun bog-citekey-as-bib (citekey)
  (expand-file-name (concat citekey ".bib") bog-bib-directory))

(defun bog-bib-citekeys ()
  "Return a list citekeys for all BibTeX files in `bog-bib-directory'."
  (-map 'file-name-base
        (file-expand-wildcards (concat
                                (file-name-as-directory bog-bib-directory)
                                "*.bib"))))


;;; Web

;;;###autoload
(defun bog-search-citekey-on-web ()
  "Open browser and perform query based for a citekey.

The URL will be taken from `bog-web-search-url'.

The citekey is split by groups in `bog-citekey-format' and joined by
\"+\" to form the query string."
  (interactive)
  (let ((citekey (bog-citekey-from-notes)))
    (bog-open-citekey-on-web citekey)))

(defun bog-open-citekey-on-web (citekey)
  (let ((url (bog-citekey-as-search-url citekey)))
    (browse-url url)))

(defun bog-citekey-as-search-url (citekey)
  "Return URL to use for search."
  (let ((query (bog-citekey-groups-with-delim citekey "+")))
    (format bog-web-search-url query)))


;;; Notes-related

(defun bog-goto-citekey-heading-in-buffer ()
  "Find citekey heading in this buffer.
The citekey will be taken from the text under point if it matches
`bog-citekey-format'."
  (interactive)
  (let* ((citekey (or (bog-citekey-at-point)
                      (read-string "Enter citekey: ")))
         (pos (org-find-exact-headline-in-buffer citekey nil t)))
    (if pos
        (progn
          (org-mark-ring-push)
          (goto-char pos)
          (org-show-context))
      (message "Heading for %s not found in buffer" citekey))))

(defun bog-goto-citekey-heading-in-notes ()
  "Find citekey heading in notes.
All org files in `bog-notes-directory' will be searched. The
citekey will be taken from the text under point if it matches
`bog-citekey-format'."
  (interactive)
  (let* ((citekey (or (bog-citekey-at-point)
                      (read-string "Enter citekey: ")))
        (marker
         (org-find-exact-heading-in-directory citekey bog-notes-directory)))
    (if marker
        (progn
          (switch-to-buffer (marker-buffer marker))
          (goto-char (marker-position marker))
          (org-show-context))
      (message "Heading for %s not found in notes" citekey))))

(defun bog-refile ()
  "Refile heading within notes.
All headings from Org files in `bog-notes-directory' at or above
level `bog-refile-maxlevel' are considered."
  (interactive)
  (let ((org-refile-targets `((,(bog-notes-files)
                               :maxlevel . ,bog-refile-maxlevel))))
    (org-refile)))

(defun bog-notes-files ()
  (file-expand-wildcards
   (concat (file-name-as-directory bog-notes-directory)
           "*.org")))

(defun bog-search-notes (&optional todo-only)
  "Search notes using `org-search-view'.
With prefix argument TODO-ONLY, only TODO entries are searched."
  (interactive "P")
  (let ((lprops (nth 4 bog-agenda-custom-command)))
    (put 'org-agenda-redo-command 'org-lprops lprops)
    (org-let lprops '(org-search-view todo-only))))

(defun bog-search-notes-for-citekey (&optional todo-only)
  "Search notes for citekey using `org-search-view'.
With prefix argument TODO-ONLY, only TODO entries are searched."
  (interactive "P")
  (let ((citekey (bog-citekey-from-notes))
        (lprops (nth 4 bog-agenda-custom-command)))
    (put 'org-agenda-redo-command 'org-lprops lprops)
    (org-let lprops '(org-search-view todo-only citekey))))

(defun bog-sort-topic-headings-in-buffer (&optional sorting-type)
  "Sort topic headings in this buffer.
SORTING-TYPE is a character passed to `org-sort-entries'. If nil,
?a is used. The level to sort is determined by
`bog-topic-heading-level'."
  (interactive)
  (org-map-entries '(lambda () (bog-sort-if-topic-header sorting-type))))

(defun bog-sort-topic-headings-in-notes (&optional sorting-type)
  "Sort topic headings in notes.
Unlike `bog-sort-topic-headings-in-buffer', sort topic headings
in all Bog notes."
  (interactive)
  (org-map-entries '(lambda ()  (bog-sort-if-topic-header sorting-type))
                   nil (bog-notes-files)))

(defun bog-sort-if-topic-header (sorting-type)
  "Sort heading with `org-sort-entries' according to SORTING-TYPE.
Sorting is only done if the heading's level matches
`bog-topic-heading-level' and it isn't a citekey heading."
  (let ((sorting-type (or sorting-type ?a)))
    (when (and (= (org-current-level) bog-topic-heading-level)
               (not (bog-citekey-heading-p)))
      (org-sort-entries nil sorting-type))))


;;; Font-lock

(defface bog-citekey-face
  '((((class color) (background dark))
     (:bold t))
    (((class color) (background light))
     (:bold t)))
  "Face used to highlight text that matches `bog-citekey-format'.")

(defun bog-non-heading-citekey-p (limit)
  (and (re-search-forward bog-citekey-format limit t)
       (not (org-at-heading-p))))

(defun bog-add-fontlock ()
  (font-lock-add-keywords nil
                          '((bog-non-heading-citekey-p . 'bog-citekey-face)))
  (font-lock-fontify-buffer))

(defun bog-remove-fontlock ()
  (font-lock-remove-keywords nil
                             '((bog-non-heading-citekey-p . 'bog-citekey-face)))
  (font-lock-fontify-buffer))


;;; Minor mode

(defvar bog-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map "b" 'bog-find-citekey-bib)
      (define-key prefix-map "c" 'bog-search-notes-for-citekey)
      (define-key prefix-map "f" 'bog-find-citekey-file)
      (define-key prefix-map "h" 'bog-goto-citekey-heading-in-buffer)
      (define-key prefix-map "H" 'bog-goto-citekey-heading-in-notes)
      (define-key prefix-map "r" 'bog-rename-staged-file-to-citekey)
      (define-key prefix-map "s" 'bog-search-notes)
      (define-key prefix-map "w" 'bog-search-citekey-on-web)
      (define-key map bog-keymap-prefix prefix-map))
    map)
  "Keymap for Bog.")

(defvar bog-agenda-custom-command
  `(,(or bog-agenda-custom-command-key "b") "Search Bog notes" search ""
    ((org-agenda-files (bog-notes-files))
     (org-agenda-text-search-extra-files nil))))

;;;###autoload
(define-minor-mode bog-mode
  "Toggle Bog in this buffer.
With a prefix argument ARG, enable `bog-mode' if ARG is positive,
and disable it otherwise. If called from Lisp, enable the mode if
ARG is omitted or nil.

\\{bog-mode-map}"
  :keymap bog-mode-map
  :group 'bog
  :lighter " Bog"
  :require 'bog
  (cond
   (bog-mode
    (bog-add-fontlock)
    (when bog-agenda-custom-command-key
      (add-to-list 'org-agenda-custom-commands
                   bog-agenda-custom-command)))
   (t
    (bog-remove-fontlock)
    (when bog-agenda-custom-command
      (setq org-agenda-custom-commands (delete bog-agenda-custom-command
                                               org-agenda-custom-commands))))))

(provide 'bog)

;;; bog.el ends here
