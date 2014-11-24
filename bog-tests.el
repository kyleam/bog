;;; bog-tests.el --- Tests for Bog

;; Copyright (C) 2013-2014 Kyle Meyer <kyle@kyleam.com>

;; Author: Kyle Meyer <kyle@kyleam.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'org)
(require 'dash)
(require 'cl)
(require 'bog)

;; Modified from magit-tests.el.
(defmacro bog-tests--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (gensym)))
    `(let ((,dir (file-name-as-directory (make-temp-file "dir" t))))
       (unwind-protect
           (let ((default-directory ,dir)) ,@body)
         (delete-directory ,dir t)))))

;; Modified from org-tests.el.
(defmacro bog-tests--with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org-mode buffer.
Insert TEXT in buffer.

If string \"<citekey>\" appears in TEXT, replace it with the
value of the variable `citekey'.

If the string \"<point>\" appears in TEXT then remove it and
place the point there before running BODY, otherwise place the
point at the beginning of the inserted text."
  (declare (indent 1))
  `(let* ((inside-text (if (stringp ,text) ,text (eval ,text)))
          (is-citekey (string-match "<citekey>" inside-text)))
     (when (and is-citekey citekey)
       (setq inside-text (replace-match citekey nil nil inside-text)))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
         (if point
             (progn
               (insert (replace-match "" nil nil inside-text))
               (goto-char (1+ (match-beginning 0))))
           (insert inside-text)
           (goto-char (point-min))))
       ,@body)))


;;; Citekey functions

;; `bog-citekey-p'

(ert-deftest bog-citekey-p ()
  (should (bog-citekey-p "name2010word"))
  (should (bog-citekey-p "name1900word"))
  (should-not (bog-citekey-p "name201word")))

(ert-deftest bog-citekey-p-with-hyphen-in-name ()
  (should (bog-citekey-p "hyphen-ok2010word")))

(ert-deftest bog-citekey-p-with-other-text ()
  (should-not (bog-citekey-p "name2010word more text")))

;; `bog-citekey-groups-with-delim'

(ert-deftest bog-citekey-groups-with-delim ()
  (let ((citekey "name2010word"))
    (should (equal (bog-citekey-groups-with-delim citekey)
                   "name 2010 word"))
    (should (equal (bog-citekey-groups-with-delim citekey ",")
                   "name,2010,word"))
    (should (equal (bog-citekey-groups-with-delim citekey nil '(1 3))
                   "name word"))))

;; `bog-citekey-at-point'

(ert-deftest bog-citekey-at-point-bob ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert citekey)
      (goto-char (point-min))
      (should (equal (bog-citekey-at-point) citekey)))))

(ert-deftest bog-citekey-at-point-newline ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert "\n" citekey)
      (should (equal (bog-citekey-at-point) citekey)))))

(ert-deftest bog-citekey-at-point-parens ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert "\n(" citekey ")")
      (backward-char 2)
      (should (equal (bog-citekey-at-point) citekey)))))

(ert-deftest bog-citekey-at-point-spaces ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert "\n " citekey " ")
      (backward-char 2)
      (should (equal (bog-citekey-at-point) citekey)))))

(ert-deftest bog-citekey-at-point-with-hyphen ()
  (let ((citekey "hyphen-name2010word"))
    (with-temp-buffer
      (insert citekey)
      (goto-char (point-min))
      (should (equal (bog-citekey-at-point) citekey)))))

;; `bog-citekey-from-tree'

(ert-deftest bog-citekey-from-heading-title-current-level ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** <citekey>
some text
<point>"
      (should (equal (bog-citekey-from-tree) citekey)))))

(ert-deftest bog-citekey-from-heading-title-in-parent ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** <citekey>
*** subheading
some text
<point>"
      (should (equal (bog-citekey-from-tree) citekey)))))

(ert-deftest bog-citekey-from-heading-title-on-heading ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** <citekey><point>
some text"
      (should (equal (bog-citekey-from-tree) citekey)))))

(ert-deftest bog-citekey-from-property-current-level ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** subhead
   :PROPERTIES:
   :CUSTOM_ID: <citekey>
   :END:

some text<point>"
      (should (equal (bog-citekey-from-tree) citekey)))))

(ert-deftest bog-citekey-from-property-in-parent ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
  :PROPERTIES:
  :CUSTOM_ID: <citekey>
  :END:

some text

** subhead
<point>"
      (should (equal (bog-citekey-from-tree) citekey)))))

(ert-deftest bog-citekey-from-property-on-heading ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** <point>subhead
   :PROPERTIES:
   :CUSTOM_ID: <citekey>
   :END:
some text"
      (should (equal (bog-citekey-from-tree) citekey)))))

;; `bog-citekey-from-notes'

(ert-deftest bog-citekey-from-notes-on-heading ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** <point><citekey>
some text"
      (should (equal (bog-citekey-from-notes) citekey)))))

(ert-deftest bog-citekey-from-notes-before-text-citekey ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** other2000key
some text and <point><citekey>"
      (should (equal (bog-citekey-from-notes) citekey)))))

(ert-deftest bog-citekey-from-notes-after-text-citekey ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** other2000key
some text and <citekey><point>"
      (should (equal (bog-citekey-from-notes) citekey)))))

(ert-deftest bog-citekey-from-notes-on-text-citekey ()
  (let ((citekey "name2010word"))
    (bog-tests--with-temp-text
        "
* top level
** other2000key
some text and <point><citekey>"
      (forward-char)
      (should (equal (bog-citekey-from-notes) citekey)))))

(ert-deftest bog-citekey-from-notes-no-citekey ()
  (bog-tests--with-temp-text
      "
* top level
** second"
    (should-not (bog-citekey-from-notes))))


;;; File functions

(ert-deftest bog-file-citekey ()
  (should (equal (bog-file-citekey "name2000word.pdf") "name2000word"))
  (should (equal (bog-file-citekey "name2000word-supp.pdf") "name2000word"))
  (should (equal (bog-file-citekey "name2000word_0.pdf") "name2000word"))
  (should-not (bog-file-citekey "name2000.pdf"))
  (should-not (bog-file-citekey "leader_name2000word.pdf")))

(ert-deftest bog-all-file-citekeys ()
  (bog-tests--with-temp-dir
   (let ((bog-file-directory (expand-file-name "citekey-files")))
     (make-directory bog-file-directory)
     (let ((default-directory bog-file-directory))
       (make-directory "key2000butdir"))
     (write-region "" nil (expand-file-name "nokey.pdf" bog-file-directory))
     (write-region "" nil (expand-file-name "one2010key.pdf" bog-file-directory))
     (write-region "" nil (expand-file-name "two1980key.txt" bog-file-directory))
     (should (equal (bog-all-file-citekeys)
                    '("one2010key" "two1980key"))))))

(ert-deftest bog-rename-staged-file-to-citekey-one-file ()
  (bog-tests--with-temp-dir
   (let ((bog-stage-directory (expand-file-name "stage"))
         (bog-file-directory (expand-file-name "citekey-files"))
         (citekey "name2010word"))
     (make-directory bog-stage-directory)
     (make-directory bog-file-directory)
     (write-region "" nil (expand-file-name "one.pdf" bog-stage-directory))
     (bog-tests--with-temp-text
         "
* top level
** <point><citekey>
some text"
       (bog-rename-staged-file-to-citekey))
     (should (file-exists-p (expand-file-name
                             (concat citekey ".pdf") bog-file-directory)))
     (should-not (file-exists-p (expand-file-name
                                 (concat "one.pdf") bog-stage-directory))))))

(ert-deftest bog-file-citekeys-multiple-variants ()
  (bog-tests--with-temp-dir
   (let* ((bog-file-directory (expand-file-name "citekey-files"))
          (citekey "name2010word")
          (variants (list (concat citekey ".pdf")
                          (concat citekey ".txt")
                          (concat citekey "_0.pdf")
                          (concat citekey "-supplement.pdf")))
          found-files)
     (make-directory bog-file-directory)
     (--each variants
       (write-region "" nil (expand-file-name it bog-file-directory)))
     (setq files-found (bog-citekey-files citekey))
     (should (= (length files-found) 4)))))


;;; BibTeX functions

;; `bog--prepare-bib-file'

(ert-deftest bog--prepare-bib-file ()
  (bog-tests--with-temp-dir
    (let ((temp-file (make-temp-file
                      (expand-file-name "bog-testing-" default-directory)
                      nil ".bib"))
          (citekey "name2010word"))
      (with-current-buffer (find-file-noselect temp-file)
        (insert (format "\n@article{%s,\n" citekey)
                "title = {A title},\n"
                "author = {Last, First},\n"
                "journal = {Some journal},\n"
                "year = 2009,\n"
                "\n}")
        (save-buffer))
      (kill-buffer (get-file-buffer temp-file))
      (bog--prepare-bib-file temp-file)
      (should-not (file-exists-p temp-file))
      (let* ((new-file (concat citekey ".bib"))
             (new-buffer (get-file-buffer new-file)))
        (should-not new-buffer)
        (delete-file new-file)))))

(ert-deftest bog--prepare-bib-file-was-open ()
  (bog-tests--with-temp-dir
    (let ((temp-file (make-temp-file
                      (expand-file-name "bog-testing-" default-directory)
                      nil ".bib"))
          (citekey "name2010word"))
      (with-current-buffer (find-file-noselect temp-file)
        (insert (format "\n@article{%s,\n" citekey)
                "title = {A title},\n"
                "author = {Last, First},\n"
                "journal = {Some journal},\n"
                "year = 2009,\n"
                "\n}")
        (save-buffer))
      (bog--prepare-bib-file temp-file)
      (should-not (file-exists-p temp-file))
      (let* ((new-file (concat citekey ".bib"))
             (new-buffer (get-file-buffer new-file)))
        (should new-buffer)
        (kill-buffer new-buffer)
        (delete-file new-file)))))

;; `bog-collect-references'

(ert-deftest bog-collect-unique-references ()
  (with-temp-buffer
    (insert  "abc1900word\nhij2000word\nefg1800word\n")
    (should (equal (bog-collect-unique-references)
                   '("abc1900word" "efg1800word" "hij2000word")))))

;; `bog-sort-topic-headings-in-buffer'

(ert-deftest bog-sort-topic-headings-in-buffer ()
  (bog-tests--with-temp-text
      "
* topic heading
** zoo2000key
** apple2000key

* another topic heading
** orange2000key
** banana2000key
** yogurt2000key"
    (let ((bog-topic-heading-level 1))
      (bog-sort-topic-headings-in-buffer)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "apple2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "banana2000key")))))

(ert-deftest bog-sort-topic-headings-in-buffer-ignore-citekey-heading ()
  (bog-tests--with-temp-text
      "
* topic heading
** zoo2000key
** apple2000key
* citekey2000heading
** orange2000key
** banana2000key
** yogurt2000key"
    (let ((bog-topic-heading-level 1))
      (bog-sort-topic-headings-in-buffer)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "apple2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "orange2000key")))))

(ert-deftest bog-sort-topic-headings-in-buffer-ignore-citekey-property ()
  (bog-tests--with-temp-text
      (format  "
* topic heading
** zoo2000key
** apple2000key
* non-topic heading
  :PROPERTIES:
  :%s: citekey2000prop
  :END:
** orange2000key
** banana2000key
** yogurt2000key"
               bog-citekey-property)
    (let ((bog-topic-heading-level 1))
      (bog-sort-topic-headings-in-buffer)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "apple2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "orange2000key")))))

(ert-deftest bog-sort-topic-headings-in-buffer-passed-sorting-type ()
  (bog-tests--with-temp-text
      "
* topic heading
** zoo2000key
** apple2000key

* another topic heading
** orange2000key
** banana2000key
** yogurt2000key"
    (let ((bog-topic-heading-level 1))
      (bog-sort-topic-headings-in-buffer ?n)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "zoo2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "orange2000key")))))
