(require 'ert)
(require 'org)
(require 'dash)
(require 'bog)

;; Modified from magit-tests.el.
(defmacro bog-tests--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (gensym)))
    `(let ((,dir (file-name-as-directory (make-temp-file "dir" t))))
       (unwind-protect
           (let ((default-directory ,dir)) ,@body)
         (delete-directory ,dir t)))))


;;; Citekey functions

;; `bog-citekey-p'

(ert-deftest bog-citekey-p ()
  (should (bog-citekey-p "name2010word"))
  (should (bog-citekey-p "name1900word"))
  (should-not (bog-citekey-p "name201word")))

(ert-deftest bog-citekey-p-with-hyphen-in-name ()
  (should (bog-citekey-p "hyphen-ok2010word")))

(ert-deftest bog-citekey-p-with-other-text ()
  (should (bog-citekey-p "name2010word more text")))

(ert-deftest bog-citekey-only-p ()
  (should (bog-citekey-only-p "name2010word"))
  (should-not (bog-citekey-only-p "name201word"))
  (should (bog-citekey-only-p "hyphen-ok2010word"))
  (should-not (bog-citekey-only-p "name2010word more text")))

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

(ert-deftest bog-citekey-at-point ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert citekey)
      (goto-char (point-min))
      (should (equal (bog-citekey-at-point) citekey)))))

;; `bog-citekey-from-heading-title'

(ert-deftest bog-citekey-from-heading-title-current-level ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** %s\n\nsome text\n"
                      citekey))
      (org-mode)
      (show-all)
      (should (equal (bog-citekey-from-heading-title) citekey)))))

(ert-deftest bog-citekey-from-heading-title-in-parent ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** %s\n\n*** subheading\n\nsome text\n"
                      citekey))
      (org-mode)
      (show-all)
      (should (equal (bog-citekey-from-heading-title) citekey)))))

(ert-deftest bog-citekey-from-heading-title-on-heading ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** %s\n\nsome text\n"
                      citekey))
      (org-mode)
      (show-all)
      (re-search-backward bog-citekey-format)
      (should (equal (bog-citekey-from-heading-title) citekey)))))

;; `bog-citekey-from-property'

(ert-deftest bog-citekey-from-property-current-level ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert "\n* top level\n\n** subhead\n"
              (format  ":PROPERTIES:\n:CUSTOM_ID: %s\n" citekey)
              ":END:\nsome text\n")
      (org-mode)
      (show-all)
      (should (equal (bog-citekey-from-property) citekey)))))

(ert-deftest bog-citekey-from-property-in-parent ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert "\n* top level\n"
              (format  ":PROPERTIES:\n:CUSTOM_ID: %s\n" citekey)
              ":END:\nsome text\n"
              "** subhead\n\n")
      (org-mode)
      (show-all)
      (should (equal (bog-citekey-from-property) citekey)))))

(ert-deftest bog-citekey-from-property-on-heading ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert "\n* top level\n\n** subhead\n"
              (format  ":PROPERTIES:\n:CUSTOM_ID: %s\n" citekey)
              ":END:\nsome text\n")
      (org-mode)
      (show-all)
      (org-back-to-heading)
      (should (equal (bog-citekey-from-property) citekey)))))

;; `bog-citekey-from-notes'

(ert-deftest bog-citekey-from-notes-on-heading ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** %s\n\nsome text\n"
                      citekey))
      (org-mode)
      (show-all)
      (re-search-backward bog-citekey-format)
      (should (equal (bog-citekey-from-notes) citekey)))))

(ert-deftest bog-citekey-from-notes-on-in-text-citekey ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** other2000key\n\nsome text and %s\n"
                      citekey))
      (org-mode)
      (show-all)
      (re-search-backward bog-citekey-format)
      (should (equal (bog-citekey-from-notes) citekey)))))

(ert-deftest bog-citekey-from-notes-no-citekey ()
  (with-temp-buffer
    (insert  "\n* top level\n\n** second\n\n")
    (org-mode)
    (show-all)
    (should-error (bog-citekey-from-notes))))


;;; PDF functions

(ert-deftest bog-rename-staged-pdf-to-citekey-one-pdf ()
  (bog-tests--with-temp-dir
   (let ((bog-stage-directory (expand-file-name "stage"))
         (bog-pdf-directory (expand-file-name "pdfs"))
         (citekey "name2010word"))
     (make-directory bog-stage-directory)
     (make-directory bog-pdf-directory)
     (write-region "" nil (expand-file-name "one.pdf" bog-stage-directory))
     (with-temp-buffer
       (insert (format "\n* top level\n\n** %s\n\nsome text\n"
                       citekey))
       (org-mode)
       (show-all)
       (re-search-backward bog-citekey-format)
       (bog-rename-staged-pdf-to-citekey))
     (should (file-exists-p (expand-file-name
                             (concat citekey ".pdf") bog-pdf-directory)))
     (should-not (file-exists-p (expand-file-name
                                 (concat "one.pdf") bog-pdf-directory))))))

(ert-deftest bog-pdf-citekeys-multiple-variants ()
  (bog-tests--with-temp-dir
   (let* ((bog-pdf-directory (expand-file-name "pdfs"))
          (citekey "name2010word")
          (variants (--map (concat citekey it ".pdf")
                           '("" "_0" "-supplement")))
          found-files)
     (make-directory bog-pdf-directory)
     (--each variants
       (write-region "" nil (expand-file-name it bog-pdf-directory)))
     (setq files-found (bog-citekey-pdfs citekey))
     (should (= (length files-found) 3)))))


;;; BibTeX functions

;; `bog-prepare-bib-file'

(ert-deftest bog-prepare-bib-file ()
  (let ((temp-file (make-temp-file "bog-testing-" nil ".bib"))
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
    (bog-prepare-bib-file temp-file)
    (should-not (file-exists-p temp-file))
    (let* ((new-file (expand-file-name (concat citekey ".bib") "/tmp"))
           (new-buffer (get-file-buffer new-file)))
      (should-not new-buffer)
      (delete-file new-file))))

(ert-deftest bog-prepare-bib-file-was-open ()
  (let ((temp-file (make-temp-file "bog-testing-" nil ".bib"))
        (citekey "name2010word"))
    (with-current-buffer (find-file-noselect temp-file)
      (insert (format "\n@article{%s,\n" citekey)
              "title = {A title},\n"
              "author = {Last, First},\n"
              "journal = {Some journal},\n"
              "year = 2009,\n"
              "\n}")
        (save-buffer))
    (bog-prepare-bib-file temp-file)
    (should-not (file-exists-p temp-file))
    (let* ((new-file (expand-file-name (concat citekey ".bib") "/tmp"))
           (new-buffer (get-file-buffer new-file)))
      (should new-buffer)
      (kill-buffer new-buffer)
      (delete-file new-file))))

;; `bog-collect-references'

(ert-deftest bog-collect-references ()
  (with-temp-buffer
    (insert  "abc1900word\nhij2000word\nefg1800word\n")
    (should (equal (bog-collect-references)
                   '("abc1900word" "efg1800word" "hij2000word")))))

(ert-deftest bog-collect-references-no-sort ()
  (with-temp-buffer
    (insert  "abc1900word\nhij2000word\nefg1800word\n")
    (should (equal (bog-collect-references t)
                   '("efg1800word" "hij2000word" "abc1900word")))))

;; `bog-sort-topic-headings-in-buffer'

(ert-deftest bog-sort-topic-headings-in-buffer ()
  (with-temp-buffer
    (let ((bog-topic-heading-level 1))
      (insert  "\n* topic heading\n\n"
               "** zoo2000key\n\nsome text\n\n"
               "** apple2000key\n\nsome text\n"
               "* another topic heading\n\n"
               "** orange2000key\n\nsome text\n\n"
               "** banana2000key\n\nsome text\n"
               "** yogurt2000key\n\nsome text\n")
      (org-mode)
      (show-all)
      (bog-sort-topic-headings-in-buffer)
      (goto-char 0)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "apple2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "banana2000key")))))

(ert-deftest bog-sort-topic-headings-in-buffer-ignore-citekey-heading ()
  (with-temp-buffer
    (let ((bog-topic-heading-level 1))
      (insert  "\n* topic heading\n\n"
               "** zoo2000key\n\nsome text\n\n"
               "** apple2000key\n\nsome text\n"
               "* citekey2000heading\n\n"
               "** orange2000key\n\nsome text\n\n"
               "** banana2000key\n\nsome text\n"
               "** yogurt2000key\n\nsome text\n")
      (org-mode)
      (show-all)
      (bog-sort-topic-headings-in-buffer)
      (goto-char 0)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "apple2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "orange2000key")))))

(ert-deftest bog-sort-topic-headings-in-buffer-ignore-citekey-property ()
  (with-temp-buffer
    (let ((bog-topic-heading-level 1))
      (insert  "\n* topic heading\n\n"
               "** zoo2000key\n\nsome text\n\n"
               "** apple2000key\n\nsome text\n"
               "* non-topic heading\n"
               " :PROPERTIES:\n"
               (format " :%s: citekey2000prop\n" bog-citekey-property)
               " :END:\n"
               "** orange2000key\n\nsome text\n\n"
               "** banana2000key\n\nsome text\n"
               "** yogurt2000key\n\nsome text\n")
      (org-mode)
      (show-all)
      (bog-sort-topic-headings-in-buffer)
      (goto-char 0)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "apple2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "orange2000key")))))

(ert-deftest bog-sort-topic-headings-in-buffer-passed-sorting-type ()
  (with-temp-buffer
    (let ((bog-topic-heading-level 1))
      (insert  "\n* topic heading\n\n"
               "** zoo2000key\n\nsome text\n\n"
               "** apple2000key\n\nsome text\n"
               "* another topic heading\n\n"
               "** orange2000key\n\nsome text\n\n"
               "** banana2000key\n\nsome text\n"
               "** yogurt2000key\n\nsome text\n")
      (org-mode)
      (show-all)
      (bog-sort-topic-headings-in-buffer ?n)
      (goto-char 0)
      (outline-next-visible-heading 2)
      (should (equal (org-no-properties (org-get-heading t t))
                     "zoo2000key"))
      (outline-next-visible-heading 3)
      (should (equal (org-no-properties (org-get-heading t t))
                     "orange2000key")))))
