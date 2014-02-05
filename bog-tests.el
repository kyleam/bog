(require 'ert)
(require 'org)
(require 'bog)


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

(ert-deftest bog-citekey-action-in-normal-text ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** %s\n\nsome text\n"
                      citekey))
      (org-mode)
      (show-all)
      (flet ((funcall (action citekey) citekey))
        (should (equal (bog-citekey-action nil nil nil) citekey))))))

;; `bog-citekey-action'

(ert-deftest bog-citekey-action-on-heading ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** %s\n\nsome text\n"
                      citekey))
      (org-mode)
      (show-all)
      (re-search-backward bog-citekey-format)
      (flet ((funcall (action citekey) citekey))
        (should (equal (bog-citekey-action nil nil nil) citekey))))))

(ert-deftest bog-citekey-action-on-in-text-citekey ()
  (let ((citekey "name2010word"))
    (with-temp-buffer
      (insert (format "\n* top level\n\n** other2000key\n\nsome text and %s\n"
                      citekey))
      (org-mode)
      (show-all)
      (re-search-backward bog-citekey-format)
      (flet ((funcall (action citekey) citekey))
        (should (equal (bog-citekey-action nil nil nil) citekey))))))

(ert-deftest bog-citekey-action-no-citekey ()
  (with-temp-buffer
    (insert  "\n* top level\n\n** second\n\n")
    (org-mode)
    (show-all)
    (should-error (bog-citekey-action nil nil nil))))


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
