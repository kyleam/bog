(defvar org-location "~/src/emacs/org-mode/lisp")

(when (and org-location (file-exists-p org-location))
  (add-to-list 'load-path org-location)
  (require 'org))

(let ((readme-file "README.org")
      exported-file
      (final-file "README"))
  (with-current-buffer (find-file-noselect readme-file)
    (setq exported-file (org-ascii-export-to-ascii)))
  (rename-file exported-file final-file t))
