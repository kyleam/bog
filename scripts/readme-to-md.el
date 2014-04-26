(defvar org-location "~/src/emacs/org-mode/lisp")

(when (and org-location (file-exists-p org-location))
  (add-to-list 'load-path org-location)
  (require 'org))
(require 'ox-md)

(let ((readme-file "README")
      exported-file
      (final-file "README.md"))
  (with-current-buffer (find-file-noselect readme-file)
    (setq exported-file (org-md-export-to-markdown)))
  (rename-file exported-file final-file t))
