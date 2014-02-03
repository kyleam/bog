```lisp
(let ((url "https://raw.github.com/kyleam/bog/master/README.org")
      (buffer (generate-new-buffer "*Bog README*")))
  (switch-to-buffer-other-window buffer)
  (url-insert-file-contents url)
  (org-mode))
```
