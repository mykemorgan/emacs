;; $Id: mm-project-helpers.el,v 1.1 2005/09/14 19:11:40 mimorgan Exp $

(defun mm-load-most-used-files ()
  "
Load a sampling of API related files into buffers

It's generally simpler to use the 'desktop' package to save and
recover buffer states between emacs executions.

But this function will do the job, as well as being a place to
remember various important files.
"
  (interactive)
  (save-excursion
    (find-file (concat (getenv "BUILDTOP") "/foo/bar/whatever.cpp"))
;    (find-file (concat (getenv "BUILDTOP") ""))
  )
)


