;; -*- emacs-lisp -*-
;;
;; Oops, See also: mm-examples.el
;

(defun test-find-header-at-point ()
  "Test new function."
  (interactive)
  (save-excursion
    (setq filename-begin (point))
    (setq filename-end (+ 2 (point)))
    (setq filename (buffer-substring-no-properties filename-begin filename-end))
    )
  )
  

