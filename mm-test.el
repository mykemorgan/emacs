;; -*- emacs-lisp -*-
;;
;; Oops, See also: mm-examples.el
;

(defun test-sample ()
  "Test new function."
  (interactive)
  (save-excursion
    (setq name-begin (point))
    (setq name-end (+ 4 (point)))
    (setq name (buffer-substring-no-properties name-begin name-end))
    (message "Found name: '%s'" name)
    )
  )
  

