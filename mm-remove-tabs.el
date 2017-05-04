;;
;; emacs to automagically convert tabs to spaces when I save a file
;;

(defun mm-untabify-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (untabify (point) (mark))))

(add-hook 'write-file-hooks 'mm-untabify-buffer)

