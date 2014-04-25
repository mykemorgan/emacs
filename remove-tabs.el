;;
;; Soo, can someone tell me how to get emacs to automagically convert
;; tabs to spaces when I save a file?
;;

(defun my-untabify-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (untabify (point) (mark))))

(add-hook 'write-file-hooks 'my-untabify-buffer)

