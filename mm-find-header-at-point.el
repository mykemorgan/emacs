;; -*- emacs-lisp -*-
;;
;; 
;
; XXX/mm Will (current-word) get a fill filename type string within quotes
; instead of all this?
;                                                                                                       
; TODO:                                                                                                 
; Try to look for headers not containing slashes in current directory?                                  
; Look for headers with <> in standard locations?                                                       
; For PayPal infra headers, look in infra dir?                                                          
;                                                                                                       
;
(defun mm-find-header-at-point ()
"
Find and load the given header file around the current point.
If set, prepends $BUILDTOP/ from environment to the name of the file.
If file does not exist, do not try to create it.
"
  (interactive)
  (save-excursion
    (skip-chars-backward "^\"\n")
                                        ; Did we find a " or newline or begin of file
    (if (and (> (point) 1) (= (char-before) ?\"))
        (let ((filename-begin (point)))
          (skip-chars-forward "^\"\n")
                                        ; Did we find a " or newline or end of file.
          (if (and (< (point) (1+ (buffer-size))) (= (char-after) ?\"))
                                        ; Add BUILDTOP if set
              (let ((filename 
                     (concat (getenv "BUILDTOP") 
                             (if (getenv "BUILDTOP") "/") 
                             (buffer-substring-no-properties filename-begin (point)))))
                (print (concat "Trying to load header file " filename) t)
                                        ; Only bother trying to load if it exists already
                (if (file-exists-p filename)
                    (find-file filename))
               )
            )
         )
      )
    )
  )
