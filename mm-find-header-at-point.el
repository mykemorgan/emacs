;; -*- emacs-lisp -*-
;;
;; 
; XXX/mm Will (current-word) help to get a fill filename type string
; within quotes instead of all this?
;                                                                                                       


; Return the prefix of the given include file
(defun mm-get-include-file-prefix (file type)
  "
Examine the FILE assuming its an #include directive file name and attempt
to determine its root directory in the filesystem, based first on the TYPE
character:

Common root directories will be determined by:
  type = '\"' - env(BUILDTOP)
  type = '<'  - Infra top if FILE begins with 'infra' or 'ifeature'
  type = '<'  - Otherwise the system include directory
"
  (interactive "Mfile:\nctype:")
  (let (
        (prefix (cond ((= type ?\")
                       "buildtop")
                      ((= type ?<)
                       "system")))
        )
    (cond ((string= prefix "buildtop")
           (if (string-match "/" file)
               (getenv "BUILDTOP")
             "."))
          ((eq t (compare-strings "infra" nil nil file nil 5))
           "~/repositories/infra/all")
          ((eq t (compare-strings "ifeature" nil nil file nil 8))
           "~/repositories/infra/all")
          (t "/usr/include"))
    )
  )

(defun mm-test-get-include-file-prefix ()
  (interactive)
  (let (
        (b1 (mm-get-include-file-prefix "applogic/server/Merchant/foo.h" ?\"))
        (b2 (mm-get-include-file-prefix "MyFoo.h" ?\"))
        (i1 (mm-get-include-file-prefix "infra/framework/foo.h" ?<))
        (i2 (mm-get-include-file-prefix "ifeature/framework/asf/foo.h" ?<))
        (s1 (mm-get-include-file-prefix "usr/local/foo.h" ?<))
        (s2 (mm-get-include-file-prefix "MyFoo.h" ?<))
        )
    (message "Prefixes found:\nb1: [%s]\nb2: [%s]\ni1: [%s]\ni2: [%s]\ns1: [%s]\ns2: [%s]" b1 b2 i1 i2 s1 s2)
    )
  )

(defun mm-find-header-at-point ()
  "
Find and load the given header file around the current point.
If set, prepends $BUILDTOP/ from environment to the name of the file.
If file does not exist, do not try to create it.
"
  (interactive)
  (save-excursion
    (skip-chars-backward "^\"<\n")
                                        ; Did we find a " or newline or begin of file
    (if (and (> (point) 1)
             (or (= (char-before) ?\") (= (char-before) ?<)))
        (let (
              (filename-begin (point))        ; Save the beginning of file
              (type-of-header (char-before))  ; Save the way #include includes the header
              )
          (skip-chars-forward "^\">\n")
                                        ; Did we find a " or newline or end of file.
          (if (and (< (point) (1+ (buffer-size)))
                   (or (= (char-after) ?\") (= (char-after) ?>)))
                                        ; Add the file prefix we believe is correct
              (let ((header (buffer-substring-no-properties filename-begin (point))))
                (let ((path (mm-get-include-file-prefix header type-of-header)))
                  (let ((filename (concat path (if path "/") header)))
                    (message "Trying to load header file: %s" filename)
                                        ; Only bother trying to load if it exists already
                    (if (file-exists-p filename)
                        (find-file filename)
                      (message "Failed to load. File does not exist: %s" filename)
                      )
                    )
                  )
                )
            )
         )
      )
    )
  )
