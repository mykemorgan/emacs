;; -*- emacs-lisp -*-
;;
;; 
;                                                                                                       


; Return the prefix of the given include file
; XXX/mm Feature request: auto-find the C++ headers as well.
; They seem to be in /usr/include/c++/<version>/
;
; This should be done by having a "config variable" which is setq'able
; to contain a list of header directories to search. This way can be
; user-customizable. Would allow others to use and point to their own
; infra repos as well, for exmaple.
;
; And/Or, a list of string prefixes that signal which "system"
; location to search, like I do manually with the "infra" and
; "ifeature" below.
;
; Or, should I be doing all this by building an actual filename and
; checking if it exists like the caller of this function?
;
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
          (t "/usr/include")
          )
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
Find and load the given header filename around the current point.
If set, prepends $BUILDTOP/ from environment to the name of the file.
If file does not exist, do not try to create it.
"
  (interactive)
  (let ((delim (mm-find-enclosing-header-delimiter)))
    (if delim
        (save-excursion
          (let ((header (mm-get-enclosed-string delim)))
            (let ((path (mm-get-include-file-prefix header delim)))
              (let ((filename (concat path (if path "/") header)))
                                        ; Only bother trying to load if it exists already
                (if (file-exists-p filename)
                    (progn (find-file filename)
                           (message "Loaded header file: %s" filename))
                  (message "Failed to load. File does not exist: %s" filename)
                  )
                )
              )
            )
          )
      )
    )
  )

;----------------------------------------------------------------------
; Test Data in which to park your point when calling this function
; Success: "Sample string in quotes"
; Success: <Sample string in less/greater than>
; Fail: [Sample string in square brackets]
; Fail: {Sample string in curly brackets}
; (The fails are due to mm-find-enclosing-header-delimiter not yet looking
;  for the brackets).
;----------------------------------------------------------------------
(defun mm-test-find-enclosed-string ()
  "
Test the get enclosing delimiter and get enclosed string
functions. Displays the delimiter for and string it finds
given the current point.
"
  (interactive)
  (let ((delim (mm-find-enclosing-header-delimiter)))
    (if delim
        (message "Found Delim [%c]\nFound string [%s]"
                 delim
                 (mm-get-enclosed-string delim))
      (message "Didn't find a begin or possibly an end")
        )
    )
  )

(defun mm-get-enclosed-string (&optional delim-begin &optional delim-end)
  "
Get the string enclosed by the delimiters delim-begin and delim-end.
Both delimiters are optional: see below for behvior if they are nil.
Returns nil if such a string does not exist all on a single line.

If delim-begin is nil, sets it to &\"
Then, if delim-end is nil, sets delim-end based on delim-begin:
delim-begin  delim-end
    ?\"           ?\"
    ?<           ?>
    ?[           ?]
    ?{           ?}
"
  (interactive)
  (setq delim-begin (if delim-begin delim-begin ?\"))
  (setq delim-end (if delim-end
                      (delim-end)
                    (cond ((eq delim-begin ?\") delim-begin)
                          ((eq delim-begin ?<) ?>)
                          ((eq delim-begin ?[) ?])
                          ((eq delim-begin ?{) ?})
                          )
                    ))
  (if delim-end
      (let (
            (back-search (concat "^\n" (string delim-begin)))
            (fwd-search (concat "^\n" (string delim-end)))
            )
                                        ; do the stuff
        (save-excursion
          (skip-chars-backward back-search)
                                        ; Did we find a begin thing?
          (if (and (> (point) 1) (= (char-before) delim-begin))
              (let (
                    (name-begin (point)) ; Save the beginning of string
                    )
                (skip-chars-forward fwd-search)
                                        ; Did we find a " or newline or end of file.
                (if (and (< (point) (1+ (buffer-size)))
                         (= (char-after) delim-end))
                                        ; Extract the name between delims
                    (buffer-substring-no-properties name-begin (point))
                  )
                )
            )
          )
        )
    nil
    )
  )

(defun mm-find-enclosing-header-delimiter ()
  "
Find and return the 'header file delimiter' surrounding the current
point.
\"mydir/myheader.h\"   - returns &\"
<asm/ioctl.h>         - returns &<

If no end-region character is found appropriate to the begin-region
character, returns nil.
For now, only finds delimiters surrounding the point on the same
line as the point.
"
  (interactive)
  (save-excursion
    ; XXX/mm add single-quotes? Seems to be used in oml files.
    (skip-chars-backward "^\"<\n")
                                        ; Did we find a begin-char or newline / begin of file
    (if (and (> (point) 1)
             (or (= (char-before) ?\") (= (char-before) ?<)))
        (let (
              (found-delim-begin (char-before))
              )
          ; XXX/mm Should only skip forward until the appropriate end-string character?
          (skip-chars-forward "^\">\n")
                                        ; Did we find an end-char or newline / end of file.
          (if (and (< (point) (1+ (buffer-size)))
                   (or (= (char-after) ?\") (= (char-after) ?>)))
              (let (
                    (found-delim-end (char-after))
                    )
                ; Make sure the begin char matches the end char
                (cond ((and (= found-delim-begin ?\")
                            (= found-delim-end ?\"))
                       found-delim-begin)
                      ((and (= found-delim-begin ?<)
                            (= found-delim-end ?>))
                       found-delim-begin)
                      ((t nil))
                      )
                )
            )
         )
      ; Couldn't find a begin char
      nil
      )
    )
  )
