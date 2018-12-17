;; -*- emacs-lisp -*-
;;
;; 
;                                                                                                       
;; Return the prefix of the given include file.
;;
;; XXX/mm Feature requests:
;; - auto-find the standard library C++ headers?
;; - Actually, this location should be definable by the user,
;;   but maybe with a default if not defined? Also see below.
;; - This should be done by having a "config variable" which is
;;   setq'able to contain a list of header directories to search.
;; - Have a list of string prefixes that signal which "system"
;;   location(s) to search, like is done manually with the "infra" and
;;   "infrastructure" below.
;;
;; All the above is so things can be user-customizable without
;; changing this code depending on the project. Would allow others to
;; use and point to their own infra repos as well, for exmaple.
;;
;; Maybe instead just search in "known directories" by building an
;; full path filename and checking if it exists like the caller of
;; this function?
;;
(defun mm-get-include-file-prefix (file type)
  "
Examine the FILE assuming its an #include directive file name and attempt
to determine its root directory in the filesystem, based first on the TYPE
character:

Common root directories will be determined by:
  type = '\"' - env(BUILDTOP)
  type = '<'  - Infra include directory env(INFRAINCLUDE) if FILE begins
                with the string 'infra' or 'infrastructure'
  type = '<'  - Otherwise the system include directory env(SYSINCLUDE)

TODO?
- Need to make all these 'system' and 'infra' locations configurable
  beyond environment variables? Use emacs variables?
"
  (interactive "Mfile:\nctype:")
  (let (
        (prefix (cond ((= type ?\")
                       "buildtop")
                      ((= type ?<)
                       "system")))
        (sysinclude (getenv "SYSINCLUDE"))
        (infradir (getenv "INFRAINCLUDE"))
        )
    (cond ((string= prefix "buildtop")
           (if (string-match "/" file)
               (getenv "BUILDTOP")
             "."))
          ((eq t (compare-strings "infra" nil nil file nil 5))
           infradir)
          ((eq t (compare-strings "infrastructure" nil nil file nil 8))
           infradir)
          (t sysinclude)
          )
    )
  )

(defun mm-test-get-include-file-prefix ()
  (interactive)
  (let (
        ;; Save environment during test!
        (buildtop (getenv "BUILDTOP"))
        (sysinclude (getenv "SYSINCLUDE"))
        (infrainclude (getenv "INFRAINCLUDE"))
        )
    (setenv "BUILDTOP" "~/repo/foo/bar")
    (setenv "SYSINCLUDE" "/foo/c++/include")
    (setenv "INFRAINCLUDE" "~/repo/foo/infralocation")

    (let (
          (b1 (mm-get-include-file-prefix "apps/server/Foo/foo.h" ?\"))
          (b2 (mm-get-include-file-prefix "MyFoo.h" ?\"))
          (i1 (mm-get-include-file-prefix "infra/framework/foo.h" ?<))
          (i2 (mm-get-include-file-prefix "infrastructure/framework/Foo/foo.h" ?<))
          (s1 (mm-get-include-file-prefix "usr/local/foo.h" ?<))
          (s2 (mm-get-include-file-prefix "MyFoo.h" ?<))
          )
      (message "Prefixes found:")
      (message "  build1: [%s]" b1)
      (message "  build2: [%s]" b2)
      (message "  infra1: [%s]" i1)
      (message "  infra2: [%s]" i2)
      (message "  systm1: [%s]" s1)
      (message "  systm2: [%s]" s2)
      (message "Results:")
      (if (string= b1 (getenv "BUILDTOP"))
          (message "  SUCCESS b1")
        (message "  FAIL b1")
        )
      (if (string= b2 ".")
          (message "  SUCCESS b2")
        (message "  FAIL b2")
        )
      (if (string= i1 (getenv "INFRAINCLUDE"))
          (message "  SUCCESS i1")
        (message "  FAIL i1")
        )
      (if (string= i2 (getenv "INFRAINCLUDE"))
          (message "  SUCCESS i2")
        (message "  FAIL i2")
        )
      (if (string= s1 (getenv "SYSINCLUDE"))
          (message "  SUCCESS s1")
        (message "  FAIL s1")
        )
      (if (string= s2 (getenv "SYSINCLUDE"))
          (message "  SUCCESS s2")
        (message "  FAIL s2")
        )
      )
    ;; Restore environment
    (setenv "BUILDTOP" buildtop)
    (setenv "SYSINCLUDE" sysinclude)
    (setenv "INFRAINCLUDE" infrainclude)
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

;;----------------------------------------------------------------------
;; Test Data in which to park your point when calling this function
;; Success: "Sample string in quotes"
;; Success: <Sample string in less/greater than>
;; Fail: [Sample string in square brackets]
;; Fail: {Sample string in curly brackets}
;; (The fails are due to mm-find-enclosing-header-delimiter not yet looking
;;  for the brackets).
;;----------------------------------------------------------------------
(defun mm-test-find-enclosed-string ()
  "
Test the get enclosing delimiter and get enclosed string
functions. Displays the delimiter for and string it finds
given the current point.
"
  (interactive)
  (let ((delim (mm-find-enclosing-header-delimiter)))
    (if delim
        (message "SUCCESS: Delim [%c] String [%s]"
                 delim
                 (mm-get-enclosed-string delim))
      (message "FAIL: Didn't find a begin or possibly an end")
        )
    )
  )

(defun mm-get-enclosed-string (delim-begin &optional delim-end)
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
