;; -*- emacs-lisp -*-
;;
;; 
;
; elisp file for playing around and learning/debugging.
;

(defun mm-test ()
  "
Test elisp function for goofing around.
"
  (interactive)
  (let (
        (mystring (read-string "Foo: " "INITIAL"))
        (begin "Beginning")
        )
    (print (concat "String: " mystring " Begin: " begin)))
)

;
; Test simple interactive function.
(defun mm-test-interactive (a b)
  "
Test elisp function for testing interactive reading of input.
"
  (interactive "nNumber: \nMString: ")
  (let ( 
        (begin "Beginning")
        )
    (message "Begin: %s / a: %d / b: %s / c: %s" begin a b))
)

;
; Test simple interactive function with other input also manually read.
(defun mm-test-interactive-manual (a b)
  "
Test elisp function for testing interactive reading of input.
"
  (interactive "nNumber: \nMString: ")
  (let (
        (mystring (read-string "Foo: " "INITIAL"))
        (begin "Beginning")
        )
    (message "mystring: %s / begin: %s / a: %d / b: %s" mystring begin a b))
)

;
; Test interactive function arguments with setting of defailt arguments manually.
(defun mm-test-interactive-default (begin end file)
  "
Examine the FILE assuming its an #include directive file name and attempt 
to determine its root directory in the filesystem.
Common root directories will be:
  BUILDTOP
  Infra top
  The system include directory
"
  (interactive
   (let (
         (string (read-string "Foo: " "INITIAL"))
         )
     (list (region-beginning) (region-end) string)))
  (message "Begin: %d End: %d Input: %s" begin end file)
)

