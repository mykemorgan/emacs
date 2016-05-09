;; -*- emacs-lisp -*-
;;
;; elisp file for playing around and learning/debugging.
;;
;; Some random commands or packages to look up/explore later:
;;
;; longlines-mode
;; visual-line-mode
;; - Maybe one of those will change to the behavior I want to C-n and C-p with lines that wrap
;;
;; markdown-mode.el
;; - has gfm-mode for GitHub markdown as well.
;;
;; One built-in package manager:
;; M-x list packages
;; Not sure where the packages get installed?
;;
;; Conditionals help:
;; (if condition then-form else-forms)
;; then-form has to be (progn a b c) to do multiple things. Else can be multiple
;; (when condition a b c) is like (if condition (progn a b c) nil)

(defun mm-test ()
  "
Test elisp function for goofing around.
"
  (interactive)
  (let (
        (begin "Beginning")
        )
    (let (
          (end (concat begin "End"))
          )
      (message "Current word: %s" (current-word))
      )
    )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages stuff from mpavlinksky
;; https://github.com/mpavlinsky/emacsconfig
;;

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (package-initialize)

;; (setq prelude-packages
;;   '(smex projectile helm helm-projectile ibuffer-vc ag key-chord company omnisharp flycheck go-mode company-go))

;; (defun prelude-packages-installed-p ()
;;   (loop for p in prelude-packages
;;         when (not (package-installed-p p)) do (return nil)
;;         finally (return t)))

;; (unless (prelude-packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs Prelude is now refreshing its package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p prelude-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))
