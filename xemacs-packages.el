;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Customization of Specific Packages                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load and  customize the Font-Lock syntax-highlighting package
(require 'font-lock)
(add-hook 'font-lock-mode-hook  'turn-on-fast-lock)
(setq-default font-lock-maximum-decoration t)
(setq-default font-lock-maximum-size 1000000)
(setq fast-lock-cache-directories '("~/emacs/fast-lock/"))
; Face to use for comments
(set-face-foreground  'font-lock-comment-face       "red")
; Face to use for function and variable names
(set-face-foreground  'font-lock-function-name-face "blue4")
(set-face-foreground  'font-lock-variable-name-face "blue4")
; Face to use for strings and documentation strings
(set-face-foreground  'font-lock-string-face        "green4")
(set-face-foreground  'font-lock-doc-string-face    "green4")
; Face to use for preprocessor commands
(set-face-foreground  'font-lock-preprocessor-face  "dark blue")

; Face to use for keywords
(set-face-foreground  'font-lock-keyword-face  "blue3")
; Face to use for reference names
(set-face-foreground  'font-lock-reference-face  "red3")
; Face to use for type names
(set-face-foreground  'font-lock-type-face          "medium orchid")
;(set-face-underline-p 'font-lock-string-face nil)
;(make-face-unitalic   'font-lock-string-face)
;(make-face-unitalic   'font-lock-doc-string-face)
;(make-face-unitalic   'font-lock-comment-face)
;(make-face-unitalic   'font-lock-function-name-face)
;(make-face-unitalic   'font-lock-type-face)
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'lisp-mode-hook       'turn-on-font-lock)
(add-hook 'c-mode-hook          'turn-on-font-lock)
(add-hook 'c++-mode-hook        'turn-on-font-lock)
(add-hook 'perl-mode-hook       'turn-on-font-lock)
(add-hook 'tex-mode-hook        'turn-on-font-lock)
(add-hook 'bibtex-mode-hook     'turn-on-font-lock)
(add-hook 'texinfo-mode-hook    'turn-on-font-lock)
(add-hook 'postscript-mode-hook 'turn-on-font-lock)
(add-hook 'dired-mode-hook      'turn-on-font-lock)
(add-hook 'ada-mode-hook        'turn-on-font-lock)
(add-hook 'idl-mode-hook        'turn-on-font-lock)

;; Load the func-menu package (menubar entry for function definitions)
;;(require 'func-menu)
;;(add-hook 'find-file-hooks 'fume-add-menubar-entry)

;; Load search-menu, a popup window for control search and replace
;(cond ((not (eq (locate-library "search-menu") nil))
;       (load-library "search-menu")))

;; Load the auto-save.el package, which lets you put all of your autosave
;; files in one place, instead of scattering them around the file system.
(require 'auto-save)
(setq auto-save-directory (expand-file-name "~/emacs/autosaves/")
      auto-save-directory-fallback auto-save-directory
      auto-save-hash-p nil
      auto-save-interval 2000)

;; Load a partial-completion mechanism
;;(load-library "completer")

;; Load the file template thingy.
(require 'template)
(template-initialize)
