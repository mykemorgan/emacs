;; -*- emacs-lisp -*-
;;
;; Face functions and settings.
;
; Helpful hints:
; M-x list-faces-display lists all faces and allows changing!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

; Faces for light backgrounds
(defun mm-face-light-background ()
  "Set the font-lock faces for emacsen with light backgrounds."
  (interactive)
  (set-face-foreground  'font-lock-function-name-face "blue4")
  (set-face-foreground  'font-lock-variable-name-face "blue4")
  (set-face-foreground  'font-lock-type-face          "blue4")
  (set-face-foreground  'font-lock-string-face        "green4")
  (set-face-foreground  'font-lock-builtin-face       "black")
  (when (or (and (>= emacs-major-version 22) (>= emacs-minor-version 1))
          (>= emacs-major-version 23))
      (set-face-foreground  'minibuffer-prompt "black")
    )
  (when (>= emacs-major-version 23)
      (set-face-background  'show-paren-match "lightblue")
      (set-face-foreground  'show-paren-match "black")
    )
  (set-cursor-color "blue")

  ;; Some dynamically displayed faces
  ;; the 'current' isearch item
  (set-face-background  'isearch                      "palevioletred2")
  (set-face-foreground  'isearch                      "black")
  ;; the 'next' isearch item
  (set-face-background  'lazy-highlight               "palevioletred4")
  ;;  (set-face-foreground  'lazy-highlight               "black")

  ;; -------------------------------------------------------------------------
  ;; Override Faces needing changing on gnu/linux to "simple terminal colors":
  ;; Remember to do this last, haha.
  (when (eq system-type 'gnu/linux)
    (set-face-foreground  'font-lock-function-name-face "blue")
    (set-face-foreground  'font-lock-variable-name-face "blue")
    (set-face-foreground  'font-lock-type-face          "blue")
    (set-face-foreground  'font-lock-string-face        "green")
    (set-face-background  'isearch                      "magenta")
    (set-face-foreground  'isearch                      "black")
    (set-face-background  'lazy-highlight               "cyan")
    (set-face-background  'region                       "cyan")
    (when (>= emacs-major-version 23)
      (set-face-background  'show-paren-match "magenta")
      (set-face-foreground  'show-paren-match "black")
      )
    )
  )

; Faces for dark backgrounds.
(defun mm-face-dark-background ()
  "Set the font-lock faces for emacsen with dark backgrounds."
  (interactive)
  (set-face-foreground  'font-lock-function-name-face "yellow")
  (set-face-foreground  'font-lock-variable-name-face "yellow")
  (set-face-foreground  'font-lock-type-face          "cyan")
  (set-face-foreground  'font-lock-string-face        "green2")
  (set-face-foreground  'font-lock-builtin-face       "white")
  (when (or (and (>= emacs-major-version 22) (>= emacs-minor-version 1))
          (>= emacs-major-version 23))
      (set-face-foreground  'minibuffer-prompt "gray80")
    )
  (when (>= emacs-major-version 23)
    (set-face-background  'show-paren-match "lightblue")
    (set-face-foreground  'show-paren-match "black")
    )
  (set-cursor-color "yellow")

  ;; Some dynamically displayed faces
  ;; the 'current' isearch item
  (set-face-background  'isearch                      "palevioletred2")
  (set-face-foreground  'isearch                      "black")
  ;; the 'next' isearch item
  (set-face-background  'lazy-highlight               "gray65")

  ;; -------------------------------------------------------------------------
  ;; Override Faces needing changing on gnu/linux to "simple terminal colors":
  ;; Maybe all that needs to be done is make the terminal 'xterm' type?
  ;; Remember to do this last, haha.
  (when (eq system-type 'gnu/linux)
    (set-face-foreground  'font-lock-string-face        "green")
    (set-face-background  'isearch                      "yellow")
    (set-face-background  'lazy-highlight               "white")
    (set-face-background  'region                       "cyan")
    (when (>= emacs-major-version 23)
      (set-face-background  'show-paren-match "yellow")
      (set-face-foreground  'show-paren-match "black")
      )
    )
  )


; There are many new faces in emacs version 22.1 and later.
(if (or (and (>= emacs-major-version 22) (>= emacs-minor-version 1))
        (>= emacs-major-version 23))
    (progn
      ;; Face to use for preprocessor commands.
      (set-face-foreground  'font-lock-preprocessor-face  "dark blue")
      (custom-set-faces
       '(minibuffer-prompt ((t (:bold t))))
       '(mode-line-inactive ((t (:foreground "black" :background "gray60"))))
;       '(mode-line-inactive ((t (:foreground "black" :background "goldenrod"))))
       '(mode-line-buffer-id ((t (:bold t :foreground "darkgreen"))))
       )
      )
  )

; Set a dark background
(defun mm-set-dark-background ()
  "Set the winow to a dark background color."
  (interactive)
  (set-foreground-color "white")
;  (set-background-color "gray40")
  (set-background-color "#224466")
  (mm-face-dark-background)
  )

; Set a light background
(defun mm-set-light-background ()
  "Set the windo to a light background color."
  (interactive)
  (set-foreground-color "black")
  (set-background-color "cornsilk")
  (mm-face-light-background)
  )

; This seems to be the only way to set default "bold" properties?
(custom-set-faces
 '(default ((t (:family "Hack" :height 120))))
 '(markdown-header-face ((t (:family "Hack" :bold t :foreground "yellow"))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :weight normal))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 160))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 180))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 200))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 240))))
 '(markdown-inline-code-face ((t (:family "Courier New" :height 150 :background "slategray4"))))
 '(markdown-pre-face ((t (:family "Courier New" :height 150 :foreground "aquamarine" :background "slategray4"))))
 '(font-lock-comment-face ((t (:italic t :foreground "red"))))
 '(font-lock-function-name-face ((t (:bold t))))
 '(mode-line ((t (:bold t :foreground "black" :background "pink"))))
 )

;; Mac specific settings
(if (or (eq system-type 'darwin)
        (eq system-type 'gnu/linux))
    ;; mac specific settings. really, window-ized emacs settings
    (mm-face-dark-background)
  ;; else probably in a terminal... Assume dark?
  ;; Is there a way to query the terminal background color?
  (mm-face-dark-background)
  ;; Oddly enough, on linux/21.4.1 the mode face fore/background are reversed???
  (set-face-background  'mode-line "yellow")
  (if (or (and (>= emacs-major-version 22) (>= emacs-minor-version 1))
          (>= emacs-major-version 23))
      (set-face-background  'mode-line-inactive "white")
    )
  (set-face-foreground  'mode-line "blue")
  )
