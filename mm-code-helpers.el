;; -*- emacs-lisp -*-
;;
;; Example macro for F2 key:
;(defun my-save-buffer-and-tex-file ()
; (interactive)
; (save-buffer)
; (tex-file))
;
;(global-set-key [f2] 'my-save-buffer-and-tex-file)

;; Yank and indent at same time.
(defun mm-yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (indent-region (mark t) (point) nil))

;; Add a new block of code, hopefully indented.
(defun mm-add-new-c-block ()
  "Add a new block of curly-braced code, and attempt to indent it."
  (interactive)
  (let ((beg (point)))
    (insert "{\n\n}\n")
    (indent-region beg (point) nil))
  (previous-line 2)
  (indent-according-to-mode)
  (end-of-line))

(global-set-key [f2] 'mm-add-new-c-block)

;; Add a new c function, hopefully indented, with err and bail stuff...
(defun mm-add-new-c-function ()
  "Add a new indented function with proper err variable and bail tag set up."
  (interactive)
  (let ((beg (point)))
    (insert "{\n\tint err = 0;\n\n\tbail_false(false); /* XXX/mm need to finish */\n\nbail:\n\treturn (err);\n}\n")
    (indent-region beg (point) nil))
  (previous-line 4)
  (indent-according-to-mode)
  (end-of-line))

;(global-set-key [f3] 'mm-add-new-c-function)

;; Add a new flow state
(defun mm-add-new-flow-state ()
  "Add a new flow state template for an fxml file."
  (interactive)
  (let ((beg (point)))
    (insert "\t<State>\n")
    (insert "\t\t<Id></Id>\n")
    (insert "\t\t<Path></Path>\n")
    (insert "\t\t<Transition>\n")
    (insert "\t\t\t<Id></Id>\n")
    (insert "\t\t\t<NextState></NextState>\n")
    (insert "\t\t</Transition>\n")
    (insert "\t</State>\n"))
  (previous-line 7)
  (end-of-line)
  (backward-char 5))

;(global-set-key [f3] 'mm-add-new-flow-state)

;; Add a new c++ class, complete with ccdoc header to fill in.
;; XXX/mm It would be really nice to add the class name too!
(defun mm-add-ccdoc-class ()
  "Add a new ccdoc-style class header comments."
  (interactive)
  (let ((beg (point)))
    (insert " // =================================================================\n")
    (insert " //@{\n")
    (insert " // short explination.\n")
    (insert " // <begin long explination>\n")
    (insert " // \n")
    (insert " // @author <a href=\"mailto:xxx@yyy.zzz\">myke morgan</a>\n")
    (insert " // @since \n")
    (insert " // @todo Everything\n")
    (insert " // @version 0.1\n")
    (insert " //@}\n")
    (insert " // =================================================================\n")
    (insert "\tclass XXX {\n")
    (insert "\tpublic:\n\n")
    (insert "\tprotected:\n\n")
    (insert "\tprivate:\n\n")
    (insert "\t};")
    (indent-region beg (point) nil))
  (previous-line 5)
  (indent-according-to-mode)
  (end-of-line))

;(global-set-key [f4] 'mm-add-ccdoc-class)

;; Add a new ccdoc c++ method comment.
(defun mm-add-ccdoc-method ()
  "Add a new ccdoc-style method header comments."
  (interactive)
  (let ((beg (point)))
    (insert " //@{\n")
    (insert " // \n")
    (insert " // \n")
    (insert " // @param \n")
    (insert " // @returns \n")
    (insert " //@}\n")
    (indent-region beg (point) nil))
  (indent-according-to-mode)
  (previous-line 5)
  (end-of-line))

;(global-set-key [f5] 'mm-add-ccdoc-method)

;; Add a function to easily #define out a marked region.
;; From the newsgroup comp.emacs.sources
(defun mm-c-insert-ifdef (start end macro)
  "ARGS: start end macro
\"ifdef's\" the selected region, marking it with
#ifdef <macro>
      ....
#endif <macro>
The user is prompted for <macro> in the minibuffer."
  (interactive "r\nsMacro: ")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let ((if-statement (concat "#ifdef " macro "\n\n")))
      (insert if-statement)
      (goto-char (1- (+ end (length if-statement))))
      (end-of-line)
      (if (bolp)
          (insert (concat "\n#endif " macro "\n"))
        (insert (concat "\n\n#endif /* " macro " */\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goto-next-error ()
  "Vist next compilation error message and corresponding source code."
  (interactive)
  (next-error))

(defun goto-prev-error ()
  "Vist previous compilation error message and corresponding source code."
  (interactive)
  (next-error -1))

(defun find-files-in-directory (directory fmatch)
  "Finds the files which match an expresion in the current dir"
  (interactive "DDirectory to find files in: \nsPattern to match: ")
  (let* ((fl (directory-files directory t fmatch nil))
	 (numfound (length (mapcar 'may-find-file fl))))
    (message "Loaded %d files" numfound)))

(defun find-c-and-h-files-in-directory (directory)
  "Finds the files which match .*\\.[ch] expresion in the current dir"
  (interactive "DDirectory to find .c and .h files in: ")
  (find-files-in-directory directory ".*\\.[ch]"))

(defun scroll-line-up ()
  "Scrolls up a line, remaining at the same place on the screen"
  (interactive)
  (scroll-up 1)
  (next-line 1))

(defun scroll-line-down ()
  "Scrolls down a line, remaining at the same place on the screen"
  (interactive)
  (scroll-up -1)
  (next-line -1))


; Helper to set the tab width properly, while also telling C-modes to
; update their idea of how much to indent (one tab).
(defun mm-set-tab-width (width)
"Set the tab-width for the file, and then set the c-basic-offset
to that same tab width.

This facilitates c-mode code formatting rules that indent using a set
number of tabs and want to play nice with adjusting the tab-width
after the file has been loaded"
  (interactive "nDesired tab-width: ")
  (setq tab-width width
        c-basic-offset tab-width)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adds an open-closed pair of symbols, such as '()',
;; when you hit the '(' key.
;;
;; This is just playing around with elisp.
;; There are existing packages for this like:
;; - smartparans
;;
;; Commented-out option to just add a single '(' sometimes, like as
;; long as you're not in the middle of text.
(defun mm-insert-balanced (openchar)
  "Insert a pair of parenthesis if you hit '('.
There is a commented-out option to check that you are not mid-text.
(That is, if next char is space or newline, or end of buffer.)

To undo a key set, use something like this:
(global-set-key \"z\" (lambda () (interactive) (insert \"z\")))

Currently recognizes these pairs:
()
[]
{}
<>"
  (interactive)
  (let ((c (char-after (point)))
        (closed (cond ((= openchar ?\() "()")
                      ((= openchar ?\[) "[]")
                      ((= openchar ?\<) "<>")
                      ((= openchar ?\{) "{}")
                      ;; Oops, we don't have a closing match for your character.
                      ;; Add new pairs here if you want your own.
                      ))
        )
    (cond
     ((and (not (eq closed nil))
;           (or (not c) (= c 32) (= c 10)) ; Uncomment this if you want the "don't do this mid-text"
           ) (let () (insert closed) (backward-char 1)))
     (t (insert openchar)))))

;; What if we set to something that doesn't have a open/close pair?
;(global-set-key "z" (lambda () (interactive) (mm-insert-balanced ?\z)))
;; Fix problems with setting a key like this:
;(global-set-key "z" (lambda () (interactive) (insert "z")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; From mgainer, fixed by myke for some special cases.
; Commented out code gives some funky results for
; function calls in argument lists.
(defun tabs-and-spaces-indent ()
  (setq here-syn (c-guess-basic-syntax))
  (if (memq (car (car here-syn)) '(arglist-cont-nonempty arglist-close))
      ; Make sure we don't screw up cursor position
      (save-excursion    
        ; Get into whitespace at beginning of line.
        (beginning-of-line)
        ; Leave point here while we work out where open-paren is.
        (save-excursion
          ; Move up to open paren for function
          (backward-up-list 1)
          ; Save the column number of the enclosing open-thing.
          (setq rightcol (current-column)) 
          ; Back to beginning of line where function call starts
          (beginning-of-line) 

          ; Count number of tab and space characters at start of line
          (setq leading-tabs (skip-chars-forward "\t")) 
          (setq leading-spaces (skip-chars-forward " ")) 
          ; Count number of whitespace characters at start of line
;          (setq leading-ws (skip-chars-forward " \t")) 
          )

;        ; Assume tabification did same thing to this line as func call.
;        ; Skip over as many characters in this line as in func call open.
;        (forward-char leading-ws) 
        ; Skip over leading tabs only.
        (forward-char leading-tabs) 

        ; Insert whitespace as spaces up to desired indentation
        (insert (make-string (+ 1 (- rightcol (current-column))) ? )) 

        ; Delete any tabs that our insertion pushed over.
        (let ((beg (point))) 
          (skip-chars-forward " \t")
          (delete-region beg (point))
          )
        )
    )
  (if (memq (car (car here-syn)) '(c))
      (progn
        (save-excursion
          (beginning-of-line)
          (save-excursion
            (goto-char (cdr (car here-syn)))
            (skip-chars-forward "*<!/")
            (skip-chars-forward " \t")
            (setq rightcol (current-column))
            (beginning-of-line)
            (setq leading-tabs (skip-chars-forward "\t")) 
            (setq leading-spaces (skip-chars-forward " ")) 
;            (setq leading-ws (skip-chars-forward " \t"))
            )
          (forward-char leading-tabs) 
;          (forward-char leading-ws)
          (insert (make-string (- rightcol (current-column)) ? ))
          (let ((beg (point)))
            (skip-chars-forward " \t")
            (delete-region beg (point))
            )
          )
        (skip-chars-forward " \t")
        )
    )
  )

; Create a hybrid indentation style:
; Tabs into the normal indentation level, then space into function
; argument or logical continuation lines.
;;
;; Don't use this any more!
;;(c-add-style "mm-tabs-and-spaces" '("k&r" (c-basic-offset . 2) (c-special-indent-hook . tabs-and-spaces-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
