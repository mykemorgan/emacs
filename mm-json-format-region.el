;; -*- emacs-lisp -*-
;;

;; XXX Or, you know, RTFM and use json-pretty-print in the json package :)
;;
;;-------------------------------------------------------------------------------
;; JSON region formatter function:
;; In region, replace:
;; { - newline after
;; [ - newline after
;; , - newline after
;; } - newline before
;; ] - newline before
;;
;; Then format-region
;;
;; Obvious improvement: check if the characters above are already
;; "preceeded by a newline". This is tricky because after formatting
;; this would really mean "preceeded on a line only by whitespace". If
;; this is the case, don't add another newline. Then let the region
;; formatter do it's thing.

(defun mm-json-format-region ()
  "
**********************************************************************
* NOTE: json-pretty-print is probably what you really want to use.
**********************************************************************

Perform a simple format and indent on the current region,
based on JSON rules.

First, within the region, replace JSON delimiter characters with the
same character preceeded or followed by a newline:
{ - newline after
[ - newline after
, - newline after
} - newline before
] - newline before

Second, indent the resulting region using whatever current mode
is running. Obviously this is most useful when the current mode
is Javascript or some sort of JSON mode.

Tested with Javascript mode.

Obvious TODO: check if the characters above are already
'preceeded/followed by a newline' so repeated formats don't keep
adding newlines.
"
  (interactive)
  (save-excursion
    (let ((json-beg (if (< (point) (mark)) (point) (mark)))
          (json-end (if (> (point) (mark)) (point) (mark))))

      ;; Docs say manner of search/replace is better in Lisp programs
      ;; than using replace-string.
      
      ;; Things to add a newline after
      (goto-char json-beg)
      (while (search-forward "{" json-end t)
        (replace-match "{\n" nil t)
        (setq json-end (+ json-end 1))) ; extend the region with added newline
      (goto-char json-beg)
      (while (search-forward "[" json-end t)
        (replace-match "[\n" nil t)
        (setq json-end (+ json-end 1))) ; extend the region with added newline
      (goto-char json-beg)
      (while (search-forward "," json-end t)
        (replace-match ",\n" nil t)
        (setq json-end (+ json-end 1))) ; extend the region with added newline

      ;; Things to add a newline before
      (goto-char json-beg)
      (while (search-forward "}" json-end t)
        (replace-match "\n}" nil t)
        (setq json-end (+ json-end 1))) ; extend the region with added newline
      (goto-char json-beg)
      (while (search-forward "]" json-end t)
        (replace-match "\n]" nil t)
        (setq json-end (+ json-end 1))) ; extend the region with added newline

      ;; Everything newline'd: now just indent region according to the mode
      (indent-region json-beg json-end nil)
      )
    )
  )
