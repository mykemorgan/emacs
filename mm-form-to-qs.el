;; -*- emacs-lisp -*-
;;

;;-------------------------------------------------------------------------------
;; Change a form POST into a querystring based GET
;;
;; Change this form POST:
;;
;; <form class="wps" method=post action="https://www.sandbox.paypal.com/cgi-bin/webscr">
;;   <input type=hidden name=cmd value=_xclick>
;;   <input type=hidden name=undefined_quantity value="1">
;;   <input type=hidden name=business value="n8@x.com">
;;   <input type=hidden name=amount value="10.00">
;;   <input type=hidden name=item_name value="Myke-WPS-Test">
;;   <input type=hidden name=quantity value="1">
;;   <input type=hidden name=os0 value="large">
;;   <input type=hidden name=option_select0 value="large">
;;   <input type=hidden name=option_amount0 value="18.00">
;;   <input type=submit class="wpssubmit" value="Options Test L15">
;; </form>
;;
;; Into this URL to GET:
;;
;; https://www.sandbox.paypal.com/cgi-bin/webscr?cmd=_xclick&undefined_quantity=1&business=n8@x.com&amount=10.00&item_name=Myke-WPS-Test&quantity=1&os0=large&option_select0=large&option_amount0=18.00


(defun mm-form-to-qs ()
  "Transform a chunk of html <form> code doing a POST into a GET
URL with querystring parameters.

Starts at the current point and searches backwards to find the
\"<form\" tag to begin to transform.

For example, will change this form POST:

<form class=\"wps\" method=post action=\"https://www.sandbox.paypal.com/cgi-bin/webscr\">
  <input type=hidden name=cmd value=_xclick>
  <input type=hidden name=undefined_quantity value=\"1\">
  <input type=hidden name=business value=\"n8@x.com\">
  <input type=hidden name=amount value=\"10.00\">
  <input type=hidden name=item_name value=\"Myke-WPS-Test\">
  <input type=hidden name=quantity value=\"1\">
  <input type=hidden name=os0 value=\"large\">
  <input type=hidden name=option_select0 value=\"large\">
  <input type=hidden name=option_amount0 value=\"18.00\">
  <input type=submit class=\"wpssubmit\" value=\"Options Test L15\">
</form>

Into this URL to GET:

https://www.sandbox.paypal.com/cgi-bin/webscr?cmd=_xclick&undefined_quantity=1&business=n8@x.com&amount=10.00&item_name=Myke-WPS-Test&quantity=1&os0=large&option_select0=large&option_amount0=18.00

** TODO **

- Need to make sure we're within a <form> not just search backwards.
- Use replace-match with 3rd argument nil to allow for regex
  replacement strings and make code cleaner?
- URL escape the found param values? Since they could be quoted?
"
  (interactive)
  (save-excursion
    ;; Hack: move over beginning "<form" text so we get the one "we're in"
    (goto-char (+ (point) 5))
    (search-backward "<form")
    (setq form-beg (point))
    (search-forward "</form>")
    (setq form-end (point))

    (goto-char form-beg)
    ;; First find the action for the URL
    (search-forward-regexp "<form .* action=\"" form-end t)
    (replace-match "" nil t)
    (search-forward-regexp "\">\n" form-end t)
    (replace-match "?" nil t)
    ;; Find all the querystring params
    (while (search-forward-regexp " *<input *type=hidden.*name=" form-end t)
      (replace-match "" nil t)
      (search-forward-regexp " value=\"*" form-end t)
      (replace-match "=" nil t)
      (search-forward-regexp "\"*>\n" form-end t)
      (replace-match "&" nil t)
      ;; reset region boundary since we removed stuff
      (setq here (point))
      (search-forward "</form>")
      (setq form-end (point))
      (goto-char here)
      )

    (search-backward "&" form-beg t)
    (replace-match "" nil t)

    (search-forward-regexp " *<input type=submit.*>" form-end t)
    (replace-match "" nil t)
    (search-forward-regexp " *</form>" form-end t)
    (replace-match "" nil t)
    )
  )
