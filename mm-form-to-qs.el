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
  "
Transform a chunk of html <form> code doing a POST into a GET
URL with querystring parameters.

Only transforms the given region.

For example, if selecting the whole form region,
will change this form POST:

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

TODO

Start at the point, go backward to the <form> beginning, and grab
the region outselves. That way user just has to have cursor in
the <form>...

Or just go back to <form> and move forward until find the
</form>, since as we replace text, the area where we need to look
shrinks, and thus might accidentally grab another form right after
the current one.
"
  (interactive)
  (save-excursion
    (let ((form-beg (if (< (point) (mark)) (point) (mark)))
          (form-end (if (> (point) (mark)) (point) (mark))))

      ;; Docs say manner of search/replace is better in Lisp programs
      ;; than using replace-string.
      
      ;; Things to add a newline after
      (goto-char form-beg)
      (search-forward-regexp "<form .* action=\"" form-end t)
      (replace-match "" nil t)
      (search-forward-regexp "\">\n" form-end t)
      (replace-match "?" nil t)
                                        ;      (setq form-end (+ form-end 1))
      (while (search-forward-regexp " *<input *type=hidden.*name=" form-end t)
        (replace-match "" nil t)
        (search-forward-regexp " value=\"*" form-end t)
        (replace-match "=" nil t)
        (search-forward-regexp "\"*>\n" form-end t)
        (replace-match "&" nil t)
        )

      (search-backward "&" form-beg t)
      (replace-match "" nil t)

      (search-forward-regexp " *<input type=submit.*>" form-end t)
      (replace-match "" nil t)
      (search-forward-regexp " *</form>" form-end t)
      (replace-match "" nil t)
      )
    )
  )
