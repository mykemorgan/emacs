;> [ Victor Zandy, 04 Apr 2000 14:11 ]
;> >     Need to check an RFC?  Type M-x rfc and the RFC #, and, with this
;> > function, Emacs will get it for you, if it hasn't already.  Works on
;> > GNU Emacs 20.3.
;> 
;> This was very useful.  But often I don't know the number of the RFC,
;> but I would like to search for keywords.  I hacked up your function
;> and added a new one.  If you say M-x rfc-name you can type in a regexp
;> to search for.  Then you will get a list of matches (a simple search
;> in the rfc-index file), before you get asked for the number.
;
;I think it should use view-file instead of find-file.

(defvar rfc-path "/anonymous@ftp.isi.edu:in-notes/")
(defvar loc-rfc-path  "~/doc/RFC/")
(defun view-with-cache (remote-path 
			cache-path filename 
			&optional buf)
  (let ((locfullname (concat cache-path filename))
	(ftpfullname (concat remote-path filename))
	(buffer (or buf "in-notes")))
    (if (file-exists-p locfullname)
	(view-file locfullname)
      (or (get-buffer buffer)
	  ;; FIXME: We can't visit the file unless we first visit the
	  ;; directory.
	  (find-file-noselect rfc-path))
      (message (concat "Looking for " filename "..."))
      (sit-for 1)
      (view-file ftpfullname)
      (write-file locfullname))))

(defun rfc (num)
  (interactive "sRFC #")
  (let* ((filename (concat "rfc" num ".txt")))
    (view-with-cache rfc-path loc-rfc-path filename "in-notes")))

(defun rfc-name (name)
  (interactive "sRFC match-pattern (regexp): ")
  (let ((orig-buffer (current-buffer)))
    (let ((buffer (get-buffer "rfc-index.txt")))
      (if buffer
	  (set-buffer buffer)
	(view-with-cache rfc-path loc-rfc-path "rfc-index.txt")))
    (let ((pattern (concat "^0*\\([0-9]+\\) .*\\(" name "\\).*$"))
	  (tmpbuffer (get-buffer-create "*rfcindex*"))
	  (num nil)
	  (string ""))
      (with-current-buffer tmpbuffer (erase-buffer))
      (beginning-of-buffer)
      (while (re-search-forward pattern nil t) 
	(unless num (setq num (match-string 1)))
	(setq string (match-string 0))
	(with-current-buffer tmpbuffer
	  (insert string "\n")))
      (switch-to-buffer tmpbuffer)
      (if num 
	  (rfc (read-from-minibuffer "RFC #" num))
	(switch-to-buffer orig-buffer)
	(message "No RFC's match")))))
