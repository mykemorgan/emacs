;; Add a function to easily set up environment variables for compile.
(defun my-set-footag-environment (basedir)
  "ARGS: basedir
Sets the following Footag environment variables for compilation:
Variable        Set to
-------------------------------------------------------
VAR1     basedir/foodangle1
VAR2     basedir/foodangle2/subdir
  (interactive "DBasedir:")
  (save-excursion
    (setenv "VAR1" (concat basedir "/foodangle1"))
    (setenv "VAR2" (concat basedir "/foodangle2/subdir"))))

(global-set-key [f12] 'my-set-footag-environment)

