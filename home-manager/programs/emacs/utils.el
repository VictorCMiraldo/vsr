(defun vcm/buffer-file-list ()
  "Returns the list of files that are open in some buffer"
  (seq-filter #'(lambda (x) (not (null (buffer-file-name x)))) (buffer-list))
)

(defun vcm/goto-file-buffer (nxt)
  "Switches to the next buffer that visits a file, as oposed to local buffers such as *Messages*
   The 'nxt' argument is a function that given a list of (at least 2) buffers, selects the next one"
  (interactive)
  (let ((bufs (vcm/buffer-file-list)))
       (if (< 1 (length bufs))
           (switch-to-buffer (funcall nxt bufs))
           (message "Only one file buffer: %s" bufs)))
)

(defun vcm/next-file-buffer ()
  "Switches to the next file buffer"
  (interactive)
  (vcm/goto-file-buffer 'cadr)
)

(defun vcm/prev-file-buffer ()
  "Switches to the previous file buffer"
  (interactive)
  (vcm/goto-file-buffer #'(lambda (x) (car (last x))))
)
