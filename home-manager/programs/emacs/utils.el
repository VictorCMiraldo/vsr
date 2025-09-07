(defun vcm/zipper-next (z)
  (pcase z
    (`(,prev ,next0 . nil)
      (pcase next0
        (`(,this ,next . ,rest) (list (cons this prev) (cons next rest)))
        (_ (progn
	     (message "end")
             z))))
    (_ (error "Expecting a zipper"))))

(defun vcm/zipper-prev (z)
  (pcase z
    (`(,prev0 ,next . nil)
      (pcase prev0
        (`(,prev . ,rest) (list rest (cons prev next)))
        (_ (progn
	     (message "end")
             z))))
    (_ (error "Expecting a zipper"))))

(defun vcm/zipper-this (z)
  (pcase z
    (`(,prev ,next0 . nil)
      (pcase next0
        (`(,this . ,rest) this)
        (_ z)))
    (_ (error "Expecting a zipper"))))

(defun vcm/zipper-length (z)
  (pcase z
    (`(,prev ,next . nil) (+ (length prev) (length next)))
    (_ (error "Expecting a zipper"))))

(setq vcm/var/buffer-zipper '(() ()))
(setq vcm/var/aux-buffer-last-time 0)
(defun vcm/set-buffer-zipper ()
  "Sets the vcm/var/buffer-zipper to '(() (filter ... (buffer-list))) if called with
   more than 2s after the last time that variable was reset"
  (let ((now (time-convert (current-time) 'integer)))
       (if (> now (+ vcm/var/aux-buffer-last-time 2))
           (setq vcm/var/buffer-zipper
              (list '() (seq-filter #'(lambda (x) (not (null (buffer-file-name x)))) (buffer-list))))
       )
       (setq vcm/var/aux-buffer-last-time now)
       vcm/var/buffer-zipper)
)

(defun vcm/goto-file-buffer (nxt)
  "Switches to the next buffer that visits a file, as oposed to local buffers such as *Messages*
   The 'nxt' argument is a function from a zipper to a zipper"
  (interactive)
  (let ((bufs (vcm/set-buffer-zipper)))
       (if (< 1 (vcm/zipper-length bufs))
           (progn
             (setq vcm/var/buffer-zipper (funcall nxt bufs))
             (switch-to-buffer (vcm/zipper-this vcm/var/buffer-zipper)))
           (message "Only one file buffer: %s" bufs)))
)

(defun vcm/prev-file-buffer ()
  "Switches to the buffer that was visited previously to this. If you repeat this command, you'll see
   buffers alternating."
  (interactive)
  (vcm/goto-file-buffer #'vcm/zipper-prev)
)

(defun vcm/next-file-buffer ()
  "Switches to the previous file buffer"
  (interactive)
  (vcm/goto-file-buffer #'vcm/zipper-next)
)
