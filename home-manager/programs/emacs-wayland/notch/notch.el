;;; notch.el --- Oppinionated indentation functions. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Victor Miraldo

;; Author: Victor Miraldo <victor.miraldo@fastmail.com>
;; Keywords: indentation, indent
;; Homepage: https://github.com/VictorCMiraldo/vsr/home-manager/programs/emacs/notch
;; Package-Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Indentation in emacs has way to many options I don't care about, in fact,
;; I much rather have a predictable behavior of the `TAB' command, so I wrote
;; my own variation on the `indent-for-tab-command'. This works best by
;; also binding `<backtab>' to `notch-back'.
;;
;; This is not portable, this will probably not "just work", but it works for
;; me. :)
;;
;; Notes for self: 'TAB' is different from '<tab>'. The former is they ascii
;; caracter 9 (also produced with `C-i'). The later is the tab key in your keyboard.
;; If you bind `<tab>' to `notch-for-tab-command' you probably will need to
;; `<remap>' it in other modes. Using 'TAB' instead for `notch' should work.

(defun previous-line-notch ()
  "Returns the indentation level of the previous non-empty line or zero if at the
beginning of the buffer"
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward "^[^\n]" nil t)
      (let ((end (save-excursion (forward-line 1) (point))))
        (skip-chars-forward " \t" end)
        (current-column)
      )
    )
  )
)

(defun notch-line ()
  "Indents the current line all the way to the previous line indent, if we're before
that point. Otherwise, always indents the line `standard-indent' forward, using
`indent-to'. The `notch-back' sends the line back.
"
  (let ((this-notch (current-indentation))
        (prev-notch (previous-line-notch))
        (cur (current-column)))
    (forward-line 0)
    (delete-horizontal-space)
    (cond
      ;; (1) At or ahead the previous line
      ((or (not prev-notch) (= 0 prev-notch) (>= this-notch prev-notch))
         (let* ((tgt (+ this-notch standard-indent))
                (diff (mod tgt standard-indent)))
           (indent-to (- tgt diff))))
      ;; (2) Before the previous line, move it to the previous line
      (t (indent-to prev-notch)))

    ;; Can't really use `save-excursion' because we're changing the line, so we
    ;; do some good old point arithmetic here. Once we're done, we move back to
    ;; the relative position from the beginning of the line.
    (forward-char (- cur this-notch))
))

(defcustom notch-punctuation-is-eow nil
  "Whether the next symbol being some punctuation mark means the point is at the end of the word.
Languages, like Python, can use this setting to help auto-completion trigger a little more aggressively.
In languages like Agda, this is a bad idea."
  :type 'boolean
  :group 'notch)

(defun notch-point-in-line-state ()
  "Returns this line's and point state. Returns:

     'in-blank-prefix when the cursor is in a point in the line where forall chars
     before the point in the same line, they are all whitespace.

     'in-eow when the point is at the end of a word. This setting is affected
     by `notch-punctuation-is-eow'.

     'in-middle when the point is at the middle of a non-empty line AND
        the prefix up to the point contains non-whitespace characters."

  ;; 0 and 12 are for newlines an spaces after the point.
  ;; 1, 3 and 4 is for punctuation
  (let ((eow-syn-classes (append '(0 12) (if notch-punctuation-is-eow '(1 3 4) nil))))
    (save-excursion
      (cond
        ;; Try to skip backwards until we're not seeing a tab or a space.
        ;; if we skip nothing, we're mid word, or at the beginning of a line.
        ((= (skip-chars-backward " \t") 0)
           (let ((syn (syntax-class (syntax-after (point)))))
             (cond
               ;; The beginning-of-line on a non-empty line should be treated especially,
               ;; since `syn' will be 0. Still, we want to return an integer, as the point
               ;; is, technicall, in the (non-existent) blank prefix.
               ((bolp) 'in-blank-prefix)

               ;; In case point is in the end of a word, we return a special tag: 'in-eow
               ((memql syn eow-syn-classes) 'in-eow)

               ;; Finally, if nothing matched, we're in an arbitrary point in the middle of the line.
               (t 'in-middle-of-line))))

        ;; If the above check skipped all the way to the beginning,
        ;; we are in the blank-prefix.
        ((= (current-column) 0) 'in-blank-prefix)

        ;; Else, we're in the middle of a non-empty line.
        (t 'in-middle-of-line))
    )
  )
)

(defun in-commentp ()
  "Returns t or nil, depending on whether or not the current point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun notch-delete-trailing-blanks ()
  "Delete the trailing blank characters after the point."
  (while (memq (following-char) '(32))
    (delete-char 1)))

(defun notch-for-tab-command ()
  "This package's main function, replacement for `indent-for-tab-command'.

Indent the current line or triggers `completion-at-point'.
Completion is only triggered if the point is at the end of a word and
we are not inside a comment region. Otherwise, this function inserts
whitespace (calling `indent-to', which obeys `indent-tabs-mode').

If the point's column is before the previous line indentation,
this will indent the point to the previous line indentation, like
`indent-relative-first-indent-point'. After that, we will indent
`standard-indent' forward. Bind '<backtab>' to `notch-back' to
move indentation backwards.
"
  (interactive)
  (cond
    ;; If we're in a comment block, just notch ahead!
    ((in-commentp) (notch-line))

    ;; Otherwise, it get's fun!
     (t
       (let ((old-tick (buffer-chars-modified-tick))
             (old-point (point))
             (col (current-column))
             (this-notch (current-indentation))
             (prev-notch (previous-line-notch))
             (state (notch-point-in-line-state)))

         ;; Now, let's look at what's happening in the current line.
         (pcase state
           ;; We're in the blank-prefix of a line, either move forward until we're at the
           ;; previous notch, or add a notch. Somewhat similar to the behavior
           ;; of `indent-relative-first-indent-point'.
           ('in-blank-prefix
            (progn
              (notch-delete-trailing-blanks)
              (if (<= this-notch prev-notch)
                  (if (= (skip-chars-forward " \t") 0) (notch-line))
                  (notch-line)
              )))

           ;; We're in the middle of the line, nothing to think about, just notch
           ('in-middle-of-line (notch-line))

           ;; Let's delete anything that might be ahead of the point, which will prevent
           ;; blank spaces at the end of the line and won't interfere with the carefully
           ;; crafted white-space arithmetic of `notch-line'.
           ('in-blank-line
             (kill-line)
             (open-line 1)
             (notch-line))

           ;; We're in the end of a word, good place to launch a completion!
           ('in-eow (completion-at-point))

           (_ (error "notch-point-in-line-state: bad return: %s" state))
         ))
    ))
)

(defun notch-back ()
  "Moves the current line back one `standard-indent' or to the beginning of the line"
  (interactive)
  (let ((this-notch (current-indentation)))
    ;; This line has an indent, bring it back.
    (unless (<= this-notch 0)
      (let* ((cur (current-column))
            (relative-pos (- cur this-notch)))
        (forward-line 0)
        (delete-horizontal-space)
        (indent-to (max 0 (- this-notch standard-indent)))
        ;; Once we're done, we move back to the relative position from
        ;; the beginning of the line, if we were mid line; otherwise,
        ;; a tab will get us to the beginning.
        (unless (< relative-pos (- standard-indent this-notch))
            (forward-char relative-pos)))
    ))
)

(provide 'notch)
;;; notch.el ends here
