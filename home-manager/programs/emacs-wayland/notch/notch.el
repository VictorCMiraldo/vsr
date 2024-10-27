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
;; I much rather have a predictable behavior of the `<tab>' key, so I wrote
;; my own variation on the `indent-for-tab-command'. This works best by
;; also binding `<backtab>' to `notch-back'.
;;
;; I tried to be "as compatible as possible" with
;;
;; This is not portable, this will probably not "just work", but it works for
;; me. :)

(defun previous-line-notch ()
  "Returns the indentation level of the previous non-empty line or zero if at the
beginning of the buffer"
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward "^[^\n]" nil t)
      (let ((end (save-excursion (forward-line 1) (point))))
        (or (looking-at "[ \t]")
            (skip-chars-forward "^ \t" end))
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
      ((or (not prev-notch) (>= this-notch prev-notch))
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

(defun notch-point-in-line-state ()
  "Returns this line's and point state. Returns:

     'in-blank-line OR integerp: when the line is empty or the prefix
        up to the point is only whitespace characters. The returned int is
        the identation level of this line: the column of the first non-whitespace
        character in this line.

     'in-bol when the point is at the beginning of the line on a non-empty line.

     'in-eow when the point is at the end of a word.

     'in-middle when the point is at the middle of a non-empty line AND
        the prefix up to the point contains non-whitespace characters."
  (save-excursion
    (cond
      ;; TODO: unify the blank prefix and blank line, it's the same situation.
      ;; Check for beginning or end of line. Beginning first.
      ((string-match "^[[:blank:]]*\n$" (thing-at-point 'line t))
        'in-blank-line)

      ;; Ok, not bol nor eol!
      ;; Now, try to skip backwards until we're not seeing a tab or a space.
      ;; if we skip nothing, we're mid word, or at the beginning of a line.
      ((= (skip-chars-backward " \t") 0)
         (let ((syn (syntax-class (syntax-after (point)))))
           (cond
             ;; The beginning-of-line on a non-empty line should be treated especially,
             ;; since `syn' will be 0.
             ((bolp) 'in-bol)

             ;; 0 and 12 are for newlines an spaces after the point.
             ((memql syn '(0 12)) 'in-eow)

             ;; Finally, if nothing matched, we're in an arbitrary point in the middle
             ;; of the line.
             (t 'in-middle))))

      ;; If the above check skipped all the way to the beginning,
      ;; we are in the blank-prefix.
      ((= (current-column) 0)
        (skip-chars-forward " \t"))

      ;; Else, we're in the middle of a non-empty line.
      (t 'in-middle))
  )
)

(defun in-commentp ()
  "Returns t or nil, depending on whether or not the current point is inside a comment."
  (nth 4 (syntax-ppss)))

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
    ((in-commentp) (notch-line))
     (t
       (let ((old-tick (buffer-chars-modified-tick))
             (old-point (point))
             (this-notch (current-indentation))
             (prev-notch (previous-line-notch))
             (state (notch-point-in-line-state)))

         ;; Now, let's look at what's happening in the current line.
         (pcase state
           ;; We're in a blank line (or in the blank prefix) just bring it to
           ;; the same level as the previous indented line, similar to the behavior
           ;; of `indent-relative-first-indent-point'.
           ((or 'in-bol 'in-middle (pred integerp))
             (notch-line))

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
  "Cycles the current line between the previous line plus and minus one `standard-indent'."
  (interactive)
  (let ((this-notch (current-indentation))
        (cur (current-column)))
    (forward-line 0)
    (delete-horizontal-space)
    ;; This line has an indent, bring it back.
    (unless (<= this-notch 0)
      (indent-to (- this-notch standard-indent)))

    ;; Once we're done, we move back to the relative position from
    ;; the beginning of the line.
    (forward-char (- cur this-notch)))
)

(provide 'notch)
;;; notch.el ends here
