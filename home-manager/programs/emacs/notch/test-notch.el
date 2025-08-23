(require 'notch)

(ert-deftest test-previous-line-notch-works-bol ()
  "Tests that we can recognize the point is at the blank prefix in an empty line"
  (with-temp-buffer
    (insert "\n")
    (insert "def first_line():\n")
    (insert "")
    (should (equal (previous-line-notch) 0))))

(ert-deftest test-previous-line-notch-works-2 ()
  "Tests that we can recognize the point is at the blank prefix in an empty line"
  (with-temp-buffer
    (insert "\n")
    (insert "  some function in Haskell\n")
    (insert "")
    (should (equal (previous-line-notch) 2))))

(ert-deftest test-notch-detects-blank-prefix-empty ()
  "Tests that we can recognize the point is at the blank prefix in an empty line"
  (with-temp-buffer
    (insert "def first_line():\n")
    (insert "")
    (should (equal (notch-point-in-line-state) 'in-blank-prefix))))

(ert-deftest test-notch-detects-blank-prefix-non-empty ()
  "Tests that we can recognize the point is at the blank prefix in a non-empty line"
  (with-temp-buffer
    (insert "def first_line():\n")
    (insert "  ")
    (save-excursion (insert "  "))
    (should (equal (thing-at-point 'line) "    "))
    (should (equal (current-column) 2))
    (should (equal (notch-point-in-line-state) 'in-blank-prefix))))

(ert-deftest test-notch-at-bol ()
  "Tests indentation behavior at start of the line: indent to as many spaces
as `standard-indent' wants"
  (with-temp-buffer
    (insert "definition first_line():\n")
    (insert "")
    (notch-for-tab-command)
    (should (equal (thing-at-point 'line) (make-string standard-indent ? )))))

(ert-deftest test-notch-follows-previous-in-blank-prefix ()
  "Tests that indentation at the start of the line where the previous line
is indented: let's follow it"
  (with-temp-buffer
    (insert "        calling_function()\n")
    (insert "")
    (notch-for-tab-command)
    ;; Precondition: standard indent must be larger than 4
    (should (<= standard-indent 4))
    (should (equal (thing-at-point 'line) "        "))))

(ert-deftest test-notch-moves-point-in-blank-prefix ()
  "When previous line is indented and point is at the blank prefix
of a non-empty line, notch just moves the point."
  (with-temp-buffer
    (insert "        calling_function()\n")
    (insert "    ")
    (save-excursion (insert "    other_function()\n"))
    (notch-for-tab-command)
    (should (equal (current-column) 8))
    (should (equal (thing-at-point 'line) "        other_function()\n"))))

(ert-deftest test-notch-moves-whole-line ()
  "When previous line is indented and point is at the blank prefix
of a non-empty line, notch just moves the point."
  (with-temp-buffer
    (insert "    calling_function()\n")
    (insert "    other_fun")
    (save-excursion (insert "ction()\n"))
    (notch-for-tab-command)
    (should (equal (thing-at-point 'line)
                   (concat (make-string standard-indent ? ) "    other_function()\n")))))

(ert-deftest test-notch-bol-non-empty-line ()
  (with-temp-buffer
    (insert "definition test():\n")
    (insert "     ")
    (forward-line 0)
    (notch-for-tab-command)
    ;; Precondition: standard indent must be larger than 4
    (should (equal (thing-at-point 'line) (make-string standard-indent ? )))))

(ert-deftest test-notch-bol-non-empty-line-small-indent ()
  (with-temp-buffer
    (insert "  definition test():\n")
    (insert "     ")
    (forward-line 0)
    (notch-for-tab-command)
    ;; Precondition: standard indent must be larger than 4
    (should (equal (thing-at-point 'line) "  "))))

(ert-deftest test-notch-multiples-of-standard-indent ()
  (with-temp-buffer
    (insert "  definition test():\n")
    (insert "")
    (forward-line 0)
    (notch-for-tab-command)
    (notch-for-tab-command)
    ;; Precondition: standard indent must be  4
    (should (equal standard-indent 4))
    (should (equal (thing-at-point 'line) (make-string standard-indent ? )))))

(ert-deftest test-notch-back-preserves-point ()
  (with-temp-buffer
    (insert "        def ")
    (save-excursion (insert "one two three\n") (insert "another line"))
    (notch-back)
    ;; Precondition: standard indent must be 4
    (should (equal standard-indent 4))
    (should (equal (thing-at-point 'line) "    def one two three\n"))
    (should (equal (current-column) 8))))
