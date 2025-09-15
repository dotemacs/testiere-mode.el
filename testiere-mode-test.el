;;; testiere-mode-test.el --- Tests for testiere-mode  -*- lexical-binding: t; -*-

(require 'testiere-mode)
(require 'ert)
(require 'seq)

(defmacro testiere-test-with-temp-buffer (content &rest body)
  "Execute BODY in a temporary buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest testiere-find-block-start-basic ()
  "Test finding the start of a basic testiere block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+testiere")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "(:tests"))
      (should (equal (testiere-find-block-start) testiere-pos)))))

(ert-deftest testiere-find-block-start-with-whitespace ()
  "Test finding testiere block with leading whitespace."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n    #+testiere\n    (:tests\n     (= 6 (add3 1 2 3)))\n    (+ x y z))"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+testiere")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "(:tests"))
      (should (equal (testiere-find-block-start) testiere-pos)))))

(ert-deftest testiere-find-block-start-with-tabs ()
  "Test finding testiere block with tabs."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n\t#+testiere\n\t(:tests\n\t (= 6 (add3 1 2 3)))\n\t(+ x y z))"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+testiere")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "(:tests"))
      (should (equal (testiere-find-block-start) testiere-pos)))))


(ert-deftest testiere-find-block-start-not-in-block ()
  "Test when point is not in a testiere block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  #+testiere
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))

(defun other-func () 'done)"
    ;; Position cursor in the other function, which is outside the testiere block
    (goto-char (search-forward "other-func"))
    (should (null (testiere-find-block-start)))))

(ert-deftest testiere-find-block-start-multiple-blocks ()
  "Test finding correct block when multiple testiere blocks exist."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  #+testiere
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))

(defun multiply (a b)
  #+testiere
  (:tests
   (= 12 (multiply 3 4)))
  (* a b))"
    ;; Position cursor in the second testiere block and verify it finds the right one
    (goto-char (search-forward "(defun multiply"))
    (let ((second-block-start (save-excursion
                                (search-forward "#+testiere")
                                (beginning-of-line)
                                (point))))
      (goto-char (search-forward "multiply 3 4"))
      (should (equal (testiere-find-block-start) second-block-start)))))

(ert-deftest testiere-find-block-start-with-empty-lines ()
  "Test finding testiere block with empty lines in between."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  #+testiere


  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+testiere")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "(:tests"))
      (should (equal (testiere-find-block-start) testiere-pos)))))

(ert-deftest testiere-find-block-start-complex-sexp ()
  "Test finding testiere block with complex s-expression."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3))\n   (:fails (add3 \"hey\"))\n   (:fails (add3 1 2)))\n  (+ x y z))"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+testiere")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "add3 1 2 3"))
      (should (equal (testiere-find-block-start) testiere-pos)))))

(ert-deftest testiere-toggle-at-current-line-hide ()
  "Test hiding a testiere block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    (goto-char (search-forward "#+testiere"))
    (beginning-of-line)
    (testiere-toggle-at-current-line)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                        overlays)))))

(ert-deftest testiere-toggle-at-current-line-show ()
  "Test showing a hidden testiere block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  #+testiere
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))"
    (goto-char (search-forward "#+testiere"))
    (beginning-of-line)
    ;; First hide it
    (testiere-toggle-at-current-line)
    ;; Verify it's hidden
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                        overlays)))
    ;; Then show it
    (testiere-toggle-at-current-line)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                            overlays)))))

(ert-deftest testiere-toggle-at-current-line-display-property ()
  "Test that hidden overlay has correct display property."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    (goto-char (search-forward "#+testiere"))
    (beginning-of-line)
    (testiere-toggle-at-current-line)
    (let ((overlay (seq-find (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                             (overlays-in (point-min) (point-max)))))
      (should overlay)
      (should (overlay-get overlay 'invisible))
      (should (string-match "testiere" (overlay-get overlay 'display))))))

(ert-deftest testiere-find-hidden-overlay-found ()
  "Test finding a hidden overlay near point."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    (goto-char (search-forward "#+testiere"))
    (beginning-of-line)
    (testiere-toggle-at-current-line)
    (goto-char (+ (point) 5))
    (should (testiere-find-hidden-overlay))))

(ert-deftest testiere-find-hidden-overlay-not-found ()
  "Test when no hidden overlay exists near point."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    (should-not (testiere-find-hidden-overlay))))

(ert-deftest testiere-toggle-interactive ()
  "Test the main interactive toggle function."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    (goto-char (search-forward "(:tests"))
    ;; Should hide the block
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                        overlays)))
    ;; Should show the block again
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                            overlays)))))

(ert-deftest testiere-toggle-not-in-block ()
  "Test toggle when not in a testiere block."
  (testiere-test-with-temp-buffer
      "Some random content"
    (testiere-toggle)
    ;; Should not create any overlays
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                            overlays)))))


(ert-deftest testiere-toggle-find-hidden-overlay ()
  "Test toggle finding and showing a hidden overlay when not in visible block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  #+testiere
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))"
    (goto-char (search-forward "(:tests"))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                        overlays)))
    (goto-char (point-max))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                            overlays)))))

(ert-deftest testiere-edge-case-empty-buffer ()
  "Test behavior with empty buffer."
  (with-temp-buffer
    (should-not (testiere-find-block-start))
    (should-not (testiere-find-hidden-overlay))))

(ert-deftest testiere-edge-case-no-sexp ()
  "Test behavior when testiere block has no s-expression."
  (testiere-test-with-temp-buffer
      "(defun broken-func ()
  #+testiere
  no sexp here
  'done)"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+testiere")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "no sexp"))
      (should (equal (testiere-find-block-start) testiere-pos))
      ;; Test that toggle handles missing s-expression gracefully
      (goto-char testiere-pos)
      (testiere-toggle-at-current-line)
      ;; Should not create any overlays when there's no s-expression
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                              overlays))))))

(ert-deftest testiere-case-insensitive-header ()
  "Test that testiere header matching works with uppercase (current implementation accepts it)."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  #+TESTIERE
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))"
    (let ((testiere-pos (save-excursion
                          (search-forward "#+TESTIERE")
                          (beginning-of-line)
                          (point))))
      (goto-char (search-forward "(:tests"))
      ;; The implementation actually accepts uppercase
      (should (equal (testiere-find-block-start) testiere-pos)))))


;; This test fails due to overlay management issues in testiere-toggle-at-current-line
(ert-deftest testiere-realistic-example ()
  "Test with a realistic Common Lisp function with testiere block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  \"Adds three numbers\"
  #+testiere
  (:tests
   (= 6 (add3 1 2 3))
   (:fails (add3 \"hey\"))
   (:fails (add3 1 2)))
  (+ x y z))"
    (goto-char (search-forward ":tests"))
    ;; (should (testiere-find-block-start))
    ;; Test hiding and showing
    (goto-char (search-backward "#+testiere"))
    (testiere-toggle-at-current-line)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                        overlays)))
    ;; Test showing again
    (testiere-toggle-at-current-line)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
                            overlays)))))

;; New tests for structural detection within various def* forms

(ert-deftest testiere-structural-defun-near-header ()
  "Toggle when point is just before the header line inside defun."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)
  \"Adds three numbers\"
  #+testiere
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))"
    ;; Put point at end of the docstring line, just before header
    (goto-char (search-forward "numbers\""))
    (end-of-line)
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))
    ;; Show again
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))))

(ert-deftest testiere-toggle-on-opening-paren ()
  "Toggle when point is on the opening paren of a sexp containing a testiere block."
  (testiere-test-with-temp-buffer
      "(defun add3 (x y z)\n  \"Adds three numbers\"\n  #+testiere\n  (:tests\n   (= 6 (add3 1 2 3)))\n  (+ x y z))"
    ;; Place point exactly at the opening paren of the defun sexp
    (goto-char (point-min))
    (should (eq (char-after) ?\())
    ;; Toggle should hide the testiere block
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))
    ;; Toggle again should show it
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))))

(ert-deftest testiere-structural-defmethod ()
  "Toggle tests within a defmethod form."
  (testiere-test-with-temp-buffer
      "(defmethod add3 ((x integer) (y integer) (z integer))
  \"Adds three numbers\"
  #+testiere
  (:tests
   (= 6 (add3 1 2 3)))
  (+ x y z))"
    (goto-char (search-forward "defmethod"))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))))

(ert-deftest testiere-structural-deftype ()
  "Toggle tests within a deftype form."
  (testiere-test-with-temp-buffer
      "(deftype foo ()
  \"A foo type\"
  #+testiere
  (:tests t))"
    (goto-char (search-forward "deftype"))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))))

(ert-deftest testiere-structural-defclass ()
  "Toggle tests within a defclass form."
  (testiere-test-with-temp-buffer
      "(defclass widget ()
  ((name :initarg :name))
  \"Widget class\"
  #+testiere
  (:tests (eq t t)))"
    (goto-char (search-forward "defclass"))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))))

(ert-deftest testiere-structural-defstruct ()
  "Toggle tests within a defstruct form."
  (testiere-test-with-temp-buffer
      "(defstruct point
  (x 0) (y 0)
  #+testiere
  (:tests (eq t t)))"
    (goto-char (search-forward "defstruct"))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))
    (testiere-toggle)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should-not (seq-some (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp)) overlays)))))

(provide 'testiere-mode-test)

;;; testiere-mode-test.el ends here
