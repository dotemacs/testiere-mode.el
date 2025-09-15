;;; testiere-mode.el --- Hide/show #+testiere blocks  -*- lexical-binding: t; -*-

;; Author: Aleksandar Simic <a@repl.ist>
;; Maintainer: Aleksandar Simic <a@repl.ist>
;; Version: 0.1.0
;; Keywords: tools, lisp, convenience
;; URL: https://github.com/dotemacs/testiere-mode.el
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; This package provides simple helpers to toggle visibility of Common
;; Lisp testiere library for inline tests, available from here
;; https://cicadas.surf/cgit/colin/testiere.git/about/, marked with
;; "#+testiere" headers.  Place point inside a def* form
;; (defun/defmethod/deftype/defclass/defstruct) or on a line with a
;; "#+testiere" header and call `testiere-toggle` to hide/show the
;; associated tests.
;;
;; Suggested keybinding:
;;
;; Common Lisp buffers
;;  (with-eval-after-load 'lisp-mode
;;    (define-key lisp-mode-map (kbd "C-c t") #'testiere-toggle))
;;
;; SLIME
;;  (with-eval-after-load 'slime
;;    (define-key slime-mode-map (kbd "C-c t") #'testiere-toggle))
;;
;; SLY
;;  (with-eval-after-load 'sly
;;    (define-key sly-mode-map (kbd "C-c t") #'testiere-toggle))
;;
;;; Code:

(require 'seq)
(require 'rx)

(defun testiere--block-end-at (start)
  "Return end position of the testiere block starting at START.
START must be at the beginning of a line containing a #+testiere header.
If no s-expression follows on the next non-empty line, return the end of
that line, so that malformed content is still considered part of the block."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (while (and (not (eobp)) (looking-at "^[ \t]*$"))
      (forward-line 1))
    (cond
     ((eobp) start)
     (t
      (let ((open-paren-pos (save-excursion
                              (re-search-forward "(" (line-end-position) t))))
        (if open-paren-pos
            (progn
              (goto-char open-paren-pos)
              (condition-case _
                  (progn (backward-char) (forward-sexp 1) (point))
                (error (line-end-position))))
          (line-end-position)))))))

(defun testiere-find-block-start ()
  "Find the start of a #+testiere block that contains point.
Returns the position of the #+testiere line, or nil if not found."
  (save-excursion
    (let ((original-point (point))
          (found nil)
          (case-fold-search t))
      (while (and (not found)
                  (re-search-backward (rx bol (* (any " \t")) "#+testiere" symbol-end) nil t))
        (let* ((testiere-pos (line-beginning-position))
               (block-end (testiere--block-end-at testiere-pos)))
          (when (and (>= original-point testiere-pos)
                     (<= original-point block-end))
            (setq found testiere-pos))))
      found)))

(defun testiere-toggle-at-current-line ()
  "Toggle testiere block at current line."
  (let* ((start (line-beginning-position))
         (end (testiere--sexp-end-after-header start))
         (overlay (seq-find (lambda (o)
                              (and (eq (overlay-get o 'category) 'testiere-sexp)
                                   (<= (overlay-start o) start)
                                   (>= (overlay-end o) start)))
                            (overlays-in start (min (point-max) (1+ start))))))
    (if overlay
        (progn
          (delete-overlay overlay)
          (message "Showed #+testiere block"))
      (if (and end (> end start))
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'category 'testiere-sexp)
            (overlay-put ov 'invisible t)
            (overlay-put ov 'display
                         (propertize "  #+testiere ..."
                                     'face 'font-lock-comment-face))
            (message "Hid #+testiere block"))
        (message "Nothing to hide")))))

(defun testiere-find-hidden-overlay ()
  "Find a hidden testiere overlay near point."
  (let ((overlays (overlays-in (point-min) (point-max))))
    (seq-find (lambda (o) (eq (overlay-get o 'category) 'testiere-sexp))
              overlays)))

(defun testiere--sexp-end-after-header (start)
  "Return end position of s-expression following header at START.
If there is no valid s-expression on the next non-empty line, return nil."
  (save-excursion
    (goto-char start)
    (if (not (looking-at (rx bol (* (any " \t")) "#+testiere" symbol-end)))
        nil
      (forward-line 1)
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (forward-line 1))
      (if (eobp)
          nil
        (let ((open-paren-pos (save-excursion
                                (re-search-forward "(" (line-end-position) t))))
          (when open-paren-pos
            (goto-char open-paren-pos)
            (condition-case _
                (progn (backward-char) (forward-sexp 1) (point))
              (error nil))))))))

(defun testiere--in-string-or-comment-p (&optional pos)
  "Return non-nil if POS (or point) is in string or comment."
  (let* ((ppss (syntax-ppss (or pos (point))))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    (or in-string in-comment)))

(defun testiere--current-def-bounds-and-kind ()
  "If point is inside a supported def form, return (START END KIND).
KIND is one of symbols: defun, defmethod, deftype, defclass, defstruct.
Returns nil if none found. Uses regex search to avoid mode dependencies."
  (save-excursion
    (let* ((case-fold-search t)
           (original (point))
           (result nil))
      (while (and (not result)
                  (re-search-backward
                   (rx bol "(" (* (any " \t"))
                       (group (or "defun" "defmethod" "deftype" "defclass" "defstruct"))
                       symbol-end)
                   nil t))
        (let* ((kind-str (downcase (match-string 1)))
               (start (match-beginning 0)))
          (goto-char start)
          (condition-case _
              (let ((end (save-excursion (forward-sexp 1) (point)))
                    (kind (intern kind-str)))
                (when (and (>= original start) (<= original end))
                  (setq result (list start end kind))))
            (error nil))))
      result)))

(defun testiere--skip-def-header (start kind)
  "From START at beginning of a def KIND, move point after header parts.
KIND is one of symbols supported by `testiere--current-def-bounds-and-kind'.
Returns point position just before body forms."
  (save-excursion
    (goto-char start)
    (forward-char 1)
    (forward-sexp 1)
    (pcase kind
      ('defun
          (forward-sexp 1)
          (forward-sexp 1)
        (skip-syntax-forward " ")
        (when (eq (char-after) ?\") (forward-sexp 1))
        (skip-syntax-forward " ")
        (while (looking-at (rx "(" (* space) "declare" symbol-end))
          (forward-sexp 1)
          (skip-syntax-forward " ")))
      ('defmethod
       (forward-sexp 1)
       (skip-syntax-forward " ")
       (while (and (not (eobp)) (not (looking-at "(")))
         (forward-sexp 1)
         (skip-syntax-forward " "))
       (when (looking-at "(") (forward-sexp 1))
       (skip-syntax-forward " ")
       (when (eq (char-after) ?\") (forward-sexp 1))
       (skip-syntax-forward " ")
       (while (looking-at (rx "(" (* space) "declare" symbol-end))
         (forward-sexp 1)
         (skip-syntax-forward " ")))
      ('deftype
          (forward-sexp 1)
          (skip-syntax-forward " ")
        (when (looking-at "(") (forward-sexp 1))
        (skip-syntax-forward " ")
        (when (eq (char-after) ?\") (forward-sexp 1)))
      ('defclass
        (forward-sexp 1)
        (skip-syntax-forward " ")
        (when (looking-at "(") (forward-sexp 1))
        (skip-syntax-forward " ")
        (when (looking-at "(") (forward-sexp 1))
        (skip-syntax-forward " ")
        (when (eq (char-after) ?\") (forward-sexp 1)))
      ('defstruct
          (forward-sexp 1)
        (skip-syntax-forward " ")
        (when (looking-at "(") (forward-sexp 1))
        (skip-syntax-forward " ")
        (when (eq (char-after) ?\") (forward-sexp 1))))
    (point)))

(defun testiere--find-header-in-def (start end kind)
  "Search between START and END of a def KIND for a #+testiere header.
Returns the beginning of the header line, or nil if not found."
  (save-excursion
    (let ((scan-start (testiere--skip-def-header start kind))
          (case-fold-search t)
          (found nil))
      (goto-char scan-start)
      (beginning-of-line)
      (while (and (not found)
                  (re-search-forward (rx bol (* (any " \t")) "#+testiere" symbol-end) end t))
        (let ((pos (match-beginning 0)))
          (unless (testiere--in-string-or-comment-p pos)
            (setq found (line-beginning-position)))))
      found)))

;;;###autoload
(defun testiere-toggle ()
  "Toggle visibility of testiere tests."
  (interactive)
  (let ((testiere-start (testiere-find-block-start)))
    (if testiere-start
        (save-excursion
          (goto-char testiere-start)
          (testiere-toggle-at-current-line))
      (let* ((def-info (testiere--current-def-bounds-and-kind))
             (header-pos (when def-info
                           (testiere--find-header-in-def (nth 0 def-info)
                                                         (nth 1 def-info)
                                                         (nth 2 def-info))))
             (header-in-list
              (when (eq (char-after) ?\()
                (save-excursion
                  (let ((case-fold-search t)
                        (start (point))
                        (end (save-excursion (condition-case _ (forward-sexp 1) (error nil)) (point)))
                        (found nil))
                    (when (and end (> end start))
                      (goto-char start)
                      (while (and (not found)
                                  (re-search-forward (rx bol (* (any " \t")) "#+testiere" symbol-end) end t))
                        (let ((pos (match-beginning 0)))
                          (unless (testiere--in-string-or-comment-p pos)
                            (setq found (line-beginning-position)))))
                      found))))))
        (cond
         (header-pos
          (save-excursion
            (goto-char header-pos)
            (testiere-toggle-at-current-line)))
         (header-in-list
          (save-excursion
            (goto-char header-in-list)
            (testiere-toggle-at-current-line)))
         (t
          ;; Fallback: look for any hidden overlay in buffer
          (let ((hidden-overlay (testiere-find-hidden-overlay)))
            (if hidden-overlay
                (progn (delete-overlay hidden-overlay)
                       (message "Showed #+testiere block"))
              (message "No #+testiere block found to toggle")))))))))

;;;###autoload
(defalias 'testiere-toggle-tests #'testiere-toggle)

(provide 'testiere-mode)

;;; testiere-mode.el ends here
