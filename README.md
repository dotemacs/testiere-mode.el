# `testiere-mode`

Hide or show inline Common Lisp tests written with the
[`testiere`](https://cicadas.surf/cgit/colin/testiere.git/about/)
testing library. `M-x testiere-toggle` command that collapses or
reveals the `#+testiere` sections.

## Example usage

This is how the code looks with `testiere`:
```lisp
(defun add3 (x y z)
  "Adds three numbers"
  #+testiere
  (:tests
   (= 6 (add3 1 2 3))
   (:fails (add3 "hey"))
   (:fails (add3 1 2)))
  (+ x y z))
```

and this is how it looks toggled via `M-x testiere-toggle`:
```lisp
(defun add3 (x y z)
  "Adds three numbers"
  #+testiere ...
  (+ x y z))
```

## Features
- Works from anywhere inside a supported `def*` form (`defun`,
  `defmethod`, `deftype`, `defclass`, `defstruct`) or from the header
  line itself

## Installation

```elisp
(use-package testiere-mode
  :vc (:fetcher github :repo "dotemacs/testiere-mode.el"))
```

## Usage
1. Open a Common Lisp buffer that contains inline tests wrapped in a `#+testiere` block.
2. Place point either on the header line or anywhere inside the surrounding `defun`, `defmethod`, `deftype`, `defclass`, or `defstruct` form.
3. Run `M-x testiere-toggle` (or invoke the `testiere-toggle-tests` alias). The block will collapse into a friendly comment-like overlay.
4. Call the command again to reveal the tests. Repeated invocations cycle between the hidden and visible states.

### Suggested key bindings
Attach the command to a convenient key in the Lisp modes you use most:
```elisp
(with-eval-after-load 'lisp-mode
  (define-key lisp-mode-map (kbd "C-c t") #'testiere-toggle))

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c t") #'testiere-toggle))

(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c t") #'testiere-toggle))
```
