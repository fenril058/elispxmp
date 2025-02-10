# elispxmp.el

It provides automatic annotation of Emacs Lisp code with evaluation
results.  The main function `elispxmp` processes the selected
region or the entire buffer, evaluates the expressions, and
annotates the code with the results.  It also includes a
customization for `comment-dwim` to add a " =>" mark when called
successively.  This is useful for quickly annotating code with
expected results or comments.

This package is an Emacs Lisp version of [Ruby's xmpfilter](https://github.com/rcodetools/rcodetools).
It was originally created by [rubikitch](https://github.com/rubikitch/)
and thepackage can be found on [lispxmp](https://github.com/rubikitch/lispxmp).
However, this package does not seem to be maintenaced.
Therefore, I have rewritten the new version of lispxmp, elispxmp.

## How to use

Insert `; =>` at the end of S-expression, then call `elispxmp`.
Then, the value of the S-expression inserts after `; =>`.

## Example

from: https://rubikitch.hatenadiary.org/entry/20090313/lispxmp

``` sample-before1.el
(+ 3 4) ; =>
(current-buffer) ; =>

(require 'cl-lib)
(cl-loop for i from 1 to 3
      for y = (* i 2) do
      (* i 10) ; =>
      (+ i 1) ; =>
)
```

```sample-before2.el
(setq i 0)
(progn
  1                                     ; =>
  )
i                                       ; =>
(dotimes (x 3)
  i                                     ; =>
  (incf i)
  i                                     ; =>
  )

(+ 1                                    ; =>
   (+ 3
      ;; => 3
      4)
   ;; => 7
   )
;; => 8
```

After `M-x elispxmp`
``` sample-after1.el
(+ 3 4) ; => 7
(current-buffer) ; => #<buffer 13-044055.el>

(require 'cl-lib)
(cl-loop for i from 1 to 3
      for y = (* i 2) do
      (* i 10) ; => 10, 20, 30
      (+ i 1) ; => 2, 3, 4
)
```

``` sample-after2.el
(setq i 0)
(progn
  1                                     ; => 1
  )
i                                       ; => 0
(dotimes (x 3)
  i                                     ; => 0, 1, 2
  (incf i)
  i                                     ; => 1, 2, 3
  )

(+ 1                                    ; => 1
   (+ 3
      ;; => 3
      4)
   ;; => 7
   )
;; => 8
```

## Test

This project has simple tests using [cort.el](https://github.com/conao3/cort.el)
and [cask](https://github.com/cask/caske).
If all the tests pass, the result will look like this:

```
make test
cask exec emacs -Q --batch -l elispxmp-test.el -f cort-test-run

Running 19 tests...
GNU Emacs 29.4 (build 9, x86_64-pc-linux-gnu, GTK+ Version 3.24.41, cairo version 1.18.0)
 of 2025-01-31
[PASSED] elispxmp-test/string-without-properties-1
[PASSED] elispxmp-test/string-without-properties-2
[PASSED] elispxmp-test/string-without-properties-3
[PASSED] elispxmp-test/string-with-properties-1
[PASSED] elispxmp-test/string-with-properties-2
[PASSED] elispxmp-test/string-with-properties-3
[PASSED] elispxmp-test/destructive-annotation-test-1
[PASSED] elispxmp-test/destructive-annotation-test-2
[PASSED] elispxmp-test/destructive-annotation-test-3
[PASSED] elispxmp-test/pp-test-1
[PASSED] elispxmp-test/pp-test-2
[PASSED] elispxmp-test/pp-test-3
[PASSED] elispxmp-test/pp-test-4
[PASSED] elispxmp-test/pp-test-5
[PASSED] elispxmp-test/pp-test-6
[PASSED] elispxmp-test/pp-reexecute-1
[PASSED] elispxmp-test/pp-reexecute-2
[PASSED] elispxmp/cl-lib-1
[PASSED] elispxmp/cl-lib-2
```
