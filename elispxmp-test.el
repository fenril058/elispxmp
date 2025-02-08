;;; elispxmp-test.el --- test code for elispxmp.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ril

;; Author: ril <fenril.nh@gmail.com>
;; Package-Version: 1.0.0
;; Package-Requires: ((cort "7.2.0") (cl-lib "0.5"))
;; Keywords:tools

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

;;

;;; Code:
(require 'elispxmp)
(require 'cort)

(defun elispxmp-to-string (no-properties-p from)
  "Test function for `elispxmp.el'.
It inserts the FROM to the temp-buffer, run elispxmp, gets the
annotation string ang retuns.  If NO-PROPERTIES-P is non-nil,
elispxmp retuns the value with priperties of strings in
annotation,"
  (let ((elispxmp-string-no-properties no-properties-p))
    (with-temp-buffer
      (insert from)
      (elispxmp)
      (buffer-string))))

;;; unit test
(cort-deftest-generate elispxmp-test/string-without-properties :string=
  '(
    ((elispxmp-to-string t "\"a\\nb\" ; => ") "\"a\\nb\" ; => \"a\\nb\"")
    ((elispxmp-to-string t "(propertize \"aaaa\" 'face 'match) ; => ") "(propertize \"aaaa\" 'face 'match) ; => \"aaaa\"")
    ((elispxmp-to-string t "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => ")
     "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => (\"a\" \"b\")")))

(cort-deftest-generate elispxmp-test/string-with-properties :string=
  '(((elispxmp-to-string nil "\"a\\nb\" ; => ") "\"a\\nb\" ; => \"a\\nb\"")
    ((elispxmp-to-string nil "(propertize \"aaaa\" 'face 'match) ; => ") "(propertize \"aaaa\" 'face 'match) ; => #(\"aaaa\" 0 4 (face match))")
    ((elispxmp-to-string nil "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; => ")
     "
 (list (propertize \"a\" 'face 'match)
 (propertize \"b\" 'face 'match)) ; =>\
 (#(\"a\" 0 1 (face match)) #(\"b\" 0 1 (face match)))")))

(cort-deftest-generate elispxmp-test/destructive-annotation-test :string=
  '(((elispxmp-to-string t "
         (setq l (list 1 2)) ; =>
         (setcar l 100)      ; =>
         l                   ; =>
")
     "
         (setq l (list 1 2)) ; => (1 2)
         (setcar l 100)      ; => 100
         l                   ; => (100 2)
")
    ((elispxmp-to-string t "
         (setq s (copy-sequence \"abcd\")) ; =>
         (aset s 0 ?A)                     ; =>
         s                                 ; =>
")
     "
         (setq s (copy-sequence \"abcd\")) ; => \"abcd\"
         (aset s 0 ?A)                     ; => 65
         s                                 ; => \"Abcd\"
")
    ((elispxmp-to-string t "
         (setq c (cons 1 2)) ; =>
         (setcar c 100)      ; =>
         c                   ; =>
")
     "
         (setq c (cons 1 2)) ; => (1 . 2)
         (setcar c 100)      ; => 100
         c                   ; => (100 . 2)
")))
(cort-deftest-generate elispxmp-test/pp-test :string=
  '(((elispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
")
    ((elispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;; => ((\"a\")
;;     \"b\"
;;     (\"c\"))
" )
    ((elispxmp-to-string nil "'((\"a\") \"b\" (\"c\"))
;;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
")
    ((elispxmp-to-string nil "'((\"a\") \"b\" (\"c\"))
;; =>
")
     "'((\"a\") \"b\" (\"c\"))
;; => ((\"a\")
;;     \"b\"
;;     (\"c\"))
"
     )
    ((elispxmp-to-string t "'a
;;; =>
")
     "'a
;;; => a
"
     )
    ((elispxmp-to-string t "'(\"a\")
;;; =>
")
     "'(\"a\")
;;; => (\"a\")
")))

(cort-deftest-generate elispxmp-test/pp-reexecute :string=
  '(((elispxmp-to-string t "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
")
     "'((\"a\") \"b\" (\"c\"))
;;; => ((\"a\")
;;;     \"b\"
;;;     (\"c\"))
"
     )
    ((elispxmp-to-string t "1
;;; => 1
;;; 2
;;;    3
")
     "1
;;; => 1
;;; 2
;;;    3
" )))

(cort-deftest-generate elispxmp/cl-lib :string=
  '(((elispxmp-to-string nil "
(require 'cl-lib)
(cl-loop for i from 1 to 3
      for y = (* i 2) do
      (* i 10) ; =>
      (+ i 1) ; =>
)")
     "
(require 'cl-lib)
(cl-loop for i from 1 to 3
      for y = (* i 2) do
      (* i 10) ; => 10, 20, 30
      (+ i 1) ; => 2, 3, 4
)"
     )
    ((elispxmp-to-string nil "
(require 'cl-lib)
(setq i 0)
(progn
  1                                     ; =>
  )
i                                       ; =>
(dotimes (x 3)
  i                                     ; =>
  (cl-incf i)
  i                                     ; => 1, 2, 3
  )

(+ 1                                    ; =>
   (+ 3
      ;; => 3
      4)
   ;; => 7
   )
;; => 8")
     "
(require 'cl-lib)
(setq i 0)
(progn
  1                                     ; => 1
  )
i                                       ; => 0
(dotimes (x 3)
  i                                     ; => 0, 1, 2
  (cl-incf i)
  i                                     ; => 1, 2, 3
  )

(+ 1                                    ; => 1
   (+ 3
      ;; => 3
      4)
   ;; => 7
   )
;; => 8"
     )))

(provide 'elispxmp-test)
;;; elispxmp-test.el ends here
