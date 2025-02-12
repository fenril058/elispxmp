;;; elispxmp.el --- Automatic annotation of Emacs Lisp code with evaluation results -*- lexical-binding: t; -*-

;; Copyright (C) 2025 ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: lisp, convenience
;; Package-Version: 1.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/fenril058/elispxmp

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

;; It provides automatic annotation of Emacs Lisp code with evaluation
;; results.  The main function `elispxmp' processes the selected
;; region or the entire buffer, evaluates the expressions, and
;; annotates the code with the results.  It also includes a
;; customization for `comment-dwim' to add a " =>" mark when called
;; successively.  This is useful for quickly annotating code with
;; expected results or comments.

;; This package is an Emacs Lisp version of Ruby's xmpfilter,
;; originally created by rubikitch <rubikitch@ruby-lang.org>
;; the original version is here: https://www.emacswiki.org/emacs/LispXmp

;; For more information, visit:
;; https://github.com/fenril058/elispxmp/README.md

;;; Code:

(require 'pp)

(defgroup elispxmp nil
  "Automagic Emacs Lisp code annotation."
  :group 'emacs)

;;;###autoload
(defcustom elispxmp-string-no-properties t
  "If non-nil, remove text properties from evaluation results."
  :type 'boolean
  :group 'elispxmp)

;;;###autoload
(defcustom elispxmp-comment-dwim-enable-modes (list 'emacs-lisp-mode 'lisp-mode 'scheme-mode)
  "List of major modes where lispxmp-hack-comment-dwim is enabled."
  :type 'list
  :group 'lispxmp)

;;;###autoload
(defcustom elispxmp-debug nil
  "If non-nil, do not kill `elispxmp-temp-buffer'."
  :type 'boolean
  :group 'elispxmp)

(defvar elispxmp-results-alist nil
  "Association list of results from evaluating Lisp expressions.
Each element is a cons cell where the car is an index (integer)
and the cdr is a cons cell of the form (RESULT . USE-PP). RESULT
is the evaluation result of the Lisp expression, converted to a
string, and USE-PP is a boolean indicating whether
pretty-printing was used.

Example:
  ((0 . (\"result0\" . use-pp))
   (1 . (\"result1\" . use-pp))
   (2 . (\"result2\" . use-pp))
   ...)")

(defvar elispxmp-temp-buffer " *elispxmp tmp*"
  "Temporary buffer used for evaluating Lisp expressions.")

;;; for scheme

;;;###autoload
(defcustom elispxmp-scheme-command "racket"
  "The command to evaluate Scheme scripts.")

(defvar elispxmp-header--scheme "(define lispxmp-result-alist '())

(define nil '())

(define (%elispxmp-store-result use-pp index result)
  (define (push! item)
    (set! lispxmp-result-alist (cons item lispxmp-result-alist)))
  (define (prin1-to-string use-pp obj)
    (let ((output (open-output-string)))
      (write obj output)
      (cons (get-output-string output) use-pp)))
  (push! (cons index (prin1-to-string use-pp result)))
  result)

(define (get-lispxmp-result-alist)
  lispxmp-result-alist)
")

(defvar elispxmp-footer--scheme "
(get-lispxmp-result-alist)")

;;;###autoload
(defun elispxmp ()
  "Evaluate Lisp expressions and insert results into the buffer.
Emacs-lisp-mode and scheme-mode are supported."
  (interactive)
  (save-excursion
    (save-restriction
      (when (region-active-p)
        (narrow-to-region (region-beginning) (region-end)))
      (let ((mode major-mode)
            (buf (current-buffer)))
        (elispxmp-evaluate-with-temp-buffer buf mode))
      (elispxmp-insert-annotations))))

(defun elispxmp-evaluate-with-temp-buffer (buf mode)
  "Create a temporary buffer from BUF, replace markers, and evaluate the buffer.
BUF is the original buffer containing Lisp expressions.
MODE is the major mode to be used for evaluation."
  (with-current-buffer (get-buffer-create elispxmp-temp-buffer)
    (unwind-protect
        (progn
          (let ((intern (concat (symbol-name mode) "-hook"))
                after-change-major-mode-hook)
            (funcall mode))
          (erase-buffer)
          (insert-buffer-substring buf)
          (goto-char (point-min))
          (elispxmp-process-markers-and-wrap mode)
          (setq elispxmp-results-alist nil)
          (elispxmp-eval-buffer buf mode))
      (unless elispxmp-debug
        (kill-buffer elispxmp-temp-buffer)))))

(defun elispxmp-remove-pp-annotations ()
  "Remove pretty-print annotations in the buffer.

This function searches for lines that match the pattern of multiple semicolons
followed by ' => ' at the beginning of the line. It then deletes any subsequent
lines that match the same pattern, effectively cleaning up the annotations.

The function preserves the current point and mark using `save-excursion`."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(;+ *\\)\\(=> \\)" nil t)
      (let ((next-line-re (concat
                           "^"
                           (regexp-quote
                            (concat (match-string 1)
                                    (make-string (- (match-end 2) (match-beginning 2))
                                                 ?\s)))
                           ".+\n")))
        (forward-line 1)
        (while (looking-at next-line-re)
          (delete-region (point) (progn (forward-line 1) (point))))))))

(defun elispxmp-process-markers-and-wrap (mode)
  "Process markers and wrap S-expressions in the buffer.

This function searches for lines that match the pattern of multiple semicolons
followed by ' => ' and wraps the preceding S-expression with a result marker."
  (save-excursion
    (goto-char (point-min))
    (elispxmp-insert-header mode)
    (let ((index 0))
      (while (re-search-forward "\\(;+ *\\)=>.*$" nil t)
        (let ((use-pp (eq (line-beginning-position) (match-beginning 0))))
          (when (elispxmp-annotation-p)
            (goto-char (match-beginning 0))
            (elispxmp-wrap-expressions use-pp index)
            (replace-match (format "%s" (match-string 1)))
            (setq index (1+ index))))))
    (goto-char (point-max))
    (elispxmp-insert-footer mode)))

(defun elispxmp-insert-header (mode)
  (cond ((eq mode 'scheme-mode)
         (insert elispxmp-header--scheme))
        (t
         nil)))

(defun elispxmp-insert-footer (mode)
  (cond ((eq mode 'scheme-mode)
         (insert elispxmp-footer--scheme))
        (t
         nil)))

(defun elispxmp-annotation-p ()
  "Check if the current line contains an annotation."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (ignore-errors (comment-search-forward (line-end-position) t))
      (looking-at "=>"))))

(defun elispxmp-wrap-expressions (use-pp index)
  "Wrap preceding S-expressions with 'elispxmp-store-result'.
USE-PP indicates whether pretty-printing should be used.
INDEX is the result marker index."
  (save-match-data
    (let ((e (make-marker)))
      (set-marker e (point))
      (forward-sexp -1)
      (insert (format "(%%elispxmp-store-result %s %d " use-pp index))
      (goto-char e)
      (insert ")"))))

(defun elispxmp-eval-buffer (buf mode)
  "Evaluate the temporary buffer and handle errors.
If an error occurs during evaluation, undo the changes in BUF and
display an error message.  This function supports Emacs Lisp and
Scheme modes."
  (cond ((provided-mode-derived-p mode 'emacs-lisp-mode)
         (elispxmp-eval-buffer--elisp buf))
        ((eq mode 'scheme-mode)
         (elispxmp-eval-buffer--scheme buf))
        (t
         (error "%s is not supported." mode))))

(defun elispxmp-eval-buffer--elisp (buf)
  "Evaluate the temporary buffer and handle errors.
If an error occurs during evaluation, undo the changes in BUF and
display an error message."
  (condition-case err
      (eval-buffer)
    (error
     (message "Error during evaluation: %s" (error-message-string err)))))

(defun elispxmp-eval-buffer--scheme (buf)
  "Evaluate the temporary buffer and handle errors.
If an error occurs during evaluation, undo the changes in BUF and
display an error message."
  (let ((result (shell-command-to-string (concat elispxmp-scheme-command " -e "
                                                 (shell-quote-argument
                                                  (buffer-substring-no-properties (point-min) (point-max)))))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert result)
      (goto-char (point-max))
      (forward-sexp -1)
      (insert "(setq elispxmp-results-alist ")
      (forward-sexp)
      (insert ")")
      (forward-sexp -1)
      (eval-region (point) (point-max)))))

(defun elispxmp-insert-annotations ()
  "Insert evaluation results back into the original buffer."
  (goto-char (point-min))
  (elispxmp-remove-pp-annotations)
  (let ((index 0))
    (while (re-search-forward "\\(;+ *\\)=>.*$" nil t)
      (let* ((result (mapconcat 'cadr
                                (remove nil
                                        (mapcar (lambda (pair) (when (= index (car pair)) pair))
                                                (reverse elispxmp-results-alist)))
                                ", "))
             (use-pp (cdr (cdr (assoc index elispxmp-results-alist))))
             (formatted-result (if use-pp
                                   (replace-regexp-in-string "\n"
                                                             (concat "\n" (match-string 1) "   ")
                                                             result)
                                 result)))
        (replace-match (format "%s=> %s" (match-string 1) formatted-result)))
      (setq index (1+ index)))))

;;; Using in `elispxmp-temp-buffer'
(defun %elispxmp-store-result (use-pp index result)
  "Store the evaluation RESULT at the given INDEX in `elispxmp-results-alist`."
  (push (cons index (%elispxmp-prin1-to-string use-pp result)) elispxmp-results-alist)
  result)

(defun %elispxmp-prin1-to-string (use-pp result)
  "Convert RESULT to a string, optionally using pretty-printing.
USE-PP indicates whether pretty-printing should be used."
  (let* ((cleaned-result (elispxmp-remove-properties-recursively result))
         (print-func (if use-pp 'pp-to-string 'prin1-to-string))
         (printed-result (funcall print-func cleaned-result)))
    ;; delete last \n in result from `pp-to-string'
    (when (and use-pp (string-suffix-p "\n" printed-result))
      (setq printed-result (substring printed-result 0 -1)))
    (cons printed-result use-pp)))

(defun elispxmp-remove-properties-recursively (obj)
  "Recursively remove text properties from strings in OBJ and replace newlines with '\\\\n'."
  (cond
   ((stringp obj) (replace-regexp-in-string "\n" "\\\\n" (if elispxmp-string-no-properties
                                                             (substring-no-properties obj)
                                                           obj)))
   ((consp obj) (cons (elispxmp-remove-properties-recursively (car obj))
                      (elispxmp-remove-properties-recursively (cdr obj))))
   ((listp obj) (mapcar #'elispxmp-remove-properties-recursively obj))
   (t obj)))

;;;###autoload
(defun elispxmp-hack-comment-dwim (orig-fun &rest args)
  "If comment-dwim is successively called, add => mark."
  (if (and (apply 'derived-mode-p elispxmp-comment-dwim-enable-modes)
           (eq last-command this-command)
           (not (member "=>" (list (ignore-errors (buffer-substring (- (point) 2) (point)))
                                   (ignore-errors (buffer-substring (point) (+ (point) 2)))))))
      (insert " =>"))
  (apply orig-fun args))

;;;###autoload
(advice-add 'comment-dwim :around #'elispxmp-hack-comment-dwim)

(provide 'elispxmp)
;;; elispxmp.el ends here
