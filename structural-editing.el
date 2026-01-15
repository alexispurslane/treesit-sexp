;;; structural-editing.el --- Structural editing commands using built-in Emacs functions -*- lexical-binding: t -*-

;;; Commentary:
;; Structural editing commands (splice, slurp, barf) implemented
;; using only built-in Emacs list and s-expression movement commands.

;;; Code:

(defun structural-editing-splice ()
  "Remove parentheses around the current sexp.
Merges the current sexp with its parent sexp."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn
          (backward-up-list)
          (let ((start (point)))
            (forward-sexp)
            (delete-char -1)
            (goto-char start)
            (delete-char 1)))
      (scan-error
       (message "Not inside a list")))))

(defun structural-editing-slurp-forward ()
  "Pull the next sexp into the current list."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn
          ;; Get to the inner list opening paren
          (backward-up-list)
          (let ((inner-close (progn
                               (forward-list)  ; Go to matching )
                               (point))))
            ;; Now extract the next sexp after this list
            (let ((next-start (point)))
              (forward-sexp)
              (let ((next-sexp (delete-and-extract-region next-start (point))))
                (goto-char inner-close)
                (backward-char)
                (insert next-sexp)))))
      (scan-error
       (message "Cannot slurp forward")))))

(provide 'structural-editing)
;;; structural-editing.el ends here