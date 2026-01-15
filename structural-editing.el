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

(defun structural-editing-slurp-backward ()
    "Pull the previous sexp into the current list.

When the cursor is inside a list, slurp the preceding sexp into that list.
Example: (a (|b) c) → ((a b) c)"
    (interactive)
    (save-excursion
        (condition-case nil
                (progn
                    ;; Find the outer list and the element before the inner list
                    (backward-up-list)
                    (let ((outer-open (point)))
                        (forward-sexp)  ; Skip to after the inner list
                        (backward-sexp) ; Back to the inner list  
                        (backward-char) ; To space before inner list
                        (let ((space-end (point)))
                            (backward-sexp) ; Back to start of preceding element
                            (let ((text-to-slurp (delete-and-extract-region (point) space-end)))
                                (goto-char outer-open)
                                (forward-char)
                                (insert text-to-slurp)))))
            (scan-error
             (message "Cannot slurp backward")))))

(provide 'structural-editing)
;;; structural-editing.el ends here
