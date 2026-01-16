;;; structural-editing.el --- Structural editing commands using built-in Emacs functions -*- lexical-binding: t -*-

;;; Commentary:
;; Structural editing commands (splice, slurp, barf) implemented
;; using only built-in Emacs list and s-expression movement commands.

;;; Code:

(defun structural-editing-splice ()
    "Remove parentheses around the current sexp, merging it with its parent."
    (interactive)
    (save-excursion
        (condition-case err
                (progn
                    (backward-up-list)
                    (let ((start (point)))
                        (forward-sexp)
                        (delete-char -1)
                        (goto-char start)
                        (delete-char 1)))
            (scan-error (error "Cannot splice: not in a list")))))

(defun structural-editing-slurp-forward ()
    "Pull the next sexp into the current list."
    (interactive)
    (condition-case err
        (let ((sibling nil))
            (save-excursion
                (catch 'found
                    (while t
                        (backward-up-list)
                        (forward-list)
                        (let ((test-pos (point)))
                            (condition-case nil
                                (progn
                                    (forward-sexp)
                                    (setq sibling (delete-and-extract-region test-pos (point)))
                                    (throw 'found nil))
                                (scan-error nil))))
                    (error "Cannot slurp forward: no next sexp in any containing list")))
            (backward-up-list)
            (forward-list)
            (backward-char)
            (when (eq (char-before) ?\()
                (when (and (> (length sibling) 0)
                           (eq (string-to-char sibling) ?\ ))
                    (setq sibling (substring sibling 1))))
            (insert sibling))
        (scan-error (error "Cannot slurp forward: no next sexp in any containing list"))))

(defun structural-editing-slurp-backward ()
    "STUB: Pull the previous sexp into the current list."
    (interactive)
    nil)

(defun structural-editing-barf-forward ()
    "STUB: Move the last element out of the current list."
    (interactive)
    nil)

(defun structural-editing-barf-backward ()
    "STUB: Move the first element out of the current list."
    (interactive)
    nil)

(provide 'structural-editing)
;;; structural-editing.el ends here
