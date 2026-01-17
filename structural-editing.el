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
    "Pull the previous sexp into the current list."
    (interactive)
    (condition-case err
            (let ((sibling nil))
                (save-excursion
                    (catch 'found
                        (while t
                            (backward-up-list)
                            (let ((test-end (point)))
                                (condition-case nil
                                        (progn
                                            (backward-sexp)
                                            (setq sibling (delete-and-extract-region (point) test-end))
                                            (throw 'found nil))
                                    (scan-error nil))))
                        (error "Cannot slurp backward: no previous sexp in any containing list")))
                (backward-up-list)
                (forward-char)
                (when (eq (char-after) ?\))
                    (when (and (> (length sibling) 0)
                               (eq (string-to-char (substring sibling -1)) ?\ ))
                        (setq sibling (substring sibling 0 -1))))
                (insert sibling))
        (scan-error (error "Cannot slurp backward: no previous sexp in any containing list"))))

(defun structural-editing-barf-forward ()
    "Move the last element out of the current list."
    (interactive)
    (save-excursion
        (condition-case err
                (progn
                    (backward-up-list)
                    (forward-list)
                    (backward-char)
                    (let ((last-element-end (point))
                          (last-element nil))
                        (backward-sexp)
                        (let ((previous-char (char-syntax (char-before))))
                            (setq last-element (delete-and-extract-region (point) last-element-end))
                            (when (not (eq previous-char ?\())
                                (delete-char -1)
                                (forward-char))
                            (forward-char)
                            (let ((char-after-insert (char-after))
                                  (char-before-insert (char-before)))
                                (when (not (eq char-before-insert ?\ ))
                                    (insert " "))
                                (insert last-element)
                                (when (not (eq char-after-insert ?\ ))
                                    (insert " "))))))
            (scan-error (error "Cannot barf forward: no elements to barf")))))

(defun structural-editing-barf-backward ()
    "Move the first element out of the current list."
    (interactive)
    (save-excursion
        (condition-case err
                (progn
                    (backward-up-list)
                    (delete-char 1)
                    (forward-sexp)
                    (when (eq (char-after) ?\ )
                        (delete-char 1))
                    (insert " ("))
            (scan-error (error "Cannot barf backward: no elements to barf")))))

(provide 'structural-editing)
;;; structural-editing.el ends here
