;;; treesit-sexp.el --- Tree-sitter based s-expression navigation -*- lexical-binding: t -*-

;; SPDX-License-Identifier: 0BSD
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; Author: Alexis Purslane
;; Version: 0.1.0
;; Keywords: convenience, lisp, tools
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides tree-sitter based s-expression movement functions
;; that respect syntax structure.  It moves by semantic units rather than
;; character positions, skipping over punctuation like commas and semicolons
;; while honoring delimiter boundaries.

;; The main functions are:
;;   `treesit-sexp-forward' - move forward by s-expressions
;;   `treesit-sexp-backward' - move backward by s-expressions

;;; Code:

(defun treesit-sexp--find-inner-node (node pos)
    "Find the innermost node that contains POS by walking up from NODE."
    (let ((n node))
        (while (and n
                    (not (and (< (treesit-node-start n) pos)
                              (> (treesit-node-end n) pos))))
            (setq n (treesit-node-parent n)))
        n))

(defun treesit-sexp--walk-parents (curr direction inner boundary)
    "Walk up parent chain from CURR based on DIRECTION and BOUNDARY."
    (let ((compare-pos (if (eq direction 'forward) '>= '<)))
        (while (let ((p (treesit-node-parent curr)))
                   (and p (not (treesit-node-eq p inner))
                        (funcall compare-pos
                                 (if (eq direction 'forward)
                                         (treesit-node-start p)
                                     (treesit-node-end p))
                                 boundary)))
            (setq curr (treesit-node-parent curr)))
        (if (eq direction 'forward)
                (treesit-node-end curr)
            (treesit-node-start curr))))

(defun treesit-sexp--find-in-inner (inner direction pos punctuation forward-walls backward-walls)
    "Handle sexp finding when in an inner node."
    (let ((target-sexp nil)
          (i (if (eq direction 'forward) 0 (1- (treesit-node-child-count inner))))
          (step (if (eq direction 'forward) '1+ '1-))
          (compare-start (if (eq direction 'forward) '>= '<))
          (compare-pos (if (eq direction 'forward) '>= '<))
          (wall-list (if (eq direction 'forward) forward-walls backward-walls)))
        (while (and (if (eq direction 'forward)
                            (< i (treesit-node-child-count inner))
                        (>= i 0))
                    (not target-sexp))
            (let ((c (treesit-node-child inner i)))
                (when (funcall compare-start (treesit-node-start c) pos)
                    (let ((type (treesit-node-type c))
                          (named (treesit-node-check c 'named)))
                        (cond
                         ((and (not named) (member type wall-list))
                          (throw 'hit-wall nil))
                         ((and (not named) (member type punctuation))
                          nil)
                         (t (setq target-sexp c)))))
                (setq i (funcall step i))))

        (cond
         (target-sexp
          (treesit-sexp--walk-parents
           target-sexp direction inner
           (if (eq direction 'forward)
                   (treesit-node-start target-sexp)
               (treesit-node-end target-sexp))))
         (t (if (eq direction 'forward)
                    (treesit-node-end inner)
                (treesit-node-start inner))))))

(defun treesit-sexp--find-fallback (node direction pos punctuation forward-walls backward-walls)
    "Find sexp when not inside an inner node.
Searches siblings and then parent tree for valid sexp at/after POS."
    (let ((compare-pos (if (eq direction 'forward) '>= '<))
          (compare-start (if (eq direction 'forward) '< '>))
          (next-sibling-func (if (eq direction 'forward)
                                     'treesit-node-next-sibling
                                 'treesit-node-prev-sibling)))
        (let ((n node)
              target)
            (while (and n
                        (or (funcall compare-start (treesit-node-start n) pos)
                            (and (not (treesit-node-check n 'named))
                                 (member (treesit-node-type n) punctuation))))
                (setq n (or (funcall next-sibling-func n) (treesit-node-parent n))))

            (if (and n (funcall compare-pos (treesit-node-start n) pos))
                    (treesit-sexp--walk-parents
                     n direction nil
                     (if (eq direction 'forward)
                             (treesit-node-start n)
                         (treesit-node-end n)))
                (if (eq direction 'forward)
                        (point-max)
                    (point-min))))))

(defun treesit-sexp-forward (&optional arg)
    "Move forward ARG s-expressions, skipping punctuation like , . : ;.
Stops at `Wall' delimiters ( ) { } [ ] without signaling an error."
    (interactive "p")
    (let ((count (abs (or arg 1)))
          (direction (if (< (or arg 0) 0) 'backward 'forward))
          (punctuation ' ("," "." ";" "->"))
          (forward-walls ' ("}" ")" "]" ">"))
          (backward-walls ' ("(" "{" "[" "<")))
        (catch 'hit-wall
            (dotimes (_ count)
                (let* ((pos (point))
                       (lookup-pos (if (eq direction 'backward) (1- pos) pos))
                       (node (treesit-node-at lookup-pos))
                       (inner (treesit-sexp--find-inner-node node pos))
                       (target (cond
                                (inner (treesit-sexp--find-in-inner inner direction pos punctuation forward-walls backward-walls))
                                (t (treesit-sexp--find-fallback node direction pos punctuation forward-walls backward-walls)))))

                    (when target
                        (if (eq direction 'forward)
                                (when (> target (point))
                                    (goto-char (min target (point-max))))
                            (when (< target (point))
                                (goto-char (max target (point-min)))))))))))

(defun treesit-sexp-backward (&optional arg)
    "Move backward ARG s-expressions, skipping punctuation like , . : ;.
Stops at `Wall' delimiters ( { [ < without signaling an error."
    (interactive "p")
    (treesit-sexp-forward (- (or arg 1))))

(defun treesit-sexp-up-backward (&optional arg)
    "Move backward out of ARG levels of s-expressions.
If within a s-expression, move to its beginning first.
Then move ARG times upward through nesting levels."
    (interactive "p")
    (let ((count (abs (or arg 1))))
        (dotimes (_ count)
            (let* ((pos (point))
                   (node (treesit-node-at pos))
                   (inner (treesit-sexp--find-inner-node node pos)))
                (if inner
                        (if (= pos (treesit-node-start inner))
                                (when-let ((parent (treesit-node-parent inner)))
                                    (goto-char (treesit-node-start parent)))
                            (goto-char (treesit-node-start inner)))
                    (when-let ((parent (treesit-node-parent node)))
                        (goto-char (treesit-node-start parent))))))))

(provide 'treesit-sexp)
;;; treesit-sexp.el ends here
