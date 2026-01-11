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

(defun treesit-sexp--treesit-available-p ()
    "Check if tree-sitter is available and the current buffer has a parser."
    (and (fboundp 'treesit-available-p)
         (treesit-available-p)
         (treesit-parser-list)))

(defun treesit-sexp-forward (&optional arg interactive)
    "Move forward ARG s-expressions, skipping punctuation like , . : ;.
Stops at `Wall' delimiters ( ) { } [ ] without signaling an error.
When TreeSitter is not available, falls back to `forward-sexp'."
    (interactive "p")
    (if (not (treesit-sexp--treesit-available-p))
            (forward-sexp arg interactive)
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
                                    (goto-char (max target (point-min))))))))))))

(defun treesit-sexp-backward (&optional arg interactive)
    "Move backward ARG s-expressions, skipping punctuation like , . : ;.
Stops at `Wall' delimiters ( { [ < without signaling an error.
When TreeSitter is not available, falls back to `backward-sexp'."
    (interactive "p")
    (if (not (treesit-sexp--treesit-available-p))
            (backward-sexp arg interactive)
        (treesit-sexp-forward (- (or arg 1)) interactive)))

(defun treesit-sexp-up-list (&optional arg escape-strings no-syntax-crossing)
    "Move forward out of ARG levels of parentheses.
When TreeSitter is not available or when called interactively,
falls back to `up-list'.

With negative ARG, move backward but still to a shallower spot.
If ESCAPE-STRINGS is non-nil, move out of enclosing strings as well.
If NO-SYNTAX-CROSSING is non-nil, prefer to break out of any enclosing
string instead of moving to the end of a list broken across multiple strings."
    (interactive "p")
    (if (or (not (treesit-sexp--treesit-available-p)) (called-interactively-p 'any))
            (up-list arg escape-strings no-syntax-crossing)
        (let ((count (abs (or arg 1)))
              (direction (if (< (or arg 0) 0) 'backward 'forward)))
            (dotimes (_ count)
                (let* ((pos (point))
                       (node (treesit-node-at pos)))
                    (if (eq direction 'forward)
                            (if-let ((parent (treesit-node-parent node)))
                                    (goto-char (treesit-node-end parent))
                                (goto-char (point-max)))
                        (let ((inner (treesit-sexp--find-inner-node node pos)))
                            (if inner
                                    (if (= pos (treesit-node-start inner))
                                            (when-let ((parent (treesit-node-parent inner)))
                                                (goto-char (treesit-node-start parent)))
                                        (goto-char (treesit-node-start inner)))
                                (when-let ((parent (treesit-node-parent node)))
                                    (goto-char (treesit-node-start parent)))))))))))

(defun treesit-sexp-forward-list (&optional arg interactive)
    "Move forward across one balanced group of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do it that many times.
Negative arg -N means move backward across N groups of parentheses.
When TreeSitter is not available, falls back to `forward-list'."
    (interactive "^p\nd")
    (if (or (not (treesit-sexp--treesit-available-p)) interactive)
            (forward-list arg interactive)
        (or arg (setq arg 1))
        (let ((count (abs arg))
              (direction (if (< arg 0) 'backward 'forward))
              (wall-list '("(" "{" "[" "<" ")" "}" "]" ">")))
            (dotimes (_ count)
                (let* ((pos (point))
                       (node (treesit-node-at pos))
                       (inner (treesit-sexp--find-inner-node node pos))
                       (target (or inner node))
                       (sibling-func (if (eq direction 'forward) 'treesit-node-next-sibling 'treesit-node-prev-sibling))
                       (next target)
                       (found nil))
                    (while (and next (not found))
                        (let ((next-sibling (funcall sibling-func next)))
                            (if next-sibling
                                    (progn
                                        (setq next next-sibling)
                                        (when (and (member (treesit-node-type next) wall-list)
                                                   (if (eq direction 'forward)
                                                           (> (treesit-node-start next) pos)
                                                       (< (treesit-node-end next) pos)))
                                            (setq found next)))
                                (setq next nil))))
                    (when found
                        (goto-char (if (eq direction 'forward)
                                           (treesit-node-end found)
                                       (treesit-node-start found)))))))))

(defun treesit-sexp-backward-list (&optional arg interactive)
    "Move backward across one balanced group of parentheses.
This command will also work on other parentheses-like expressions
defined by the current language mode.
With ARG, do it that many times.
Negative arg -N means move forward across N groups of parentheses.
When TreeSitter is not available, falls back to `backward-list'."
    (interactive "^p\nd")
    (or arg (setq arg 1))
    (if (or (not (treesit-sexp--treesit-available-p)) interactive)
            (backward-list arg interactive)
        (treesit-sexp-forward-list (- arg) interactive)))

(defun treesit-sexp-down-list (&optional arg interactive)
    "Move forward down one level of parentheses.
Find the next sibling node with children and position inside it."
    (interactive "^p\nd")
    (if (or (not (treesit-sexp--treesit-available-p)) interactive)
            (down-list arg interactive)
        (or arg (setq arg 1))
        (let ((direction (if (> arg 0) 'forward 'backward))
              (count (abs arg)))
            (dotimes (_ count)
                (let* ((node (treesit-node-at (point)))
                       (target-node
                        (catch 'found
                            (let ((curr (if (eq direction 'forward)
                                                (treesit-node-next-sibling node)
                                            (treesit-node-prev-sibling node))))
                                (while curr
                                    (when (> (treesit-node-child-count curr) 0)
                                        (throw 'found curr))
                                    (setq curr (if (eq direction 'forward)
                                                       (treesit-node-next-sibling curr)
                                                   (treesit-node-prev-sibling curr))))))))
                    (when target-node
                        (goto-char (if (eq direction 'forward)
                                           (1+ (treesit-node-start target-node))
                                       (1- (treesit-node-end target-node))))))))))

(defun treesit-sexp-mark-sexp (&optional arg allow-extend)
    "Set mark ARG sexps from point or move mark one sexp.
When called from Lisp with ALLOW-EXTEND omitted or nil, mark is
set ARG sexps from point.
With ARG and ALLOW-EXTEND both non-nil (interactively, with prefix
argument), the place to which mark goes is the same place `treesit-sexp-forward'
would move to with the same argument; if the mark is active, it moves
ARG sexps from its current position, otherwise it is set ARG sexps
from point.
When invoked interactively without a prefix argument and no active
region, mark moves one sexp forward.
When invoked interactively without a prefix argument, and region
is active, mark moves one sexp away of point (i.e., forward
if mark is at or after point, back if mark is before point), thus
extending the region by one sexp.  Since the direction of region
extension depends on the relative position of mark and point, you
can change the direction by `exchange-point-and-mark'.
When TreeSitter is not available, falls back to `mark-sexp'."
    (interactive "P\np")
    (if (not (treesit-sexp--treesit-available-p))
            (mark-sexp arg allow-extend)
        (cond ((and allow-extend
                    (or (and (eq last-command this-command) (mark t))
                        (and transient-mark-mode mark-active)))
               (setq arg (if arg (prefix-numeric-value arg)
                             (if (< (mark) (point)) -1 1)))
               (set-mark
                (save-excursion
                    (goto-char (mark))
                    (condition-case error
                            (treesit-sexp-forward arg)
                        ((scan-error)
                         (user-error (if (equal (cadr error)
                                                "Containing expression ends prematurely")
                                             "No more sexp to select"
                                         (cadr error)))))
                    (point))))
              (t
               (push-mark
                (save-excursion
                    (condition-case error
                            (treesit-sexp-forward (prefix-numeric-value arg))
                        ((scan-error)
                         (user-error (if (equal (cadr error)
                                                "Containing expression ends prematurely")
                                             "No sexp to select"
                                         (cadr error)))))
                    (point))
                nil t)))))

(defun treesit-sexp-beginning-of-defun (&optional arg)
    "Move backward to the beginning of a defun.
With ARG, do it that many times.  Negative ARG means move forward
to the ARGth following beginning of defun.
When TreeSitter is not available or cannot find function boundaries,
falls back to `beginning-of-defun'."
    (interactive "^p")
    (if (or (not (treesit-sexp--treesit-available-p)) (called-interactively-p 'any))
            (beginning-of-defun arg)
        (or arg (setq arg 1))
        (let ((count (abs arg))
              (direction (if (< arg 0) 'forward 'backward))
              (defun-types '("function_definition" "function_declaration" "function_item" "method_definition" "class_declaration" "class_definition" "impl_item" "interface_declaration" "type_declaration")))
            (dotimes (_ count)
                (let* ((pos (point))
                       (node (treesit-node-at pos))
                       (found nil))
                    (if (eq direction 'backward)
                            (while (and node (not found))
                                (let ((parent (treesit-node-parent node)))
                                    (if parent
                                            (if (member (treesit-node-type parent) defun-types)
                                                    (setq found parent)
                                                (setq node parent))
                                        (setq node nil))))
                        (while (and node (not found))
                            (let ((next-sibling (treesit-node-next-sibling node)))
                                (if next-sibling
                                        (if (member (treesit-node-type next-sibling) defun-types)
                                                (setq found next-sibling)
                                            (setq node next-sibling))
                                    (setq node nil)))))
                    (when found
                        (goto-char (treesit-node-start found))
                        (beginning-of-line)))))))

(defun treesit-sexp-end-of-defun (&optional arg interactive)
    "Move forward to next end of defun.
With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.
When TreeSitter is not available or cannot find function boundaries,
falls back to `end-of-defun'."
    (interactive "^p\nd")
    (if (or (not (treesit-sexp--treesit-available-p)) interactive (called-interactively-p 'any))
            (end-of-defun arg interactive)
        (or (not (eq this-command 'treesit-sexp-end-of-defun))
            (eq last-command 'treesit-sexp-end-of-defun)
            (and transient-mark-mode mark-active)
            (push-mark))
        (if (or (null arg) (= arg 0)) (setq arg 1))
        (let ((count (abs arg))
              (direction (if (< arg 0) 'backward 'forward))
              (defun-types '("function_definition" "function_declaration" "function_item" "method_definition" "class_declaration" "class_definition" "impl_item" "interface_declaration" "type_declaration")))
            (dotimes (_ count)
                (let* ((pos (point))
                       (node (treesit-node-at pos))
                       (found nil))
                    (if (eq direction 'forward)
                            (while (and node (not found))
                                (let ((parent (treesit-node-parent node)))
                                    (if parent
                                            (if (member (treesit-node-type parent) defun-types)
                                                    (setq found parent)
                                                (setq node parent))
                                        (setq node nil))))
                        (while (and node (not found))
                            (let ((prev-sibling (treesit-node-prev-sibling node)))
                                (if prev-sibling
                                        (if (member (treesit-node-type prev-sibling) defun-types)
                                                (setq found prev-sibling)
                                            (setq node prev-sibling))
                                    (setq node nil)))))
                    (when found
                        (goto-char (treesit-node-end found))
                        (skip-syntax-forward " ")
                        (when (eq (char-after) ?\n) (forward-char 1))))))))

;;;###autoload
(define-minor-mode treesit-sexp-mode
    "Minor mode for tree-sitter based sexp navigation.
When enabled, sexp navigation functions use tree-sitter for better
accuracy when tree-sitter parsers are available."
    :global nil
    :require 'treesit-sexp
    :group 'treesit-sexp
    (if treesit-sexp-mode
            (progn
                (setq-local forward-sexp-function 'treesit-sexp-forward)
                (setq-local forward-list-function 'treesit-sexp-forward-list)
                (setq-local backward-list-function 'treesit-sexp-backward-list)
                (setq-local up-list-function 'treesit-sexp-up-list)
                (setq-local down-list-function 'treesit-sexp-down-list)
                (setq-local mark-sexp-function 'treesit-sexp-mark-sexp)
                (setq-local beginning-of-defun-function 'treesit-sexp-beginning-of-defun)
                (setq-local end-of-defun-function 'treesit-sexp-end-of-defun))
        (kill-local-variable 'forward-sexp-function)
        (kill-local-variable 'forward-list-function)
        (kill-local-variable 'backward-list-function)
        (kill-local-variable 'up-list-function)
        (kill-local-variable 'down-list-function)
        (kill-local-variable 'mark-sexp-function)
        (kill-local-variable 'beginning-of-defun-function)
        (kill-local-variable 'end-of-defun-function)))

;;;###autoload
(defun treesit-sexp-global-mode-setup ()
    "Setup treesit-sexp-mode for tree-sitter enabled buffers."
    (when (treesit-sexp--treesit-available-p)
        (treesit-sexp-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-treesit-sexp-mode treesit-sexp-mode
    treesit-sexp-global-mode-setup
    :require 'treesit-sexp
    :group 'treesit-sexp)

(provide 'treesit-sexp)
;;; treesit-sexp.el ends here
