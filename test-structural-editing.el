;;; test-structural-editing.el --- Unit tests for structural editing commands -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for structural-editing.el commands using ERT
;; Run with: emacs -batch -Q -L . -l test-structural-editing.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(load-file "structural-editing.el")

(defun test--extract-pos (content-with-pipe)
  "Extract cursor position and content from a string containing a pipe | character.
Returns (CONTENT . POS) where POS is the position of the pipe, and CONTENT has the pipe removed."
  (let ((pipe-pos (seq-position (string-to-list content-with-pipe) ?|)))
    (unless pipe-pos
      (error "No pipe character found in test content: %s" content-with-pipe))
    (cons (replace-regexp-in-string "|" "" content-with-pipe)
          (1+ pipe-pos)))) ; Convert 0-index to 1-index for Emacs

(defun test--run-in-buffer (content-with-pipe command)
  "Run COMMAND in a temporary buffer with CONTENT-WITH-PIPE.
Returns the buffer string after running the command."
  (let* ((content-info (test--extract-pos content-with-pipe))
         (content (car content-info))
         (pos (cdr content-info))
         (buffer (get-buffer-create "*test-temp*")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (insert content)
          (goto-char pos)
          (funcall command)
          (buffer-string))
      (kill-buffer buffer))))

;;;=================================================================
;;; Test: structural-editing-splice
;;;=================================================================

(ert-deftest test-splice-basic-case ()
  (should (string= (test--run-in-buffer "(a (|b) c)" 'structural-editing-splice)
                   "(a b c)")))

(ert-deftest test-splice-nested-case ()
  (should (string= (test--run-in-buffer "(a (b (|c) d) e)" 'structural-editing-splice)
                   "(a (b c d) e)")))

(ert-deftest test-splice-at-beginning ()
  (should (string= (test--run-in-buffer "((|a b) c)" 'structural-editing-splice)
                   "(a b c)")))

(ert-deftest test-splice-at-end ()
  (should (string= (test--run-in-buffer "(a (b |c))" 'structural-editing-splice)
                   "(a b c)")))

(ert-deftest test-splice-completely-nested ()
  (should (string= (test--run-in-buffer "(a (|b c d))" 'structural-editing-splice)
                   "(a b c d)")))

;;;=================================================================
;;; Test: structural-editing-slurp-forward
;;;=================================================================

(ert-deftest test-slurp-forward-basic-case ()
  (should (string= (test--run-in-buffer "(a (|b) c)" 'structural-editing-slurp-forward)
                   "(a (b c))")))

(ert-deftest test-slurp-forward-multiple-elements ()
  (should (string= (test--run-in-buffer "(a (|b) c d e)" 'structural-editing-slurp-forward)
                   "(a (b c) d e)")))

(ert-deftest test-slurp-forward-empty-inner-list ()
  (should (string= (test--run-in-buffer "(a (|) b c)" 'structural-editing-slurp-forward)
                   "(a (b) c)")))

(ert-deftest test-slurp-forward-nested-outer-list ()
  (should (string= (test--run-in-buffer "(x (a (|b) c) d)" 'structural-editing-slurp-forward)
                   "(x (a (b c)) d)")))

(ert-deftest test-slurp-forward-at-start ()
  (should (string= (test--run-in-buffer "(a (|b c) d)" 'structural-editing-slurp-forward)
                   "(a (b c d))")))

(ert-deftest test-slurp-forward-at-end ()
  (should (string= (test--run-in-buffer "(a (b c|) d)" 'structural-editing-slurp-forward)
                   "(a (b c d))")))

(ert-deftest test-slurp-forward-deeply-nested ()
  (should (string= (test--run-in-buffer "(a (b (c|)) d)" 'structural-editing-slurp-forward)
                   "(a (b (c d)))")))

;;;=================================================================
;;; Test: structural-editing-slurp-backward
;;;=================================================================

(ert-deftest test-slurp-backward-basic-case ()
  (should (string= (test--run-in-buffer "(a (|b) c)" 'structural-editing-slurp-backward)
                   "((a b) c)")))

(ert-deftest test-slurp-backward-multiple-elements-before ()
  (should (string= (test--run-in-buffer "(a b c (|d) e)" 'structural-editing-slurp-backward)
                   "(a b (c d) e)")))

(ert-deftest test-slurp-backward-at-beginning ()
  (should (string= (test--run-in-buffer "(a (|b c))" 'structural-editing-slurp-backward)
                   "((a b c))")))

(ert-deftest test-slurp-backward-nested-outer-list ()
  (should (string= (test--run-in-buffer "(x (a b (|c) d) e)" 'structural-editing-slurp-backward)
                   "(x (a (b c) d) e)")))

(ert-deftest test-slurp-backward-adjacent-parens ()
  (should (string= (test--run-in-buffer "((a) (|b) c)" 'structural-editing-slurp-backward)
                   "(((a) b) c)")))

(ert-deftest test-slurp-backward-deeply-nested ()
  (should (string= (test--run-in-buffer "(a (b (c|)) d)" 'structural-editing-slurp-backward)
                   "(a ((b c)) d)")))

;;;=================================================================
;;; Test: structural-editing-barf-forward
;;;=================================================================

(ert-deftest test-barf-forward-basic-case ()
  (should (string= (test--run-in-buffer "(a b (|c d) e)" 'structural-editing-barf-forward)
                   "(a b (c) d e)")))

(ert-deftest test-barf-forward-single-element ()
  (should (string= (test--run-in-buffer "(a (|b) c)" 'structural-editing-barf-forward)
                   "(a () b c)")))

(ert-deftest test-barf-forward-at-beginning ()
  (should (string= (test--run-in-buffer "(a b (|c d e) f)" 'structural-editing-barf-forward)
                   "(a b (c d) e f)")))

(ert-deftest test-barf-forward-nested ()
  (should (string= (test--run-in-buffer "(x (a b (|c d) e) f)" 'structural-editing-barf-forward)
                   "(x (a b (c) d e) f)")))

;;;=================================================================
;;; Test: structural-editing-barf-backward
;;;=================================================================

(ert-deftest test-barf-backward-basic-case ()
  (should (string= (test--run-in-buffer "(a (b| c) d)" 'structural-editing-barf-backward)
                   "(a b (c) d)")))

(ert-deftest test-barf-backward-multiple-elements ()
  (should (string= (test--run-in-buffer "(a b (|c d e) f)" 'structural-editing-barf-backward)
                   "(a b c (d e) f)")))

(ert-deftest test-barf-backward-single-element ()
  (should (string= (test--run-in-buffer "(a (|b) c)" 'structural-editing-barf-backward)
                   "(a b () c)")))

(ert-deftest test-barf-backward-nested ()
  (should (string= (test--run-in-buffer "(x a ((|b) c) d e)" 'structural-editing-barf-backward)
                   "(x a (b () c) d e)")))

;;;=================================================================
;;; Edge case tests
;;;=================================================================

(ert-deftest test-splice-with-strings ()
  (should (string= (test--run-in-buffer "(\"a\" (|\"b\") \"c\")" 'structural-editing-splice)
                   "(\"a\" \"b\" \"c\")")))

(ert-deftest test-splice-deeply-nested ()
  (should (string= (test--run-in-buffer "(a (b (c (|d) e) f) g)" 'structural-editing-splice)
                   "(a (b (c d e) f) g)")))

(ert-deftest test-slurp-forward-very-deeply-nested ()
  (should (string= (test--run-in-buffer "(a (b (c (|d) e) f) g)" 'structural-editing-slurp-forward)
                   "(a (b (c (d e)) f) g)")))

(ert-deftest test-slurp-forward-into-empty-list ()
  (should (string= (test--run-in-buffer "(a (|) b c)" 'structural-editing-slurp-forward)
                   "(a (b) c)")))

(ert-deftest test-slurp-backward-from-empty-list ()
  (should (string= (test--run-in-buffer "(a (|) b)" 'structural-editing-slurp-backward)
                   "((a) b)")))

(ert-deftest test-slurp-backward-three-levels ()
  (should (string= (test--run-in-buffer "(a (b (c (|d))))" 'structural-editing-slurp-backward)
                   "(a (b ((c d))))")))

(ert-deftest test-barf-forward-with-symbols ()
  (should (string= (test--run-in-buffer "(:key val (|sym1 sym2) rest)" 'structural-editing-barf-forward)
                   "(:key val (sym1) sym2 rest)")))

(provide 'test-structural-editing)
;;; test-structural-editing.el ends here
