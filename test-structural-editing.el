;;; test-structural-editing.el --- Unit tests for structural editing commands -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for structural-editing.el commands
;; Run with: emacs -batch -Q -L . -l test-structural-editing.el

;;; Code:

(load-file "structural-editing.el")

(defvar test-results '()
    "List of test results (TEST-NAME . PASSED-P)")

(defvar test-stats nil
    "Cons cell (PASSED . FAILED) counting test statistics")

(setq test-stats (cons 0 0)) ; (passed . failed)

(defun test-assert (test-name expected actual)
    "Run a test and record result."
    (let ((result (if (equal expected actual)
                          (progn (message "[PASS] %s" test-name) t)
                      (progn (message "[FAIL] %s" test-name)
                             (message "  Expected: %s" expected)
                             (message "  Actual:   %s" actual)
                             nil))))
        (setq test-results (cons (cons test-name result) test-results))
        result))

(defun test--extract-pos (content-with-pipe)
    "Extract cursor position and content from a string containing a pipe | character.
Returns (CONTENT . POS) where POS is the position of the pipe, and CONTENT has the pipe removed."
    (let ((pipe-pos (seq-position (string-to-list content-with-pipe) ?|)))
        (when (not pipe-pos)
            (error "No pipe character found in test content: %s" content-with-pipe))
        (cons (replace-regexp-in-string "|" "" content-with-pipe)
              (1+ pipe-pos)))) ; Convert 0-index to 1-index for Emacs

(defun test-run (test-name content-with-pipe command expected)
    "Run a test with CONTENT-WITH-PIPE (pipe indicates cursor pos) running COMMAND expecting EXPECTED result."
    (let* ((content-info (test--extract-pos content-with-pipe))
           (content (car content-info))
           (pos (cdr content-info))
           (buffer (generate-new-buffer "*test*"))
           (result nil)
           (error-data nil)
           (test-passed nil))
        (condition-case err
                (setq result (with-current-buffer buffer
                                 (insert content)
                                 (goto-char pos)
                                 (funcall command)
                                 (buffer-string)))
            (error
             (setq error-data err)
             (setq result (with-current-buffer buffer (buffer-string)))))
        (kill-buffer buffer)
        (if error-data
                (progn
                    (message "[ERROR] %s" test-name)
                    (message "  %s" error-data)
                    (message "  Error details:")
                    (let ((frames (backtrace-frames)))
                        (dolist (frame frames)
                            (when (and (listp frame) (cadr frame))
                                (message "    %s:%S" (or (car (caddr frame)) "?") (cadr frame)))))
                    (setq test-stats (cons (car test-stats) (1+ (cdr test-stats))))
                    (setq test-results (cons (cons test-name nil) test-results))
                    (setq test-passed nil))
            (setq test-passed (test-assert test-name expected result))
            (setq test-stats (cons (+ (car test-stats) (if test-passed 1 0))
                                   (+ (cdr test-stats) (if (not test-passed) 1 0)))))
        test-passed))

(defun test-stats-report ()
    "Print test statistics and return success/failure."
    (message "")
    (message "=== Test Summary ===")
    (message "Passed: %d" (car test-stats))
    (message "Failed: %d" (cdr test-stats)) 
    (message "Total:  %d" (+ (car test-stats) (cdr test-stats)))
    (message "Success rate: %.1f%%" (* 100.0 (/ (car test-stats) (+ (car test-stats) (cdr test-stats)))))
    (= (cdr test-stats) 0))

;;;=================================================================
;;; Test: structural-editing-splice
;;;=================================================================

(test-run "splice: basic case"
          "(a (|b) c)" 'structural-editing-splice
          "(a b c)")

(test-run "splice: nested case"
          "(a (b (|c) d) e)" 'structural-editing-splice
          "(a (b c d) e)")

(test-run "splice: at beginning"
          "((|a b) c)" 'structural-editing-splice
          "(a b c)")

(test-run "splice: at end"
          "(a (b |c))" 'structural-editing-splice
          "(a b c)")

(test-run "splice: completely nested"
          "(a (|b c d))" 'structural-editing-splice
          "(a b c d)")

;;;=================================================================
;;; Test: structural-editing-slurp-forward
;;;=================================================================

(test-run "slurp-forward: basic case"
          "(a (|b) c)" 'structural-editing-slurp-forward
          "(a (b c))")

(test-run "slurp-forward: multiple elements"
          "(a (|b) c d e)" 'structural-editing-slurp-forward
          "(a (b c) d e)")

(test-run "slurp-forward: empty inner list"
          "(a (|) b c)" 'structural-editing-slurp-forward
          "(a (b) c)")

(test-run "slurp-forward: nested outer list"
          "(x (a (|b) c) d)" 'structural-editing-slurp-forward
          "(x (a (b c)) d)")

(test-run "slurp-forward: at start of inner list"
          "(a (|b c) d)" 'structural-editing-slurp-forward
          "(a (b c d))")

(test-run "slurp-forward: at end of inner list"
          "(a (b c|) d)" 'structural-editing-slurp-forward
          "(a (b c d))")

(test-run "slurp-forward: extending a deeply nested list"
          "(a (b (c|)) d)" 'structural-editing-slurp-forward
          "(a (b (c d)))")

;;;=================================================================
;;; Test: structural-editing-slurp-backward
;;;=================================================================

(test-run "slurp-backward: basic case"
          "(a (|b) c)" 'structural-editing-slurp-backward
          "((a b) c)")

(test-run "slurp-backward: multiple elements before"
          "(a b c (|d) e)" 'structural-editing-slurp-backward
          "(a b (c d) e)")

(test-run "slurp-backward: at beginning of inner list"
          "(a (|b c))" 'structural-editing-slurp-backward
          "((a b c))")

(test-run "slurp-backward: nested outer list"
          "(x (a b (|c) d) e)" 'structural-editing-slurp-backward
          "(x (a (b c) d) e)")

(test-run "slurp-backward: adjacent parentheses"
          "((a) (|b) c)" 'structural-editing-slurp-backward
          "(((a) b) c)")

(test-run "slurp-backward: extending a deeply nested list"
          "(a (b (c|)) d)" 'structural-editing-slurp-backward
          "(a ((b c)) d)")

;;;=================================================================
;;; Test: structural-editing-barf-forward
;;;=================================================================

(test-run "barf-forward: basic case"
          "(a b (|c d) e)" 'structural-editing-barf-forward
          "(a b (c) d e)")

(test-run "barf-forward: single element inner list"
          "(a (|b) c)" 'structural-editing-barf-forward
          "(a () b c)")

(test-run "barf-forward: at beginning of inner list"
          "(a b (|c d e) f)" 'structural-editing-barf-forward
          "(a b (c d) e f)")

(test-run "barf-forward: nested case"
          "(x (a b (|c d) e) f)" 'structural-editing-barf-forward
          "(x (a b (c) d e) f)")

;;;=================================================================
;;; Test: structural-editing-barf-backward
;;;=================================================================

(test-run "barf-backward: basic case"
          "(a (b| c) d)" 'structural-editing-barf-backward
          "(a b (c) d)")

(test-run "barf-backward: inner list with multiple elements"
          "(a b (|c d e) f)" 'structural-editing-barf-backward
          "(a b c (d e) f)")

(test-run "barf-backward: single element inner list"
          "(a (|b) c)" 'structural-editing-barf-backward
          "(a b () c)")

(test-run "barf-backward: nested case"
          "(x a ((|b) c) d e)" 'structural-editing-barf-backward
          "(x a (b () c) d e)")

;;;=================================================================
;;; Edge case tests
;;;=================================================================

;; Test with strings
(test-run "splice: with strings"
          "(\"a\" (|\"b\") \"c\")" 'structural-editing-splice
          "(\"a\" \"b\" \"c\")")

;; Test deeply nested
(test-run "splice: deeply nested"
          "(a (b (c (|d) e) f) g)" 'structural-editing-splice
          "(a (b (c d e) f) g)")

(test-run "slurp-forward: deeply nested"
          "(a (b (c (|d) e) f) g)" 'structural-editing-slurp-forward
          "(a (b (c (d e)) f) g)")

;; Test empty lists
(test-run "slurp-forward: into empty list"
          "(a (|) b c)" 'structural-editing-slurp-forward
          "(a (b) c)")

(test-run "slurp-backward: from empty list"
          "(a (|) b)" 'structural-editing-slurp-backward
          "((a) b)")

;; Test multiple levels of nesting
(test-run "slurp-backward: three levels"
          "(a (b (c (|d))))" 'structural-editing-slurp-backward
          "(a (b ((c d))))")

;; Test with symbols and keywords
(test-run "barf-forward: with symbols"
          "(:key val (|sym1 sym2) rest)" 'structural-editing-barf-forward
          "(:key val (sym1) sym2 rest)")

;;;=================================================================
;;; Run all tests and report
;;;=================================================================

(kill-emacs (if (test-stats-report) 0 1))

(provide 'test-structural-editing)
;;; test-structural-editing.el ends here
