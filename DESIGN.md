# Structural Editing Commands Design

This document describes how to implement structural editing commands (splice, slurp, barf) using only built-in Emacs list and s-expression movement functions.

## Built-in Commands Reference

The following built-in Emacs commands are used throughout the implementation:

- **`forward-sexp`**
  
  Move forward across one balanced expression (sexp).
  
  With ARG, do it that many times. Negative arg -N means move backward across N balanced expressions. This command assumes point is not in a string or comment. Calls `forward-sexp-function` to do the work, if that is non-nil.
  
  If unable to move over a sexp, signal `scan-error` with three arguments: a message, the start of the obstacle (usually a parenthesis or list marker of some kind), and end of the obstacle.

- **`backward-sexp`**
  
  Move backward across one balanced expression (sexp).
  
  With ARG, do it that many times. Negative arg -N means move forward across N balanced expressions. This command assumes point is not in a string or comment. Uses `forward-sexp` to do the work.

- **`kill-sexp`**
  
  Kill the sexp (balanced expression) following point.
  
  With ARG, kill that many sexps after point. Negative arg -N means kill N sexps before point. This command assumes point is not in a string or comment.

- **`forward-list`**
  
  Move forward across one balanced group of parentheses.
  
  This command will also work on other parentheses-like expressions defined by the current language mode. With ARG, do it that many times. Negative arg -N means move backward across N groups of parentheses. This command assumes point is not in a string or comment.

- **`backward-list`**
  
  Move backward across one balanced group of parentheses.
  
  This command will also work on other parentheses-like expressions defined by the current language mode. With ARG, do it that many times. Negative arg -N means move forward across N groups of parentheses. This command assumes point is not in a string or comment.

- **`up-list`**
  
  Move forward out of one level of parentheses.
  
  This command will also work on other parentheses-like expressions defined by the current language mode. With ARG, do this that many times. A negative argument means move backward but still to a less deep spot.

- **`backward-up-list`**
  
  Move backward out of one level of parentheses.
  
  This command will also work on other parentheses-like expressions defined by the current language mode. With ARG, do this that many times. A negative argument means move forward but still to a less deep spot.

- **`down-list`**
  
  Move forward down one level of parentheses.
  
  This command will also work on other parentheses-like expressions defined by the current language mode. With ARG, do this that many times. A negative argument means move backward but still go down a level. This command assumes point is not in a string or comment.

- **`transpose-sexps`**
  
  Like C-t (`transpose-chars`), but applies to sexps.
  
  Unlike `transpose-words`, point must be between the two sexps and not in the middle of a sexp to be transposed. With non-zero prefix arg ARG, effect is to take the sexp before point and drag it forward past ARG other sexps (backward if ARG is negative).

---

## Key Distinctions

### forward-sexp vs forward-list

**`forward-sexp`** moves across any balanced expression:
- Individual symbols, numbers, string constants
- Parenthesized expressions (like `(foo bar)`)
- In `(foo bar)`, starting at `(`, `forward-sexp` stops at `foo`

**`forward-list`** moves across ONLY parenthesized expressions:
- Skips from opening delimiter directly to matching closing delimiter
- In `(foo bar)`, starting at `(`, `forward-list` stops after `)`
- In `foo bar` with no parentheses, `forward-list` would fail

**Use `forward-sexp`** for element-by-element navigation (atoms and lists)
**Use `forward-list`** to skip entire parenthesized expressions at once

### Command Philosophy

While the implementation primarily uses s-expression and list movement commands:

- **Region-based commands** like `delete-region`, `kill-region`, `yank`, and `mark-sexp` are allowed and encouraged when appropriate for operating on expression boundaries
- **Regular editing commands** (e.g., `delete-char`, `insert`) can be used when necessary for character-level operations
- The **goal** is to predominantly operate on the level of balanced expressions to ensure:
  - Commands always work reliably
  - Operations are structurally sound
  - Buffer manipulation respects Lisp syntax boundaries

### Design vs Implementation

The algorithms in this document are **design specifications** that capture the intent and logical flow of each command. The actual implementations may differ slightly based on:

- Edge cases discovered during testing (e.g., adjacent parentheses with no whitespace)
- Buffer position stability during destructive operations
- Semantic clarity (choosing the most readable and maintainable approach)
- Performance considerations

For example, an algorithm might specify `(forward-sexp)` to reach a position, while the implementation uses `(forward-list) (backward-char)` instead if testing reveals that approach handles edge cases better. Both achieve the same conceptual goal, but the latter might be more robust in practice.

---

## Command Implementation Principles

All commands use these strategies:
- **Error Handling**: Use `condition-case` with `scan-error` handlers
- **Cursor Preservation**: Use `save-excursion` to keep cursor position stable
- **Position Safety**: Prefer navigation-based positioning over stored buffer positions to avoid issues with buffer shifts during modification

---

## 1. splice

**Purpose**: Remove parentheses around the current sexp, merging it with its parent.

**Example**:
```
Before: (a (b|c) d)
After:  (a b|c d)
            ^ cursor stays here
```

**Algorithm**:
1. Move to the opening parenthesis of current list: `(backward-up-list)`
2. Save position as `start`
3. Move to the closing parenthesis: `(forward-sexp)`
4. Delete the closing paren: `(delete-char -1)`
5. Return to `start` position
6. Delete the opening paren: `(delete-char 1)`

**Key insight**: The parentheses are always adjacent to the sexp boundaries, so simple character deletion works.

---

## 2. slurp-forward

**Purpose**: Pull the next sexp into the current list.

**Example**:
```
Before: (a (|b) c d)
After:  (a (|b c) d)
            ^ cursor stays here
```

**Algorithm**:
1. Save the insertion position: `insertion-pos = (point)`
2. Keep going up the tree until we find a sibling to grab:
   - Loop: do `(backward-up-list)`, then `(forward-list)`
   - Try `(forward-sexp)`
   - If successful, we've found a parent with a next sibling
   - If `scan-error`, continue up the tree
3. Once we found a parent with a next sibling, mark its start
4. Move to the sibling's end: `(forward-sexp)`
5. Extract the sibling: `delete-and-extract-region start (point)`
6. Return to `insertion-pos`
7. Move to end of current list and insert the extracted text

**Key insight**: We must traverse up the tree hierarchy until we find a level where there's actually a next element after our current position. This handles deeply nested cases like `(a (b (c|)) d)` where we need to go up two levels to find `d`.

---

## 3. slurp-backward

**Purpose**: Pull the previous sexp into the current list.

**Example**:
```
Before: (a b (|c) d)
After:  (a (b |c) d)
              ^ cursor stays here
```

**Algorithm**:
1. Move to current list's opening paren: `(backward-up-list)`
2. Extract previous sexp with following space:
   - Capture extraction end: `space-end = (point)` (starts just before the opening parenthesis of the starting expression)
   - Move to the start of the previous sexp: `(backward-sexp)`
   - Extract region: `(delete-and-extract-region (point) space-end)`
3. Navigate to insertion position: `(forward-char)` (move past inner list's opening paren)
4. Insert extracted text (includes the space needed): `(insert text-to-slurp)`

**Key insight**: Use exclusively sexp-based navigation (`backward-sexp`, `forward-sexp`) rather than character movement for reliability. The extraction captures both the previous sexp and its following space separator in one region operation, ensuring structural integrity even when elements are adjacent.

**Edge case handling**:
- The unified region extraction naturally handles both spaced and unspaced cases

---

## 4. barf-forward

**Purpose**: Move the last element out of the current list.

**Example**:
```
Before: (a b |(c d) e)
After:  (a b (|c) d e)
              ^ cursor stays here
```

**Algorithm**:
1. Move to current list's opening paren: `(backward-up-list)`, `(forward-char)`
2. Move to last element's start:
   - Navigate through all elements until at last one by calling `forward-sexp` until we get a `scan-error`, setting `last-element-end = (point)`
3. Extract last element (including any preceding space) by calling `backward-sexp` and calling `(delete-and-extract-region (point) last-element-end)`
4. Move to after current list's closing paren by using `backward-up-list`, `forward-list`
5. Insert extracted element followed by space

**Key insight**: The last element's boundaries are found by repeatedly moving forward through the list until reaching the end. The insertion point is just after the current list's closing parenthesis.

---

## 5. barf-backward

**Purpose**: Move the first element out of the current list.

**Example**:
```
Before: (a (b c|) d e)
After:  (a b (c|) d e)
               ^ cursor stays here
```

**Algorithm**:
1. Move to current list's opening paren: `(backward-up-list)`
3. Delete opening paren: `(delete-char 1)`
2. Extract first element (including following space):
   - Move to first element: set `first-element-start = (point)`
   - Move to end of first element: `(forward-sexp)`
5. Insert opening paren after this first element we just skipped

**Key insight**: The first element extraction requires careful handling of the opening parenthesis. Delete the paren first, then reinsert it after inserting the extracted element.
