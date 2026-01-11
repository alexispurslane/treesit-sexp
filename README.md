# treesit-sexp.el

*Make Emacs treat *all* code like Lisp.*

This package enhances Emacs's [built-in S-expression navigation and editing commands](https://dawranliou.com/blog/structural-editing-in-vanilla-emacs/) --- used by Emacs die-hards as well as structural editing packages like `puni-mode`, `evil`, and countless others --- to understand any programming language via tree-sitter, not just Lisp's parenthesis-based syntax.

**It does not add new commands.** Instead, it teaches Emacs's *existing* "structural editing" commands to work intelligently with tree-sitter:

- `forward-sexp` / `backward-sexp` (bound to `C-M-f` / `C-M-b`)
- `up-list` / `down-list` (bound to `C-M-u` / `C-M-d`)
- `mark-sexp` (bound to `C-M-SPC`)
- `raise-sexp` (not bound by default in Emacs, but I bind it to `C-M-r`)
- `beginning-of-defun` / `end-of-defun` (bound to `C-M-a` / `C-M-e`)

This means **every package built on these primitives** --- including `puni-mode` (with advanced operations like `raise-sexp`), `evil`, `expand-region`, and your own custom functions --- now automatically works with Python, Rust, Go, JavaScript, and any other tree-sitter-enabled language.

## The Key Insight

Lisp S-expressions are language-construct agnostic by design. Lisp S-expressions represent a **parse tree**: a nested hierarchy of lists containing lexed symbols, but **without any differentiation between node types** --- a `function_definition`, `class_declaration`, and `if_statement` are all just "lists" to Lisp prior to execution. Emacs's built in structural editing and navigation commands piggyback on this: they don't care what construct they're looking at; all they need, generally speaking, is a parse tree, and they can work their magic.

This is perfect if we want to avoid having to write tree-sitter queries to get tree-sitter-based structural editing, because tree-sitter's concrete syntax tree has exactly the same property when you ignore node types: it's a generic nested tree structure. So commands designed for Lisp's untyped parse trees are a perfect fit for queryless tree-sitter traversal. And we do want that: attempting to write language-specific tree-sitter queries is why other tree-sitter structural editing packages struggle with consistency, broad language support, and maintenence: they try to pattern-match specific node types (`function_definition`, `class_declaration`, etc.), requiring manual implementation for each language.

By taking advantage of the fact that, in Emacs's built in S-expression commands, you can get totally language-construct agnostic structural editing: this package doesn't have to maintain a gigantic list of query configuration files for every language it wants to support.

## Features

- **Universal By Design**: Works with all tree-sitter languages automatically --- no language-specific code, queries, or manual implementations from mode authors needed
- **Transparent Integration**: Hijacks standard sexp commands when tree-sitter is available
- **Semantic Navigation**: Moves by actual code units, not characters or regex patterns
- **Punctuation Skipping**: Automatically skips commas, semicolons, arrows, and other noise
- **Graceful Fallback**: Uses standard sexp functions when tree-sitter is unavailable

## Requirements

- Emacs 29.1 or later
- Tree-sitter grammar for your target language(s)

## Installation

### Manual Installation

Clone this repository and add to your load path:

```elisp
(add-to-list 'load-path "/path/to/treesit-sexp")
(require 'treesit-sexp)
```

### Using use-package

```elisp
(use-package treesit-sexp
  :load-path "/path/to/treesit-sexp")
```

## Usage

### Enable Globally (Recommended)

Activate `treesit-sexp-mode` automatically in all tree-sitter enabled buffers:

```elisp
(global-treesit-sexp-mode 1)
```

### Enable Per-Buffer

Enable in specific buffers:

```elisp
(treesit-sexp-mode 1)
```

## Supported Languages

Works with **any language** that has tree-sitter support. No configuration needed:

- Python (`python-ts-mode`)
- Rust (`rust-ts-mode`)
- Go (`go-ts-mode`)
- JavaScript/TypeScript (`typescript-ts-mode`)
- C/C++ (`c-ts-mode`, `cpp-ts-mode`)
- Java (`java-ts-mode`)
- And any future tree-sitter mode...

## License

Zero-Clause BSD License (0BSD)

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES.
