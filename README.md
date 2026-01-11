# treesit-sexp.el

Tree-sitter based s-expression navigation for Emacs. This package provides intelligent, syntax-aware movement functions that respect your code's structure rather than just character positions.

## Features

- **Smart Navigation**: Move by semantic units instead of characters
- **Punctuation Skipping**: Automatically skips over punctuation like commas, periods, semicolons, and arrows
- **Syntax-Aware**: Understands your code's actual structure via tree-sitter
- **Universal**: Works with any language supported by tree-sitter
- **Safe Fallbacks**: Gracefully falls back to standard sexp functions when tree-sitter is unavailable
- **Non-Disruptive**: Stops at delimiter boundaries without errors

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

Enable `treesit-sexp-mode` automatically in all tree-sitter enabled buffers:

```elisp
(global-treesit-sexp-mode 1)
```

### Enable Per-Buffer

Enable in specific buffers:

```elisp
(treesit-sexp-mode 1)
```

### Manual Usage

Call functions directly:

- `M-x treesit-sexp-forward` - Move forward by s-expressions
- `M-x treesit-sexp-backward` - Move backward by s-expressions
- `M-x treesit-sexp-mark-sexp` - Mark s-expressions
- `M-x treesit-sexp-up-list` - Move up one level of parentheses
- `M-x treesit-sexp-down-list` - Move down into nested structure
- `M-x treesit-sexp-forward-list` - Skip over balanced parentheses
- `M-x treesit-sexp-beginning-of-defun` - Move to function/class start
- `M-x treesit-sexp-end-of-defun` - Move to function/class end

### Keybindings

When `treesit-sexp-mode` is active, it replaces the standard sexp navigation functions:

- `C-M-f` / `C-M-b` - Forward/backward sexp
- `C-M-SPC` - Mark sexp
- `C-M-u` - Up list
- `C-M-d` - Down list
- `C-M-n` / `C-M-p` - Forward/backward list
- `C-M-a` / `C-M-e` - Beginning/end of defun

## How It Works

The package uses tree-sitter's syntax tree to:

1. Identify the semantic structure of your code
2. Skip over punctuation nodes (commas, semicolons, etc.)
3. Respect delimiter boundaries (parentheses, braces, brackets)
4. Navigate between named syntax nodes

Example: In `foo(bar, baz)`, moving forward will jump from `foo` to `bar` to `baz`, skipping the commas and parentheses.

## Supported Languages

Works with any language that has tree-sitter support, including:

- Python
- Rust
- JavaScript/TypeScript
- Go
- C/C++
- Java
- And many more...

## Differences from Standard Sexp Navigation

| Feature | Standard Sexp | treesit-sexp |
|---------|--------------|--------------|
| Navigation basis | Character syntax | Syntax tree nodes |
| Punctuation handling | Manual skipping | Automatic skipping |
| Language awareness | Syntax table only | Full parse tree |
| Multi-language | Limited | Universal |
| Error handling | Signals errors | Stops at boundaries |

## Contributing

Contributions are welcome! Please ensure:

1. Code follows existing style conventions
2. Changes maintain backward compatibility
3. New features include appropriate documentation

## License

Zero-Clause BSD License (0BSD)

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES.
