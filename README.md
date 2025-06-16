kdl-mode
========

Emacs major mode for editing the [KDL document language](https://kdl.dev/).

# Screenshot

![kdl-mode](screenshots/kdl-mode.png)

# Installation

- Automatic package installation from [Melpa](https://melpa.org/).

# Features

- Syntax highlighting using regular expression

- Syntax highlighting using Tree-Sitter (support Slashdash comment style)

- Automatic code indentation.

# Troubleshooting

- If `tree-sitter-kdl` isn't installed automatically when open a KDL file, run `M-x kdl-install-tree-sitter-grammar` to ensure it is installed.

# Acknowledgement

- Syntax highlighting using [tree-sitter-kdl](https://github.com/tree-sitter-grammars/tree-sitter-kdl) was adopted from [kdl-ts-mode](https://github.com/dataphract/kdl-ts-mode/):
