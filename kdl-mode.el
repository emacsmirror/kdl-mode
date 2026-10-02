;;; kdl-mode.el --- Major mode for editing KDL files -*- lexical-binding: t -*-

;; Copyright © 2025, by Ta Quang Trung

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 27 April, 2025
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/taquangtrung/emacs-kdl-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs major mode for editing KDL files (https://kdl.dev/).

;; Features:
;; - Syntax highlighting
;; - Automatic code indentation

;; Installation:
;; - Automatic package installation from Melpa.
;; - Manual installation by putting the `kdl-mode.el' file in Emacs' load path.

;; Acknowledgement:
;; - Syntax highlighting using tree-sitter was adopted from:
;;   https://github.com/dataphract/kdl-ts-mode/

;;; Code:

(require 'rx)
(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options

(defgroup kdl-mode nil
  "Support for editing KDL configuration files."
  :link '(url-link "https://kdl.dev/")
  :group 'languages
  :prefix "kdl-")

(defcustom kdl-indent-level nil
  "Amount of indentation per nesting level within a KDL expression."
  :group 'kdl-mode
  :local t
  :type '(choice (natnum :tag "Number of spaces" :value 4)
                 (const :tag "Use `tab-width'" nil)))

;; kdl-natnum-or-nil-p needs to be autoloaded, so it can't be embedded
;; in the defcustom above, and it needs to have a name and a docstring
;; so that ‘C-h v kdl-indent-level’ provides a useful explanation for
;; what values are safe.

;;;###autoload
(defun kdl-natnum-or-nil-p (v)
  "Return t if V is either a nonnegative integer, or nil."
  (or (natnump v) (null v)))

;;;###autoload
(put 'kdl-indent-level 'safe-local-variable 'kdl-natnum-or-nil-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar kdl-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; C++ style comment "// ..." and "/* ... */"
    (modify-syntax-entry ?\/ ". 124" syntax-table)
    (modify-syntax-entry ?* ". 23b" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    ;; Punctuation
    (modify-syntax-entry ?= "." syntax-table)
    syntax-table)
  "Syntax table for `kdl-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting using regular expression

(defconst kdl-special-constants
  '("inf"
    "nan"
    "true"
    "false")
  "List of KDL constants.")

(defvar kdl-special-constants-regexp
  (concat
   (rx symbol-start)
   (regexp-opt kdl-special-constants t)
   (rx symbol-end))
  "Regular expression to match special KDL constant.")

(defun kdl-match-regexp (re bound)
  "Generic regular expression matching wrapper for RE with a given BOUND."
  (re-search-forward re bound t nil))

(defun kdl-match-node-name (bound)
  "Search the buffer forward until BOUND to match node names."
  (kdl-match-regexp
   (concat
    (rx symbol-start) "\\([a-zA-Z0-9_-]+\\)" (rx symbol-end)
    "[[:space:]]*[^=]")
   bound))

(defun kdl-match-property-name (bound)
  "Search the buffer forward until BOUND to match property names."
  (kdl-match-regexp
   (concat
    (rx symbol-start) "\\([a-zA-Z0-9_-]+\\)" (rx symbol-end)
    "[[:space:]]*=")
   bound))

(defconst kdl-font-locks
  (list
   `(,kdl-special-constants-regexp . font-lock-constant-face)
   '(kdl-match-node-name (1 font-lock-function-name-face))
   '(kdl-match-property-name (1 font-lock-variable-name-face)))
  "Font lock keywords of `kdl-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting using tree-sitter
;;
;; Adopted from: https://github.com/dataphract/kdl-ts-mode/

(defun kdl--treesit-fontify-commented-parent (node override start end &rest _)
  "Fontify the parent of the slashdash comment NODE as a comment.
OVERRIDE, START and END are as in `treesit-font-lock-rules' capture
functions.  The slashdash node is matched on its own, because nested
patterns such as `(node (node_field (node_field_comment)))' take
practically forever to compile with grammar v2."
  (when-let* ((parent (treesit-node-parent node)))
    (treesit-fontify-with-override
     (treesit-node-start parent) (treesit-node-end parent)
     'font-lock-comment-face override start end)))

(defvar kdl-treesit-font-locks
  (treesit-font-lock-rules
   :language 'kdl
   :feature 'bracket
   '((["(" ")" "{" "}"]) @font-lock-bracket-face)

   :language 'kdl
   :feature 'comment
   '((single_line_comment) @font-lock-comment-face
     (multi_line_comment) @font-lock-comment-face)

   :language 'kdl
   :feature 'constant
   '("null" @font-lock-constant-face
     (boolean) @font-lock-constant-face)

   :language 'kdl
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'kdl
   :feature 'type
   '((type) @font-lock-type-face)

   :language 'kdl
   :feature 'string
   :override t
   '((string) @font-lock-string-face)

   :language 'kdl
   :feature 'escape-sequence
   :override t
   '((escape) @font-lock-escape-face)

   :language 'kdl
   :feature 'node
   :override t
   '((node (identifier) @font-lock-function-call-face))

   :language 'kdl
   :feature 'property
   :override t
   '((prop (identifier) @font-lock-property-use-face))

   :language 'kdl
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)

   :language 'kdl
   :feature 'comment
   :override t
   '((node_comment) @kdl--treesit-fontify-commented-parent
     (node_field_comment) @kdl--treesit-fontify-commented-parent
     (node_children_comment) @kdl--treesit-fontify-commented-parent))

  "Tree-sitter font-lock settings for `kdl-mode'.")

(defun kdl--treesit-font-lock-settings ()
  "Return the tree-sitter font-lock settings for the loaded KDL grammar.
Grammar v2 parses `#null' as a `keyword' node that the common rules do not
match, while grammar v1 has no `keyword' node and rejects any query on it."
  (if (treesit-query-valid-p 'kdl '((keyword) @font-lock-constant-face))
      (append kdl-treesit-font-locks
              (treesit-font-lock-rules
               :language 'kdl
               :feature 'constant
               '((keyword) @font-lock-constant-face)))
    kdl-treesit-font-locks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun kdl-indent-line (&optional indent)
  "Indent the current line according to the KDL syntax, or supply INDENT."
  (interactive "P")
  (let ((pos (- (point-max) (point)))
        (indent (or indent (kdl-calculate-indentation)))
        (shift-amount nil)
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (null indent)
        (goto-char (- (point-max) pos))
      (setq shift-amount (- indent (current-column)))
      (unless (zerop shift-amount)
        (delete-region beg (point))
        (indent-to indent))
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun kdl-calculate-indentation ()
  "Calculate the indentation of the current line."
  (save-excursion
    (back-to-indentation)
    (let* ((ppss (syntax-ppss))
           (depth (car ppss))
           (step (max 0 (or kdl-indent-level tab-width)))
           (base (* step depth)))
      (cond
       ((= depth 0) nil)
       ((looking-at (rx (* space) (char ?\} ?\))))
        ;; closing a block or a parentheses pair
        (- base step))
       ((looking-back (rx space "\\\n" (* space)) nil nil)
        ;; continued line
        (+ base step))
       (t base)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions

(defconst kdl--grammar-repository
  "https://github.com/tree-sitter-grammars/tree-sitter-kdl"
  "Repository of the tree-sitter-kdl grammar.")

(defconst kdl--grammar-revisions '("v2.0.0" "v1.1.0")
  "Supported revisions of tree-sitter-kdl, newest first.
Revisions are pinned so that the font-lock rules do not break when the
grammar changes.  v2.0.0 uses grammar ABI 15, which needs libtree-sitter
0.25 or later; v1.1.0 uses ABI 14, which older libraries load.")

(defun kdl--install-grammar-revision (revision)
  "Build and install the tree-sitter-kdl grammar at REVISION."
  (let ((treesit-language-source-alist
         `((kdl . (,kdl--grammar-repository ,revision "src")))))
    (treesit-install-language-grammar 'kdl)))

(defun kdl-install-tree-sitter-grammar ()
  "Install tree-sitter-kdl grammar.
Install the newest supported revision that the installed libtree-sitter
can load.  A grammar source set by the user in
`treesit-language-source-alist' is used as is."
  (interactive)
  (if (assoc 'kdl treesit-language-source-alist)
      (treesit-install-language-grammar 'kdl)
    (let ((revisions kdl--grammar-revisions))
      (kdl--install-grammar-revision (pop revisions))
      (while (and revisions (not (treesit-language-available-p 'kdl)))
        (message "kdl-mode: the grammar cannot be loaded, installing %s"
                 (car revisions))
        (kdl--install-grammar-revision (pop revisions))
        ;; Emacs keeps the rejected library loaded, so only a new session
        ;; can load the replacement.
        (message "kdl-mode: restart Emacs to load the tree-sitter-kdl grammar")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major mode settings

;;;###autoload
(define-derived-mode kdl-mode prog-mode
  "kdl-mode"
  "Major mode for editing KDL document language."
  :syntax-table kdl-syntax-table

  ;; Syntax highlighting using regex
  (setq font-lock-defaults '(kdl-font-locks))

  ;; Install tree-sitter grammar if not already installed
  (unless (treesit-ready-p 'kdl)
    (message "kdl-mode: tree-sitter-kdl is not available. Start installing it...")
    (kdl-install-tree-sitter-grammar))

  ;; Syntax highlighting using tree-sitter
  (when (treesit-ready-p 'kdl)
    (treesit-parser-create 'kdl)
    (setq-local treesit-font-lock-settings (kdl--treesit-font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (string type)
                  (constant escape-sequence number node property)
                  (bracket error)))
    (treesit-major-mode-setup))

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'kdl-indent-line)

  ;; Set comment command
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-multi-line nil)
  (setq-local comment-use-syntax t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-mode))

;; Finally export the `kdl-mode'
(provide 'kdl-mode)

;;; kdl-mode.el ends here
