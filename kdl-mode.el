;;; kdl-mode.el --- Major mode for editing KDL files.

;; Copyright © 2022, by Ta Quang Trung

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 27 April, 2025
;; Keywords: languages
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
;; - Syntax highlight for KDL intermediate code.

;; Installation:
;; - Automatic package installation from Melpa.
;; - Manual installation by putting the `kdl-mode.el' file in Emacs' load path.

;;; Code:

(require 'rx)

(defconst kdl-special-constants
  '("inf"
    "nan"
    "true"
    "false")
  "List of KDL constants.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting

(defvar kdl-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; C++ style comment "// ..."
    (modify-syntax-entry ?\/ ". 124" syntax-table)
    (modify-syntax-entry ?* ". 23b" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    ;; Punctuation
    (modify-syntax-entry ?= "." syntax-table)
    syntax-table)
  "Syntax table for `kdl-mode'.")

(defvar kdl-special-constants-regexp
  (concat
   (rx symbol-start)
   (regexp-opt kdl-special-constants t)
   (rx symbol-end))
  "Regular expression to match special KDL constant.")

(defun kdl--match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))

(defun kdl--match-node-name (limit)
  "Search the buffer forward until LIMIT matching node names.
Highlight the 1st result."
  (kdl--match-regexp
   (concat
    (rx symbol-start) "\\([a-zA-Z0-9_-]+\\)" (rx symbol-end)
    "[[:space:]]*[^=]")
   limit))

(defconst kdl-font-locks
  (list
   `(,kdl-special-constants-regexp . font-lock-constant-face)
   '(kdl--match-node-name (1 font-lock-function-name-face)))
  "Font lock keywords of `kdl-mode'.")

;;;;;;;;;;;;;;;;;;
;;; Indentation

(defun kdl-indent-line (&optional indent)
  "Indent the current line according to the KDL syntax, or supply INDENT."
  (interactive "P")
  (let ((pos (- (point-max) (point)))
        (indent (or indent (kdl--calculate-indentation)))
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

(defun kdl--calculate-indentation ()
  "Calculate the indentation of the current line."
  (let (indent)
    (save-excursion
      (back-to-indentation)
      (let* ((ppss (syntax-ppss))
             (depth (car ppss))
             (paren-start-pos (cadr ppss))
             (base (* tab-width depth)))
        (unless (= depth 0)
          (setq indent base)
          (cond ((looking-at "\s*[})]")
                 ;; closing a block or a parentheses pair
                 (setq indent (- base tab-width)))
                ((looking-at "\s*:=")
                 ;; indent for multiple-line assignment
                 (setq indent (+ base (* 2 tab-width))))
                ((looking-back "\s*:=\s*\n\s*")
                 ;; indent for multiple-line assignment
                 (setq indent (+ base (* 2 tab-width))))))))
    indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major mode settings

;;;###autoload
(define-derived-mode kdl-mode prog-mode
  "kdl-mode"
  "Major mode for editing Ethereum KDL intermediate code."
  :syntax-table kdl-syntax-table

  ;; Syntax highlighting
  (setq font-lock-defaults '(kdl-font-locks))

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
