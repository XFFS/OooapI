;;; atd-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 um
;;
;; Author: Shon Feder <@shonfeder>
;; Maintainer: Shon Feder <@shonfeder>
;; Created: March 24, 2023
;; Modified: March 24, 2023
;; Version: 0.0.1
;; Keywords: tools ocaml json atd
;; Homepage: https://github.com/shonfeder/atd-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; Define the ATD mode syntax table
(defvar atd-mode-syntax-table nil "Syntax table for `atd-mode'.")
(setq atd-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?\( "()1n" syn-table)
        (modify-syntax-entry ?\) ")(4n" syn-table)
        (modify-syntax-entry ?* ". 23n" syn-table)
        syn-table))

;; Define a regular expression for matching record labels
(defconst atd-record-label-regexp
  "\\<\\([a-z][A-Za-z0-9_]*\\) *:"
  "Regular expression for matching record labels in `atd-mode'.")

;; Define the ATD mode font-lock keywords
(defvar atd-font-lock-keywords
  (list
   '("\\<\\(type\\|alias\\|record\\|variant\\|abstract\\|enum\\)\\>" . font-lock-keyword-face)
   '("\\<[A-Z][A-Za-z0-9_]*\\>" . font-lock-type-face)
   `(,atd-record-label-regexp 1 font-lock-function-name-face))
  "Keyword highlighting specification for `atd-mode'.")

;; Define the ATD mode
(define-derived-mode atd-mode prog-mode "ATD"
  "A major mode for editing ATD (Adjustable Type Definitions) Language files."
  :syntax-table atd-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local comment-start-skip "(\\*")
  (setq-local comment-end-skip "\\*)")
  (setq-local font-lock-defaults '(atd-font-lock-keywords)))

;; Associate the ATD mode with the ".atd" file extension
(add-to-list 'auto-mode-alist '("\\.atd\\'" . atd-mode))

(provide 'atd-mode)
;;; atd-mode.el ends here
