;;; modern-sh.el --- Minor mode for shell script writing  -*- lexical-binding: t -*-
;;
;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; URL: https://github.com/damon-kwok/modern-sh
;; Keywords: languages programming
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2020 Damon Kwok
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a major mode for the Verona programming language
;;
;; For more details, see the project page at
;; https://github.com/damon-kwok/modern-sh
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install modern-sh
;;
;;; Code:

(defconst modern-sh-keywords
  '("let" "local"                       ;
     "if" "else" "elif" "then" "fi"     ;
     "case" "esac" "for" "in"           ;
     "while" "until" "do" "done")
  "Bash language keywords.")

(defconst modern-sh-declaration-keywords '("function" "declare")
  "Bash declaration keywords.")

(defconst modern-sh-preprocessor-keywords '("source")
  "Bash preprocessor keywords.")

(defconst modern-sh-careful-keywords '("export")
  "Bash language careful keywords.")

(defconst modern-sh-builtin-keywords
  '("sh" "bash" "zsh" "csh" "ksh" "fish" "pwsh" ;
     ;; "cd" "tput" "setaf" "sgr0"
     "sudo" "exit" "passwd" "sleep" "kill" "read")
  "Bash language keywords.")

(defconst modern-sh-constants
  '("HOME" "EDITOR" "ED"                            ;
     "PATH" "MANPATH" "INFOPATH"                    ;
     "LIBRARY_PATH" "LD_LIBRARY_PATH" "LD_RUN_PATH" ;
     "PKG_CONFIG_PATH"                              ;
     "CMAKE_INCLUDE_PATH" "CMAKE_LIBRARY_PATH"      ;
     "C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH")
  "Common constants.")

(defconst modern-sh-operator-functions '("-eq" "-ne" "-gt" "-lt" "-ge" "-le")
  "Bash language operators functions.")

;; create the regex string for each class of keywords

(defconst modern-sh-keywords-regexp (regexp-opt modern-sh-keywords 'words)
  "Regular expression for matching keywords.")

(defconst modern-sh-declaration-keywords-regexp
  (regexp-opt modern-sh-declaration-keywords 'words)
  "Regular expression for matching declaration keywords.")

(defconst modern-sh-preprocessor-keywords-regexp
  (regexp-opt modern-sh-preprocessor-keywords 'words)
  "Regular expression for matching preprocessor keywords.")

(defconst modern-sh-careful-keywords-regexp
  (regexp-opt modern-sh-careful-keywords 'words)
  "Regular expression for matching careful keywords.")

(defconst modern-sh-builtin-keywords-regexp
  (regexp-opt modern-sh-builtin-keywords 'words)
  "Regular expression for matching builtin type.")

(defconst modern-sh-constant-regexp (regexp-opt modern-sh-constants 'words)
  "Regular expression for matching constants.")

(defconst modern-sh-operator-functions-regexp
  (regexp-opt modern-sh-operator-functions 'words)
  "Regular expression for matching operator functions.")

(defconst modern-sh-font-lock-keywords
  `(
     ;;
     ("\\(\\$\\*\\|\\$\\?\\)" . 'font-lock-warning-face)
     ("\\([*|`@#/?]+\\)" . 'font-lock-warning-face)

     ;; delimiter: path
     ("\\([/]\\)" . 'font-lock-keyword-face)

     ;; refs
     ("$\\([A-Za-z0-9_/?/*]+\\)" . 'font-lock-warning-face)
     ("${\\([A-Za-z0-9_]+\\)" 1 'font-lock-warning-face)

     ;; path
     ("/\\([.]*[A-Za-z0-9_-]+\\)" 1 'font-lock-string-face)
     ("\\([.]*[A-Za-z0-9_-]+\\)/" 1 'font-lock-string-face)

     ;; command options
     ("[+-]+\\([A-Za-z0-9_-]+\\)" . 'font-lock-builtin-face) ;font-lock-negation-char-face

     ;; builtin
     (,modern-sh-builtin-keywords-regexp . font-lock-warning-face)

     ;; careful
     (,modern-sh-careful-keywords-regexp . font-lock-warning-face)

     ;; declaration
     (,modern-sh-declaration-keywords-regexp . font-lock-preprocessor-face)

     ;; preprocessor
     (,modern-sh-preprocessor-keywords-regexp . font-lock-preprocessor-face)

     ;; type
     ;; ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)

     ;; function
     ("\\(?:function\s+\\)*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*(" 1
       'font-lock-function-name-face)

     ;; operator function
     (,modern-sh-operator-functions-regexp . font-lock-builtin-face)

     ;; constants reference
     (,modern-sh-constant-regexp . font-lock-constant-face)

     ;; keyword
     (,modern-sh-keywords-regexp . font-lock-keyword-face)

     ;; command
     ("^[ \t]*\\([A-Za-z_][A-Za-z0-9_-]+\\)[ \t]*" 1
       'font-lock-function-name-face)

     ;; numeric literals
     ("[ \t=><([,;$+-/*//|]\\([0-9][0-9a-zA-Z_-]*\\)+" 1
       'font-lock-constant-face)

     ;; variable references
     ("\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 'font-lock-variable-name-face)

     ;; delimiter: modifier
     ("\\(->\\|=>\\|\\.>\\|:>\\|:=\\|\\.\\.\\)" 1 'font-lock-keyword-face)

     ;; delimiter: . , ; separate
     ("\\([,;]+\\)" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: operator symbols
     ("\\([%~=<>?!&$|`^+-/*///.]+\\)" 1 'font-lock-warning-face)
     ;; ("\\([]+\\)" 1 'font-lock-warning-face)

     ;; delimiter: = : separate
     ("[^%~^!=<>+-*/]\\([=:]\\)[^%~^!=<>+-*/]" 1
       'font-lock-comment-delimiter-face)

     ;; delimiter: brackets
     ("\\(\\[\\|\\]\\|[(){}]\\)" 1 'font-lock-comment-delimiter-face))
  "An alist mapping regexes to font-lock faces.")


(defun modern-sh-add-keywords
  (&optional
    mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (font-lock-add-keywords
    mode
    modern-sh-font-lock-keywords))

(defun modern-sh-remove-keywords
  (&optional
    mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (font-lock-remove-keywords mode modern-sh-font-lock-keywords))

;;;###autoload
(define-minor-mode modern-sh-mode "Minor mode for editing shell script."
  :init-value nil
  :lighter " modern-sh"
  :group 'modern-sh
  (if modern-sh-mode (modern-sh-add-keywords)
    (modern-sh-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
    (font-lock-flush)
    (when font-lock-mode (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'modern-sh)

;;; modern-sh.el ends here
