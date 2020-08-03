;;; modern-sh.el --- Minor mode for editing shell script  -*- lexical-binding: t -*-
;;
;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; URL: https://github.com/damon-kwok/modern-sh
;; Keywords: languages programming
;; Package-Requires: ((emacs "24.3") (hydra "0.15.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2020 Damon Kwok
;;
;;; Commentary:
;;
;; Description:
;;
;; An Emacs minor mode for editing shell script.
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

(require 'cl-lib)
(require 'xref)
(require 'hydra)
(require 'imenu)

(defvar modern-sh-mode-hook nil)

(defvar modern-sh-mode-map (let ((map (make-keymap))) map)
  "Keymap for Modern shell minor mode.")

(defconst modern-sh-keywords
  '("let" "local"                       ;
     "if" "else" "elif" "then" "fi"     ;
     "case" "esac" "for" "in"           ;
     "while" "until" "do" "done")
  "Modern shell keywords.")

(defconst modern-sh-declaration-keywords '("function" "declare")
  "Modern shell declaration keywords.")

(defconst modern-sh-preprocessor-keywords
  '("source" "sh" "bash" "zsh" "csh" "ksh" "fish" "pwsh")
  "Modern shell preprocessor keywords.")

(defconst modern-sh-careful-keywords
  ;;
  '("export" "eval" "set" "unset"       ;
     "return" "break" "continue"        ;
     "shift" "pushd" "popd")
  "Modern shell language careful keywords.")

(defconst modern-sh-builtin-keywords
  '("su" "sudo" "chroot" "exit" "rm"    ;
     "kill" "pkill" "skill" "killall"   ;
     "passwd" "chmod" "sleep" "read")
  "Modern shell language keywords.")

(defconst modern-sh-constants
  '("true" "false"                                  ;
     "HOME" "EDITOR" "ED"                           ;
     "PATH" "MANPATH" "INFOPATH"                    ;
     "LIBRARY_PATH" "LD_LIBRARY_PATH" "LD_RUN_PATH" ;
     "PKG_CONFIG_PATH"                              ;
     "CMAKE_INCLUDE_PATH" "CMAKE_LIBRARY_PATH"      ;
     "C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH")
  "Common constants.")

(defconst modern-sh-operator-functions '("-eq" "-ne" "-gt" "-lt" "-ge" "-le")
  "Modern shell language operators functions.")

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
     ;; careful
     (,modern-sh-careful-keywords-regexp . font-lock-warning-face)

     ;; command options
     ;; ("^[ \t]*\\([A-Za-z0-9_-]+\\)[ \t]+\\([+-]+[A-Za-z0-9_-]*\\)[= \t]*" 2 'font-lock-builtin-face)
     ("[ \t:|]\\([+-]+[A-Za-z0-9_-]*\\)[ \t=]*" 1 'font-lock-builtin-face)
     ("|\\([.]*[A-Za-z0-9_-]+\\)" 1 'font-lock-builtin-face)
     ("\\([.]*[A-Za-z0-9_-]+\\)|" 1 'font-lock-builtin-face)
     ("^[ \t]*\\([.]*[A-Za-z0-9_-]+\\)[ \t]*)" 1 'font-lock-builtin-face)

     ;; env variable
     ("[$&]\\([A-Za-z0-9_-]+\\)" 1 'font-lock-warning-face)
     ("${\\([A-Za-z0-9_-]+\\)" 1 'font-lock-warning-face)

     ;; delimiter: path
     ("\\(/\\)" . 'font-lock-doc-face)

     ;; path
     ("/\\([A-Za-z0-9_.-]*\\)" 1 'font-lock-negation-char-face)
     ("\\([A-Za-z0-9_.-]*\\)/" 1 'font-lock-negation-char-face)

     ;;
     ("\\(++\\|--\\|\\.\\.\\.\\|\\^\\*\\|\\*\\^|\\|\\$\\*\\|\\$\\?\\)" .
       'font-lock-warning-face)
     ("\\([*|`@#?]+\\)" . 'font-lock-warning-face)

     ;; variable define
     ("\\([A-Za-z_.-][A-Za-z0-9_.-]*\\)[ \t]*[=[]" 1
       'font-lock-variable-name-face)

     ;; builtin
     (,modern-sh-builtin-keywords-regexp . font-lock-warning-face)

     ;; declaration
     (,modern-sh-declaration-keywords-regexp . font-lock-preprocessor-face)

     ;; preprocessor
     (,modern-sh-preprocessor-keywords-regexp . font-lock-preprocessor-face)

     ;; keyword
     (,modern-sh-keywords-regexp . font-lock-keyword-face)

     ;; operator function
     (,modern-sh-operator-functions-regexp . font-lock-builtin-face)

     ;; constants reference
     (,modern-sh-constant-regexp . font-lock-constant-face)

     ;; function
     ("\\(?:function\s+\\)*\\([A-Za-z_.-][A-Za-z0-9_.-]*\\)[ \t]*(" 1
       'font-lock-function-name-face)

     ;; command
     ("^[ \t]*\\([A-Za-z_.-]+[A-Za-z0-9_.-]+\\)[ \t]*" 1
       'font-lock-function-name-face)

     ;; variable refs
     ("\\([A-Za-z_.-][A-Za-z0-9_.-]*\\)" . 'font-lock-variable-name-face)

     ;; numeric literals
     ("[ \t=><([,;$+-*|]\\([0-9][0-9a-zA-Z_.-]*\\)+" 1 'font-lock-constant-face)

     ;; type
     ;; ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)

     ;; delimiter: modifier
     ("\\(->\\|=>\\|\\.>\\|=:\\|\\.\\.\\)" 1 'font-lock-keyword-face)

     ;; delimiter: . , ; separate
     ("\\([,;]+\\)" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: operator symbols
     ("\\([%~/?!&$|`/^/*//]+\\)" 1 'font-lock-warning-face)
     ("\\([=<>]+\\)" 1 'font-lock-negation-char-face)

     ;; delimiter: = : separate
     ("[^%~^!=<>+-*/]\\([=:]\\)[^%~^!=<>+-*/]" 1
       'font-lock-comment-delimiter-face)

     ;; delimiter: brackets
     ("\\(\\[\\|\\]\\|[(){}]\\)" 1 'font-lock-comment-delimiter-face))
  "An alist mapping regexes to font-lock faces.")

(defun modern-sh-project-root-p (path)
  "Return t if directory `PATH' is the root of the Modern shell project."
  (let* ((files '("CMakeLists.txt" "make.bat" "Makefile" ;
                   "Dockerfile" ".editorconfig" ".gitignore"))
          (foundp nil))
    (while (and (> (length files) 0)
             (not foundp))
      (let* ((filename (car files))
              (filepath (concat (file-name-as-directory path) filename)))
        (setq files (cdr files))
        (setq foundp (file-exists-p filepath)))) ;
    foundp))

(defun modern-sh-project-root
  (&optional
    path)
  "Return the root of the Modern shell project.
Optional argument PATH: project path."
  (let* ((bufdir (if buffer-file-name   ;
                   (file-name-directory buffer-file-name) default-directory))
          (curdir (if path (file-name-as-directory path) bufdir))
          (parent (file-name-directory (directory-file-name curdir))))
    (if (or (not parent)
          (string= parent curdir)
          (string= parent "/")
          (modern-sh-project-root-p curdir)) ;
      curdir                                 ;
      (modern-sh-project-root parent))))

(defun modern-sh-project-name ()
  "Return Modern shell project name."
  (file-name-base (directory-file-name (modern-sh-project-root))))

(defun modern-sh-project-file-exists-p (filename)
  "Return t if file `FILENAME' exists."
  (file-exists-p (concat (modern-sh-project-root) filename)))

(defun modern-sh-run-command (command &optional path)
  "Return `COMMAND' in the root of the Modern shell project.
Optional argument PATH: project path."
  (setq default-directory (if path path (modern-sh-project-root path)))
  (compile command))

(defun modern-sh-project-build ()
  "Build project."
  (interactive)
  (if (modern-sh-project-file-exists-p "Makefile")
    (modern-sh-run-command "make")))

(defun modern-sh-project-open ()
  "Open `Makefile' file."
  (interactive)
  (if (modern-sh-project-file-exists-p "Makefile")
    (find-file (concat (modern-sh-project-root) "Makefile"))))

(defun modern-sh-buffer-dirname ()
  "Return current buffer directory file name."
  (directory-file-name (if buffer-file-name (file-name-directory
                                              buffer-file-name)
                         default-directory)))

(defun modern-sh-project-run ()
  "Run project."
  (interactive)
  (let* ((bin1 (concat (modern-sh-project-root) "bin/"
                 (modern-sh-project-name)))
          (bin2 (concat (modern-sh-project-root) "/" (modern-sh-project-name)))
          (bin3 (concat (modern-sh-buffer-dirname) "/"
                  (modern-sh-project-name))))
    (if (file-exists-p bin1)
      (modern-sh-run-command bin1)
      (if (file-exists-p bin2)
        (modern-sh-run-command bin2)
        (if (file-exists-p bin3)
          (modern-sh-run-command bin3))))))

(defun modern-sh-banner-default ()
  "Modern shell banner."
  "
       _                   _       _
      | |                 (_)     | |
   ___| |__  ___  ___ _ __ _ _ __ | |_
  / __| '_ \\/ __|/ __| '__| | '_ \\| __|
  \\__ \\ | | \\__ \\ (__| |  | | |_) | |_
  |___/_| |_|___/\\___|_|  |_| .__/ \\__|
                            | |
                            |_|
")

(defhydra modern-sh-hydra-menu
  (:color blue
    :hint none)
  "
%s(modern-sh-banner-default)
  Project     |  _b_: Build     _r_: Run
  _q_: Quit"                            ;
  ("b" modern-sh-project-build "Build")
  ("r" modern-sh-project-run "Run")
  ("q" nil "Quit"))

(defun modern-sh-menu ()
  "Open Modern shell hydra menu."
  (interactive)
  (modern-sh-hydra-menu/body))

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

(defun modern-sh-build-tags ()
  "Build tags for current project."
  (interactive)
  (let ((tags-buffer (get-buffer "TAGS"))
         (tags-buffer2 (get-buffer (format "TAGS<%s>"
                                     (modern-sh-project-name)))))
    (if tags-buffer ;;
      (kill-buffer tags-buffer))
    (if tags-buffer2 ;;
      (kill-buffer tags-buffer2)))
  (let ((ctags-params                   ;
          (concat "ctags -e -R . ")))
    (setq default-directory (modern-sh-project-root))
    (message "ctags:%s" (shell-command-to-string ctags-params))
    (modern-sh-load-tags)))

(defun modern-sh-load-tags
  (&optional
    build)
  "Visit tags table.
Optional argument BUILD If the tags file does not exist, execute the build."
  (interactive)
  (let* ((tags-file (concat (modern-sh-project-root) "TAGS")))
    (if (file-exists-p tags-file)
      (progn (visit-tags-table (concat (modern-sh-project-root) "TAGS")))
      (if build (modern-sh-build-tags)))))

(defun modern-sh-after-save-hook ()
  "After save hook."
  (when (eq major-mode 'sh-mode)
    (indent-region (point-min)
      (point-max))
    (if (not (executable-find "ctags"))
      (message "Could not locate executable '%s'" "ctags")
      (modern-sh-build-tags))))

;;;###autoload
(define-minor-mode modern-sh-mode ;;
  "Minor mode for editing shell script."
  :init-value nil
  :lighter " modern-sh"
  :group 'modern-sh
  ;;
  (setq-local imenu-generic-expression ;;
    '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
       ("function"
         "^\\(function[ \t]*\\)?\\([a-z0-9_]+\\)[ \t]*\\((.*)\\)[ \t{]*" 2)))
  ;;
  (if modern-sh-mode ;;
    (progn           ;
      (modern-sh-add-keywords)
      (imenu-add-to-menubar "Index")
      (add-hook 'after-save-hook #'modern-sh-after-save-hook nil t)
      (modern-sh-load-tags))
    (progn                              ;
      (modern-sh-remove-keywords)
      (imenu--cleanup)
      (remove-hook 'after-save-hook #'modern-sh-after-save-hook)))
  ;;
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
    (font-lock-flush)
    (when font-lock-mode ;;
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'modern-sh)

;;; modern-sh.el ends here
