;;; package --- Major mode for JAL(Just Another Language)
;;;
;;; Commentary:
;;;
;;; JAL(Just Another Language) is an open source programming language to progmram
;;; Microchip PIC controllers. (see URL `http://justanotherlanguage.org/')
;;;
;;; This major mode supports jalv2 compiler.

;;; Example settings:
;;;
;;; (use-package jal-mode
;;;   :config
;;;   (setq tab-width 4)                          ;; as you like
;;;   (setq jal-mode-compiler-path "/SOMEWHERE/jalv2-XXXX")
;;;   (setq jal-mode-lib-path "/SOMEWHERE/jallib-XXXX/lib")
;;;
;;;   ;; Enable flymake
;;;   (setq temporary-file-directory "~/tmp")     ;; as you like
;;;   (add-hook 'jal-mode-hook #'flymake-mode)
;;;
;;;   ;; or Enable flycheck
;;;   (use-package flycheck-jal
;;;     :config
;;;     (add-hook 'jal-mode-hook #'flycheck-mode))
;;; )
;;;
;;; TODOs:
;;;   support case insesitive --> (setq case-fold-search t)
;;;   jal-mode-compile --> M-x compile

;;; Code:

(require 'flymake)

(require 'jal-smie)       ;; wip

(defcustom jal-mode-compiler-path ""
  "Path string to Jal compiler (jalv2).
This variable is referred when compiling and using `flymake'.
But `flycheck' does NOT refer this variable because of its structure.

If you would like to use your favorite executable as a checker for
`flycheck', use command `flycheck-set-checker-executable' as following:

  M-x flycheck-set-checker-executable <RET> JAL
      <RET> C:/jallib-pack-bee-jalv25r6-20220522/compiler/jalv2_64.exe")

(defcustom jal-mode-lib-path ""
  "Path string to jallib (see URL `https://github.com/jallib/jallib').
This variable is referred when use of `compile', `flymake' and `flycheck'.
It should be used absolute path due to prevent \"include\" errors.")

(defvar jal-mode-compile-hook nil
  "Variable for callbacks when compiling done")

;;
;; Syntax highlight
;;

(defconst jal-mode-number-literal
  '(("\\<\\(?:\\(\\([_.0-9]+\\)\\|\\(\\(0b\\|0B\\)[_01]+\\)\\|\\(0x\\|0X\\)[_0-9A-Fa-f]+\\)\\)\\>" . font-lock-constant-face))
  "Number literal patterns for font-lock.
e.g. \"400_000_000\", \"0xA5\", \"0b01010_0101\"")

(require 'jallib-consts)            ;; jal-mode-constants (generated)
(require 'jallib-funcs-procs)       ;; jal-mode-builtins (generated)

(defvar jal-mode-preprocessors
  '("pragma")
  "JAL's pragma and others from jalv2.pdf")

(defvar jal-mode-types
  '("bit"
    "sbit"
    "byte"
    "sbyte"
    "word"
    "sword"
    "dword"
    "sdword"
    "float")
  "JAL's types from jalv2.pdf")

(defvar jal-mode-keywords
  '(;; declarations
    "end"
    "record"
    "var"
    "const"
    "alias"
    "local"

    "is"
    "at"
    "of"

    ;; flow controls
    "block"
    "case"
    "otherwise"
    "for"
    "forever"
    "loop"
    "using"
    "if"
    "then"
    "elsif"
    "else"
    "exit"
    "repeat"
    "until"
    "while"

    ;; sub-programs
    "procedure"
    "function"
    "return"
    "in"
    "out"
    "put"
    "get"
    "task"
    "suspend"

    ;; asm
    "asm"
    "assmbler"
    "bank"
    "page"
    "db"
    "dw"
    "ds"

    ;; others
    "assert"
    "include"
    "_DEBUG"
    "_ERROR"
    "_WARN"

    ;; pragma
    "target"
    "bootloader"
    "clear"
    "eedata"
    "fuses"
    "iddata"
    "task"
    "error"
    "name"
    "size"
    "speed"
    "frame"
    "inline"
    "interrupt"
    "jump_table"
    "keep"
    "nostack"
    "opt"
    "warn"
    "code"
    "data"
    "shared"
    "eeprom"
    "fuse_def"
    "id"
    "stack"
    "debug")
  "JAL's keywards from jalv2.pdf")

(defvar jal-mode-font-lock-defaults
  `((( "\"\\.\\*\\?"                                  . font-lock-string-face)
     ( ,(regexp-opt jal-mode-constants      'words)   . font-lock-constant-face)
     ( ,(regexp-opt jal-mode-preprocessors  'symbols) . font-lock-preprocessor-face)
     ( ,(regexp-opt jal-mode-builtins       'symbols) . font-lock-builtin-face)
     ( ,(regexp-opt jal-mode-keywords       'symbols) . font-lock-keyword-face)
     ( ,(regexp-opt jal-mode-types          'words)   . font-lock-type-face)
     ) nil t)
  "Font lock setting in JAL mode.")

(defvar jal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Derived from pascal-mode
    (modify-syntax-entry ?\\ "."   st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    ;; ;; This used to use comment-syntax `b'.  But the only document I could
    ;; ;; find about the syntax of Pascal's comments said that (* ... } is
    ;; ;; a valid comment, just as { ... *) or (* ... *) or { ... }.
    ;; (modify-syntax-entry ?* ". 23" st)
    ;; ;; Allow //...\n comments as accepted by Free Pascal (bug#13585).
    ;; (modify-syntax-entry ?/ ". 12c" st)
    ;; (modify-syntax-entry ?\n "> c" st)
    ;; (modify-syntax-entry ?{ "<"    st)
    ;; (modify-syntax-entry ?} ">"    st)
    (modify-syntax-entry ?+ "."    st)
    (modify-syntax-entry ?- "."    st)
    (modify-syntax-entry ?= "."    st)
    (modify-syntax-entry ?% "."    st)
    (modify-syntax-entry ?< "."    st)
    (modify-syntax-entry ?> "."    st)
    (modify-syntax-entry ?& "."    st)
    (modify-syntax-entry ?| "."    st)
    (modify-syntax-entry ?_ "_"    st)
    ;; (modify-syntax-entry ?\' "\""  st)

    ;; jal-mode specific
    (modify-syntax-entry ?\; "<"   st)   ;; comment start by ;
    (modify-syntax-entry ?- ". 12" st)   ;; comment start by --
    (modify-syntax-entry ?\n ">"   st)   ;; comment end by \n
    st)
  "Syntax table in use in JAL mode buffers.")

;;
;; Keybindings
;;

(defvar jal-mode-map
  (let ((map (make-sparse-keymap)))
    ;; jal-mode specific
    (define-key map "\C-c\C-c" #'jal-mode-compile)
    (define-key map "\C-c\C-p" 'flymake-goto-prev-error)
    (define-key map "\C-c\C-n" 'flymake-goto-next-error)

    ;; Derived from pascal-mode, wip
    ;; (define-key map "\M-\t"    'completion-at-point)
    ;; (define-key map "\M-?"     'completion-help-at-point)
    ;; (define-key map "\177"     'backward-delete-char-untabify)
    ;; (define-key map "\M-\C-a"  'pascal-beg-of-defun)
    ;; (define-key map "\M-\C-e"  'pascal-end-of-defun)
    ;; (define-key map "\C-c\C-d" 'pascal-goto-defun)
    ;; (define-key map "\C-c\C-o" 'pascal-outline-mode)
    ;; A command to change the whole buffer won't be used terribly
    ;; often, so no need for a key binding.
    ;; (define-key map "\C-cd"    'pascal-downcase-keywords)
    ;; (define-key map "\C-cu"    'pascal-upcase-keywords)
    ;; (define-key map "\C-cc"    'pascal-capitalize-keywords)
    map)
  "Keymap in use in JAL mode buffers.")

;;
;; Major mode definition
;;

;;;###autoload
(define-derived-mode jal-mode prog-mode "JAL"
  "JAL mode is a major mode for editing JAL source files.
About JAL, see URL `http://justanotherlanguage.org/'"

  ;; font-lock
  (setq font-lock-defaults jal-mode-font-lock-defaults)
  (font-lock-add-keywords 'jal-mode jal-mode-number-literal)

  ;; sytax for comments
  (setq comment-start "--")
  (setq comment-start-skip "--+\\s-*")
  (setq comment-end "")
  (setq comment-multi-line nil)

  ;; indentation
  (smie-setup jal-smie-grammar #'jal-smie-rules
              :forward-token  #'jal-smie-forward-token
              :backward-token #'jal-smie-backward-token)

  ;; flymake settings
  ;; Error message from jalv2 is something like 'test.jal:7: "x" not defined'
  ;;
  (push '("\\.jal$" jal-mode-flymake-init) flymake-proc-allowed-file-name-masks)
  (push '("^\\([^: ]+\\):\\([0-9]+\\): \\(.+\\)"
          1 2 nil 3) flymake-proc-err-line-patterns)
  ;;    1         2          nil                 3
  ;; file-idx line-idx col-idx(optional) text-idx(optional) match-end to end of string is error text
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jal\\'" . jal-mode))

;;
;; compile
;;

;;;###autoload
(defun jal-mode-compile ()
  "Compile jal sorce file."
  (interactive)
  (unless buffer-file-name
    (error "This buffer does not have a file name"))
  (when (string-empty-p jal-mode-compiler-path)
    (error "Error! Need to set \"jal-mode-compiler-path\""))
  (unless (file-exists-p jal-mode-compiler-path)
    (error "JAL compiler not found: %s" jal-mode-compiler-path))
  (unless (file-executable-p jal-mode-compiler-path)
    (error "JAL compiler is not executable: %s" jal-mode-compiler-path))

  (let* ((exe (expand-file-name jal-mode-compiler-path))
         (lib (expand-file-name jal-mode-lib-path)))
    (compile (format "%s %s -s %s"
                     exe (buffer-file-name) lib)))
  (run-hooks 'jal-mode-compile-hook))

;;
;; flymake
;;

;;;###autoload
(defun jal-mode-flymake-init ()
  (unless buffer-file-name
    (error "This buffer does not have a file name"))
  (when (string-empty-p jal-mode-compiler-path)
    (error "Error! Need to set \"jal-mode-compiler-path\""))
  (unless (file-exists-p jal-mode-compiler-path)
    (error "JAL compiler not found: %s" jal-mode-compiler-path))
  (unless (file-executable-p jal-mode-compiler-path)
    (error "JAL compiler is not executable: %s" jal-mode-compiler-path))

  (let* ((exe (expand-file-name jal-mode-compiler-path))
         (lib (expand-file-name jal-mode-lib-path))
         (temp-file   (flymake-proc-init-create-temp-buffer-copy 'flymake-create-temp-with-folder-structure))
         (local-dir   (file-name-directory buffer-file-name))
         ;; (local-file  (file-relative-name temp-file local-dir))
         (local-file  temp-file)
         (log-file    (concat (file-name-base local-file) ".log")))
    (list exe (list local-file
                    ;; "-no-asm" "-no-codfile" "-no-hex" "-no-lst" "-no-log" ;; NG
                    "-no-asm" "-no-codfile" "-no-lst" "-no-hex"
                    "-no-codegen" "-no-debug" "-no-pcode" "-Wall"
                    "-s" lib))))

(provide 'jal-mode)
;;; jal-mode.el ends here
