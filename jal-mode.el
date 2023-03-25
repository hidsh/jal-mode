;;; package --- Summary: Major mode for JAL(Just Another Language)
;;;
;;; Commentary:
;;;
;;; created at : 06 Mar 2023 11:27:11 JST
;;; author     : hidsh
;;;
;;; This major mode is for editing JAL.
;;; JAL(Just Another Language) is a programming language to program Microchip PIC controllers.
;;; This major mode will support jalv2 compiler.
;;; http://justanotherlanguage.org/
;;;
;;; Example settings:
;;;
;;; (use-package jal-mode
;;;   :config
;;;   (setq jal-mode-compiler "/SOMEWHERE/jalv2-XXXX")
;;;   (setq jal-mode-lib-path "/SOMEWHERE/jallib-XXXX"))
;;;
;;; Code:

(defvar jal-mode-ext "\\.jal\\'"
  "Extension of jal source filename.")

(defvar jal-mode-number-literal
  '(("\\<\\(?:\\(\\([_.0-9]+\\)\\|\\(\\(0b\\|0B\\)[_01]+\\)\\|\\(0x\\|0X\\)[_0-9A-Fa-f]+\\)\\)\\>" . font-lock-constant-face))
  "For font-lock number literal e.g. \"400_000_000\", \"0xA5\", \"0b01010_0101\"."
  )

(require 'jallib-consts)            ;; jal-mode-constants
(require 'jallib-funcs-procs)       ;; jal-mode-builtins

(setq jal-mode-preprocessors
    '("pragma"
      "is"
      "at"
      "of"
      ))

(setq jal-mode-types
    '(;; from jalv2.pdf
      "bit"
      "sbit"
      "byte"
      "sbyte"
      "word"
      "sword"
      "dword"
      "sdword"
      "float"
      ))

(setq jal-mode-keywords
    '(;; from jalv2.pdf
      ;; declarations
      "end"
      "record"
      "var"
      "const"
      "alias"
      "local"

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
      "debug"
      ))

;; I'd probably put in a default that you want, as opposed to nil
(defvar jal-mode-tab-width nil "Width of a tab for JAL mode")

;; Two small edits.
;; First is to put an extra set of parens () around the list
;; which is the format that font-lock-defaults wants
;; Second, you used ' (quote) at the outermost level where you wanted ` (backquote)
;; you were very close

(defvar jal-mode-font-lock-defaults
  `((
     ;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     ( ,(regexp-opt jal-mode-constants      'words)   . font-lock-constant-face)
     ( ,(regexp-opt jal-mode-preprocessors  'symbols) . font-lock-preprocessor-face)
     ( ,(regexp-opt jal-mode-builtins       'symbols) . font-lock-builtin-face)
     ( ,(regexp-opt jal-mode-keywords       'symbols) . font-lock-keyword-face)
     ( ,(regexp-opt jal-mode-types          'symbols) . font-lock-type-face)
     )))

;;;###autoload
(define-derived-mode jal-mode prog-mode "JAL"
  "JAL mode is a major mode for editing JAL source files."
  ;; you again used quote when you had '((jal-hilite))
  ;; I just updated the variable to have the proper nesting (as noted above)
  ;; and use the value directly here
  (setq font-lock-defaults jal-mode-font-lock-defaults)
  (font-lock-add-keywords 'jal-mode jal-mode-number-literal)


  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when jal-mode-tab-width
    (setq tab-width jal-mode-tab-width))

  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  (setq comment-start "--")
  (setq comment-start-skip "--+\\s-*")
  (setq comment-end "")
  (setq comment-multi-line nil)

  (modify-syntax-entry ?\; "<" jal-mode-syntax-table)   ;; comment start by ;
  (modify-syntax-entry ?- ". 12" jal-mode-syntax-table) ;; comment start by --
  (modify-syntax-entry ?\n ">" jal-mode-syntax-table)   ;; comment end by \n

  ;; Note that there's no need to manually call `jal-mode-hook'; `define-derived-mode'
  ;; will define `jal-mode' to call it properly right before it exits




  )

;;;###autoload
(add-to-list 'auto-mode-alist (cons jal-mode-ext 'jal-mode) t)


(provide 'jal-mode)
;;; jal-mode.el ends here
