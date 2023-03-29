;;; flycheck-jal.el --- Flycheck: JAL support -*- lexical-binding: t; -*-
;;;
;;; This Flycheck extension provides JAL(Just Another Language) syntax checker
;;; which uses jalv2 compiler.
;;;
;;; Commentary:
;;;
;;; If you use your favorite ckecker executable for `flycheck',
;;; use command `flycheck-set-checker-executable' as following:
;;;
;;;   M-x flycheck-set-checker-executable <RET> JAL
;;;       <RET> C:/jallib-pack-bee-jalv25r6-20220522/compiler/jalv2_64.exe")

;;; Code:

(require 'flycheck)
(require 'jal-mode)

(flycheck-define-checker jal
  "A JAL syntax checker using jalv2 compiler in jal-mode."
  :command ("jalv2-osx" source (option "-s" jal-mode-lib-path))
  :error-patterns
   ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes jal-mode)

(add-to-list 'flycheck-checkers 'jal)

(provide 'flycheck-jal)
;;; flycheck-jal.el ends here
