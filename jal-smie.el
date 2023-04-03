;;; jal-mode-smie.el --- SMIE-based indentation for JAL mode.
;;;
;;; Commentary:
;;;
;;; Code:

(require 'smie)

(defconst jal-smie-end-alist
  '( ;; an ender-token       beginner-token candidates
    (("end"   . "block")     . ("block"))       ;; block .. end block
    (("end"   . "case")      . ("of"))          ;; case .. end case
    (("end"   . "loop")      . ("loop"))        ;; for xx loop .. end loop,
                                                ;; forever loop .. end loop,
                                                ;; while xx loop .. end loop
    (("end"   . "if")        . ("then" "else")) ;; if xx then .. end if
                                                ;; elsif xx then .. end if
                                                ;; else ..end if
    (("until" . nil)         . ("repeat"))      ;; repeat .. until xx
    (("end"   . "record")    . ("is"))          ;; record xx is .. end record
    (("end"   . "procedure") . ("is"))          ;; procedure xx is .. end procedure
    (("end"   . "function")  . ("is"))          ;; function xx is end function
    (("end"   . "task")      . ("task"))        ;; task .. end task
    (("end"   . "assembler") . ("assembler")))  ;; assembler .. end assembler
  "Alist defines pairs of an ender-token and beginner-tokens for syntax block.
such as \"end block\" as an ender-token and \"block\" as a beginner-token in JAL syntax")

(defvar jal-smie--defuns
  '("procedure" "function"))

(defvar jal-smie--types
  '("bit"
    "sbit"
    "byte"
    "sbyte"
    "word"
    "sword"
    "dword"
    "sdword"
    "float"))

(defconst jal-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '( (id)
        (any)
        (stmt (exp))
        (exp (id) (exp) (virtual-indents))
        ;; (var   ("var"   decl-body))
        ;; (const ("const" decl-body))
        ;; (decl-body (decl-body)
        ;; (exp "=" exp decl-body))
        ;; (end-marker ("end"))
        ;; (blocker (any))
        (inst
         ;; ("block" stmt "end" blocker)
         ("block" insts "end" any)
         ("case" exp "of" stmt "end" any)
         ("if" exp "then" stmt "elsif" exp "then" stmt "else" stmt "end" any)
         ;; ("if" exp "elif" exp "else" ":" stmt)
         ;; ("then" insts "end" any)
         ;; ("record"    id "is" stmt "end record")
         ;; ("procedure" id "is" stmt "end procedure")
         ;; ("begin" insts "end")
         ;; ("block" insts "end block")
         ;; ("forever" "loop" insts "end loop")
         ;; ("for" exp "loop" insts "end loop")
         ;; ("for" exp "using" id "loop" insts "end loop")
         ;; ("if" exp "then" insts "end if")
         ;; ("if" exp "then" insts "else" insts "end if")
         ;; ("if" exp "then" insts "elsif" exps "then" insts "else" insts "end if")
         ;; ("repeat" insts "until" exp)
         ;; ("while" exp "loop" insts "end loop")
         ;; ("while" exp "loop" insts "exit loop" insts "end loop")
         ;; ("procedure" id exp "is" insts "end procedure")
         ;; ("fucntion" id exp "return" id "is" insts "end function")
         ;; ("task" id exp "is" insts "end task")
         ;; ("assembler" insts "end assembler")
         ;; ;; (id ":=" exp)
         ;; (id "=" exp)
         ;; (exp)

          )
        ;; ;; (insts (insts ";" insts) (inst))
        (insts (insts "\n" insts) (inst))

        (exp (exp "+" exp)
        ;;      (exp "-" exp)
        ;;      (exp "*" exp)
        ;;      (exp "/" exp)
        ;;      (exp "%" exp)     ;; mod
        ;;      (exp "<<" exp)    ;; shift left
        ;;      (exp ">>" exp)    ;; shift right
        ;;      (exp "<" exp)     ;; less than
        ;;      (exp "<=" exp)    ;; less than or equal
        ;;      (exp ">" exp)     ;; greater than
        ;;      (exp ">=" exp)    ;; greater than or equal
             (exp "==" exp)    ;; qual
        ;;      (exp "!=" exp)    ;; not qual
        ;;      (exp "&" exp)     ;; binary and
        ;;      (exp "|" exp)     ;; binary or
        ;;      (exp "^" exp)     ;; binary xor
        ;;      ("!" exp)         ;; 1's complement
        ;;      ("!!" exp)        ;; !!0 --> 0, !!x --> 1
        ;;      ;; ("+" exp)         ;; 1's complement
        ;;      ;; ("-" exp)         ;; 1's complement
             ("(" exps ")"))

        (exps (exps "\n" exps)
              (exp))
        )
     '(  (assoc "\n"))
     '(  (assoc "&" "|" "^" )
         (assoc "<<" ">>")
         (assoc "==" "!=" "<=" ">=" "<" ">" )
         (assoc "+" "-")
         (assoc "*" "/" "%")
         (assoc "!" "!!"))
     '((nonassoc "block" "case" "if" "then" "elsif" ))

     ))))

(defun jal-smie-rules (kind token)
  "Nim-mode’s indent rules.
See also ‘smie-rules-function’ about KIND and TOKEN."
  (message "kind=\'%s\'  token=\'%s\'" kind token)
  (pcase (cons kind token)
    ;; Blocks
    (`(:before . ,(or "block" "if"))
     ;; (message "token: %s" token)
     (jal-smie-indent-keep1))

    (`(:list-intro . ,(or "block" "then" "else"))
     ;; (message "token: %s" token)
     (pcase token
       ("block"
        (jal-smie-indent-<<))
       (_
        (cons 'column nil))
       ))

    (`(:after . ,(or "block" "if"))
     (message "after- token: %s" token)
     (let ((next-tok (smie-default-backward-token)))
       (message "next-tok %s" next-tok)
       (pcase next-tok
         ("end" (jal-smie-indent-keep2))  ;; "end block" --> keep indent
         (_ (jal-smie-indent->>))         ;; ""block"    --> indent++
         )))


    ('(:after . "then")
     ;; (message "token: %s" token)
     (jal-smie-indent->>))

    ;; ('(:list-intro . "then")
    ;;  ;; (message "token: %s" token)
    ;;  (cons 'column 0))

    ;; (`(:before . "then")
    ;;  ;; (message "token: %s" token)
    ;;  (jal-smie-indent-keep1))
    ;;  ;; (cons 'column 0))

    (`(:before . "end")
     (message "before- token: %s" token)
     (let* ((next-tok (save-excursion (smie-default-forward-token)
                                      (smie-default-forward-token)))
            (parent (smie-indent--parent))
            (beg-cands (cdr (assoc (cons token next-tok) jal-smie-end-alist))))
       (message "next-tok=%S, parent=%S, begs=%S" next-tok parent beg-cands)
       (if (and (string= (third parent) next-tok)
                (cdr (assoc (cons token next-tok) jal-smie-end-alist)))
           (cons 'column (save-excursion (goto-char (second parent))
                                         (message "indent %s" (current-indentation))
                                         (current-indentation)))
         (message "Parent not found")
         (let (tok)
           (save-excursion
             (while (and (not (string= tok "")) (not (member tok beg-cands)))
                  (setq tok (smie-default-backward-token))
                  (message "token \'%s\' at %d" tok (point)))
                (if (not (string= tok ""))
                    (cons 'column (current-indentation))
                  (progn
                    (error "Beginner token Not found for: %S" tok)
                     0))))
         )))

     ;; (message "token: %s" token)
     ;; (jal-smie-indent-<<))
     ;; (cons 'column 0))
     ;; (jal-smie-end-block))

    (`(:after . "end")
     (message "token--: %s" token)
     ;; (jal-smie-indent-<<))
     ;; (cons 'column 0))
     (let ((parent (smie-indent--parent)))
       (message "parent %s" parent)
       (smie-close-block)
       (if (car parent)
           (* (second parent) tab-width )
         0)))


    ('(:list-intro . "end")
     (message "token: %s" token)
     ;; (jal-smie-indent-<< 0))
     (cons 'column 0))

    ;; ;; Conditions
    ;; (`(:list-intro . ,(or "if" "elif" "else"
    ;;                       "while"))
    ;;  (jal-smie--list-intro-conditions))

    ;; ;; Colon
    ;; (`(:list-intro . ,(or "block" "case" "loop" "is" ))
    ;;  (message "%d" (current-indentation))
    ;;  (cons 'column (current-indentation)))
    ;; ;; (cons 'column (+ (current-indentation) nim-indent-offset)))

    ;; ;; ‘empty-line-token’
    ;; (empty-line-token
    ;;  ;; This has to return token; not indent number.
    ;;  ;; see ‘smie-indent-keyword’.
    ;;  (jal-smie-indent-keep1))

    ;; todo
    (_ nil)))

(defun jal-smie-forward-token ()
  (let ((pos (point)))
    (skip-chars-forward " \t")
    (forward-comment (point-max))
    (let ((tok (smie-default-forward-token)))
      (cond
       ((looking-at "\\s\"") "")                    ;A string.
       ;; ((and (equal tok "") (looking-at "\\\\\n"))
       ;;  (goto-char (match-end 0)) (jal-smie-forward-token))
       ;; TODO
       (t
        (cond
         ;; TODO
         (t tok)))))))

(defun jal-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (let ((tok (smie-default-backward-token)))
      (cond
       ;; ((and (equal tok "") (eq ?\\ (char-before)) (looking-at "\n"))
       ;;  (forward-char -1) (jal-smie-backward-token))
       ;; TODO
       (t
        (cond
         ;; TODO
         (t tok)))))))

;; ---------
(defun jal-smie-end-block ()
  (message "jal-smie-end-block")
  (cond ((smie-rule-next-p "block")
         (message "end block")
         (smie-indent-close))
        ((smie-rule-next-p "if")
         (message "end if")
         (smie-indent-close)
         (let ((parent (smie-indent--parent)))
           (if (car parent)
               (cons 'column (* (second parent) tab-width))
             nil)))
        (t
         (message "end ??")
         ;; (smie-indent-close)
         (message "%s" (smie-indent--parent))
         ;; (smie-indent--parent)
         (jal-smie-indent->> 0)
         )))

(defun jal-smie-indent-keep2 ()
  (cons 'column (current-indentation)))

(defun jal-smie-indent-keep1 ()
  (let ((parent (smie-indent--parent)))
    (message "jal-smie-indent-keep1 parent %s" parent)
    (if (car parent)
        (let ((prev-indent
               (save-excursion
                 (goto-char (second parent))
                 (current-indentation))))
          (cons 'column prev-indent))
      nil)))

(defun jal-smie-indent-set (col)
    (message "jal-smie-indent-set col %s" col)
    (cons 'column (max 0 col)))

(defun jal-smie-indent->> (&optional n)
  (unless n
    (setq n 1))
  (let ((col (+ (current-indentation) (*  tab-width n))))
    (message "jal-smie-indent->> %d col %s" (current-indentation) col)
    (cons 'column col)))

(defun jal-smie-indent-<< (&optional n)
  (unless n
    (setq n 1))
  (let ((col (* (- (current-indentation)  (* tab-width n)))))
    (message "jal-smie-indent-<< col %s" col)
    (cons 'column col)))

;; Borrowed from nim-mode
(defun jal-smie--paren-open ()
  (cons 'column (+ (current-indentation) tab-width)))

;; Borrowed from nim-mode
(defun jal-smie-before-paren-opener (_kind token)
  (if (or (and (equal "{" token)
               (eq ?. (char-after (1+ (point)))))
          (not (equal "{" token)))
      (let ((parent (when (member (nth 2 (smie-indent--parent)) nim-smie--defuns)
                      (smie-indent--parent)))
            (prev-info (nim-smie--get-prev-info)))
        (if parent
            (save-excursion
              (goto-char (nth 1 parent))
              (cond
               ((and (< (line-number-at-pos)
                        (assoc-default :line prev-info))
                     (= (current-indentation)
                        (assoc-default :indent prev-info)))
                (current-indentation))
               ((or (nim-smie--anonymous-proc-p)
                    (nim-smie--anonymous-proc-p nil t))
                (nim-traverse)
                (cons 'column (current-indentation)))
               ((nim-same-closer-line-p)
                (nim-traverse)
                (current-indentation))
               ((and (< (line-number-at-pos)
                        (assoc-default :line nim-smie--line-info))
                     (equal "{" token))
                (if (member "}" (assoc-default :closers nim-smie--line-info))
                    (current-indentation)
                  nim-smie-function-indent))))
          (nim-traverse)
          (cons 'column (current-indentation))))))



;; Borrowed from nim-mode
(defun jal-line-contain-p (char &optional pos backward)
  "Return non-nil if the current line has CHAR.
But, string-face's CHAR is ignored.  If you set POS, the check starts from POS."
  (save-excursion
    (catch 'exit
      (when pos (goto-char pos))
      (while (if backward (not (bolp)) (not (eolp)))
        (let ((ppss (syntax-ppss)))
          (when (and (not (nth 3 ppss))
                     (not (nth 4 ppss))
                     (if (numberp char)
                         (eq char (char-after (point)))
                       ;; assume list of chars
                       (member (char-after (point)) char)))
            (throw 'exit (point)))
          (if backward (backward-char) (forward-char)))))))

;; Borrowed from nim-mode
(defun jal-traverse ()
  (when (looking-back "= +" nil)
    (search-backward "="))
  (while (jal-line-contain-p '(?\} ?\) ?\]) nil 'backward)
    (condition-case nil (backward-sexp)))
  (goto-char (+ (point-at-bol) (current-indentation))))

;; Borrowed from nim-mode
(defun nim-set-force-indent (indent &optional override)
  (when (or override (not (cdr (assoc :force-indent nim-smie--line-info))))
    (setf (cdr (assoc :force-indent nim-smie--line-info)) indent))
  nil)



;; Borrowed from nim-mode
(defun jal-smie--list-intro-conditions ()
  ;; If it’s completed as one line, set indent forcefully
  (save-excursion
    (when (and
           (looking-at-p (rx (or "if" "elsif" "else"
                                 "while")))
           (looking-at-p (rx (or "then"
                                 "loop"))))
      (jal-traverse)
      ;; (nim-set-force-indent (current-indentation))   ;; TODO
      ))
  ;; When it’s non-nil, it organizes condition parts.
  ;; ex:
  ;;      if ...long multiple conditions...
  ;;         ...long multiple conditions...:
  ;;        ↑ indent like this
  t)


;; Borrowed from nim-mode
;;; For debug
;; or you can use ‘smie-edebug’.
(defun jal-debug-smie-rules (kind token)
  (let ((fmt (concat "kind(%s)-Token(%s)-Point(%d)\n"
                     "sibling(%s)-bolp(%s)\n"
                     "parent(%s)-hanging(%s)\n"
                     "line-info(%s)\n")))
    (message (format fmt kind token (point)
                     (ignore-errors (smie-rule-sibling-p))
                     (ignore-errors (smie-rule-bolp))
                     (ignore-errors (smie-indent--parent))
                     (ignore-errors (smie-rule-hanging-p))
                     ;; nim-smie--line-info ;; TODO
                     ))))
(advice-add 'jal-mode-smie-rules :before #'jal-debug-smie-rules)

(provide 'jal-smie)
;;; jal-smie.el ends here
