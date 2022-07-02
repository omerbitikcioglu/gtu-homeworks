; Gebze Technical University
; Computer Engineering Department
; CSE341 Programming Languages
; Homework 3 - Part 2
; Author: Ömer Faruk Bitikçioğlu

(setf keywords '("and" "or" "not" "equal" "less" "nil" "list"
"append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp"
"true" "false"))

(setf kw-keywords '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST"
"KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF"
"KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))

(setf kw-map (mapcar #'cons keywords kw-keywords))

(setf operators '("+" "-" "/" "*"  "(" ")" "**" "\"" "\"" ","))

(setf op-operators '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"
"OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA"))

(setf op-map (mapcar #'cons operators op-operators))

(defun gppinterpreter (&optional filename)
    "Starting point of the interpreter, if file name isn't given then go for repl"
    (if (null filename)
        (repl)
        (read-file filename)))

(defun repl ()
    "Reads the code from the terminal and analyzes it"
    (loop
        (setf code-line (read-line))
        (tokenize-line code-line)
        (parser)
    )
)

(defun read-file (filename)
    "Reads the code from the given file and analyzes it"
    (with-open-file (stream filename)
        (setf code-list
            (loop for line = (read-line stream nil)
                while line
                collect line)))
    (tokenize-list code-list)
    (parser)
)

(defun tokenize-list (code-list)
    "Splits the given code list into code lines"
    (if (not (null code-list))
        (progn
            (setf code-line (car code-list))
            (tokenize-line code-line)
            (tokenize-list (cdr code-list))
        )
    )
)

(defun tokenize-line (code-line)
    "Makes necessary changes and tokenizes the given code line"
    (setf code-line (string-trim " " code-line))
    (setf comment_pos (search ";;" code-line))
    (if (not (null comment_pos))
        ;Trim the comment
        (setf code-line (subseq code-line 0 (+ comment_pos 2))))
    (setf space_pos (position #\Space code-line))
    (if (null space_pos)
        (tokenize code-line) 
        (progn 
            (tokenize (subseq code-line 0 space_pos))
            (tokenize-line (subseq code-line space_pos))
        )
    )
)

(setf escapes '(#\( #\) #\"))

(defun tokenize (token)
    "Tokenizes the given token"
    (if (not (null token))
        (progn 
            (setf len (length token))
            (if (> len 1)
                (progn
                    (setf first-char (char token 0))
                    (setf last-char (char token (- len 1)))
                    (cond
                        ((find first-char escapes)
                            (progn
                                (determine (string first-char))
                                (tokenize (subseq token 1))
                            )
                        )
                        ((find last-char escapes)
                            (progn
                                (setf last-char-pos (position last-char token))
                                (tokenize (subseq token 0 last-char-pos))
                                (tokenize (subseq token last-char-pos))
                            )
                        )
                        (t (determine token))
                    )
                )
                (determine token)
            )
        )
    )
)

(defun determine (token)
    (setf index 0)
    (setf token-type "")
    (cond
        ((is-member token keywords)
            (progn
                (setf index (position token keywords :test #'string=))
                (setf token-type (cdr (nth index kw-map)))))
        ((is-member token operators)
            (progn
                (setf index (position token operators :test #'string=)) 
                (setf token-type (cdr (nth index op-map)))))
        ((equal token ";;") (setf token-type "COMMENT"))
        ((is-value token) (setf token-type "VALUE"))
        ((is-identifier token) (setf token-type "IDENTIFIER"))
        (t (setf token-type "ERROR"))
    )
    (with-open-file
        (stream "parsed_lisp.txt" :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream (concatenate 'string token-type "~%"))
    )
)

(defun is-member (item list)
    "Search item in list"
    (if (null list) (return-from is-member nil))
    (if (equal item (car list))
        (return-from is-member T)
        (is-member item (cdr list))
    )
)

(defun is-digit (ch)
	(setf ascii (char-code ch))
	(setf isDigit nil)
	(if (and (> ascii 47) (< ascii 58))
		(setf isDigit t))
	(return-from is-digit isDigit)
)

(defun is-alpha (ch)
	(setf ascii (char-code ch))
	(setf isAlpha nil)
	(if (or (and (> ascii 64) (< ascii 91)) (and (> ascii 96) (< ascii 123)))
		(setf isAlpha t))
	(return-from is-alpha isAlpha)
)

(defun is-value (token)
    (setf len (length token))
	(setf isValue nil)
	(if (and (= 1 len) (is-digit (char token 0)))
		(setf isValue t)
    )
    (if (and (> (length token) 1) (not (equal "0" (char token 0))))
		(if (every #'is-digit token) (setf isValue t))
        (return-from is-value isValue)
    )
)

(defun is-identifier (token)
    (setf isIdentifier nil)
	(setf len (length token))
    (if
        (and
            (is-alpha (char token 0))
            (or
                (some #'is-digit token)
                (every #'is-alpha token)
            )
        )
        (setf isIdentifier t)
    )
    (return-from is-identifier isIdentifier)
)

; Reduction rules
(setf startr (list '("INPUT" "START") '("INPUT")))

(setf inputr (list '("EXPI") '("EXPLISTI") '("EXPB")))

(setf expir
    (list
        '("OP_OP" "OP_PLUS" "EXPI" "EXPI" "OP_CP")
        '("OP_OP" "OP_MINUS" "EXPI" "EXPI" "OP_CP")
        '("OP_OP" "OP_MULT" "EXPI" "EXPI" "OP_CP")
        '("OP_OP" "OP_DIV" "EXPI" "EXPI" "OP_CP")
        '("IDENTIFIER")
        '("VALUE")
        '("OP_OP" "IDENTIFIER" "EXPLISTI" "OP_CP")
        '("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDLIST" "EXPLISTI" "OP_CP")
        '("OP_OP" "KW_IF" "EXPB" "EXPLISTI" "OP_CP")
        '("OP_OP" "KW_IF" "EXPB" "EXPLISTI" "EXPLISTI" "OP_CP")
        '("OP_OP" "KW_FOR" "OP_OP" "IDENTIFIER" "EXPI" "EXPI" "OP_CP" "EXPLISTI" "OP_CP")
        '("OP_OP" "KW_SET" "IDENTIFIER" "EXPI" "OP_CP")
    )
)

(setf idlistr (list '("OP_OP" "IDS" "OP_CP")))
(setf idsr (list '("IDS" "IDENTIFIER") '("IDENTIFIER IDENTIFIER")))

(setf expbr
    (list
        '("OP_OP" "KW_AND" "EXPB" "EXPB" "OP_CP")
        '("OP_OP" "KW_OR" "EXPB" "EXPB" "OP_CP")
        '("OP_OP" "KW_NOT" "EXPB" "OP_CP")
        '("OP_OP" "KW_EQUAL" "EXPB" "EXPB" "OP_CP")
        '("OP_OP" "KW_EQUAL" "EXPI" "EXPI" "OP_CP")
        '("BinaryValue")
    )
)

(setf binary-valuer (list '("KW_TRUE") '("KW_FALSE")))

(setf explistir
    (list
        '("OP_OP" "KW_CONCAT" "EXPLISTI" "EXPLISTI" "OP_CP")
        '("OP_OP" "KW_APPEND" "EXPI" "EXPLISTI" "OP_CP")
        '("LISTVALUE")
    )
)

(setf listvaluer 
    (list 
        '("OP_OP" "KW_LIST" "VALUES" "OP_CP")
        '("OP_OP" "KW_LIST" "OP_CP")
        '("KW_NIL")
    )
)

(setf valuesr (list '("VALUES" "VALUE") '("VALUE" "VALUE")))

(defun parser ()
    (with-open-file
        (stream "parsed_lisp.txt")
        (setf input
            (loop for line = (read-line stream nil)
                while line
                collect line)))
    (setf stack '())
    (if (parse input stack)
        (print "Syntax OK")
        (print "Syntax Error!"))
)

(defun parse (input stack)
    ; Shift input string
    (if (not (equal (car input) "COMMENT"))
        (setf stack (append stack (list(car input)))))
    (setf input (cdr input))
    (print stack)
    ; Check stack if it contains a handle
    ; If it contains a handle, update the stack with reduced handle
    (setf stack (check-stack stack))
    (setf stack (final-check-stack stack))
    (print stack)
    (cond ((equal stack '("START")) (return-from parse t))
            ((null (car input)) (return-from parse nil))
            (t (parse input stack)))
)

(defun check-stack (stack)
    (cond
        ((setf handle (find-handle expir stack)) (setf stack (reduce-handle stack handle "EXPI")))
        ((setf handle (find-handle idlistr stack)) (setf stack (reduce-handle stack handle "IDLIST")))
        ((setf handle (find-handle idsr stack)) (setf stack (reduce-handle stack handle "IDS")))
        ((setf handle (find-handle expbr stack)) (setf stack (reduce-handle stack handle "EXPB")))
        ((setf handle (find-handle binary-valuer stack)) (setf stack (reduce-handle stack handle "BinaryValue")))
        ((setf handle (find-handle explistir stack)) (setf stack (reduce-handle stack handle "EXPLISTI")))
        ((setf handle (find-handle listvaluer stack)) (setf stack (reduce-handle stack handle "LISTVALUE")))
        ((setf handle (find-handle valuesr stack)) (setf stack (reduce-handle stack handle "VALUES")))
        (t (return-from check-stack stack))
    )
    (remove nil stack)

    ; Recursively check the stack until can't find any handle
    (check-stack stack)
)

(defun final-check-stack (stack)
    (cond
        ((member stack inputr :test 'equal) (setf stack (reduce-handle stack stack "INPUT")))
        ((member stack startr :test 'equal) (setf stack (reduce-handle stack stack "START")))
        (t (return-from final-check-stack stack))
    )
    (remove nil stack)
    (final-check-stack stack)
)

(defun find-handle (rule stack)
    (cond 
        ((null stack) (return-from find-handle nil))
        ((member stack rule :test 'equal) (return-from find-handle stack))
        (t (find-handle rule (cdr stack)))
    )
)

(defun reduce-handle (stack handle reduce-to)

    ; Find pos of handle in stack
    (setf pos (position (car handle) stack :from-end t))

    ; Change handle with the given reduce-to string
    (setf (nth pos stack) reduce-to)
    (setf handle-length (length handle))
    (setf junk-part-length (- handle-length 1))

    (loop for i from (+ pos 1) to (+ pos junk-part-length)
        do (if (nth (+ i junk-part-length) stack)
                (setf (nth i stack) (nth (+ i junk-part-length) stack))
                (setf (nth i stack) nil)))
    (delete nil stack)
    ; Return new stack
    (return-from reduce-handle stack)
)

(gppinterpreter)