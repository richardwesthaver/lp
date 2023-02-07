;;; lp.el --- language parser -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A parser-combinator library for Emacs. Inspired by John Wiegley's
;; pl.el which is inspired by Haskell's parsec.

;;; Code:
(eval-when-compile 
  (require 'cl-lib))

(defgroup lp nil
  "Customization group for lp."
  :tag "lp"
  :group 'development)

(defvar lp-last-err nil
  "The last error returned by any `lp' parser.")

(defcustom lp-charset 'ascii
  "The charset used to parse stuff."
  :group 'lp
  :type 'symbol)

(defcustom lp-log-enable nil
  "Custom log"
  :group 'lp
  :type 'boolean)

(defcustom lp-log-buffer "*lp:log*"
  "`lp' log buffer."
  :group 'lp
  :type 'string)

(defcustom lp-input-buffer "*lp:input*"
  "`lp' input buffer."
  :group 'lp
  :type 'string)

(defcustom lp-output-buffer "*lp:output*"
  "`lp' output buffer."
  :group 'lp
  :type 'string)

(defun lp-eof-or-char-s ()
  (let ((c (char-after)))
    (if c
	(char-to-string c)
      "`EOF'")))


(defun lp-err (msg)
  (cons 'lp-err msg))

(defun lp-err2 (expected found)
  (lp-err (format "Found '%s' -> Expected '%s'"
		      found expected)))

(defun lp-err-p (obj)
  (and (consp obj)
       (eq (car obj) 'lp-err)))

(defalias 'lp-err-s 'cdr)

(defsubst lp-throw (msg)
  (throw 'lp-fail msg))

(defun lp-stop (&rest args)
  (lp-throw
   (setq lp-last-err
	 (let ((msg (plist-get args :msg))
	       (expected (plist-get args :expected))
	       (found (plist-get args :found)))
	   (when (or (stringp msg)
		     (and (stringp expected)
			  (stringp found)))
	     (if (stringp msg)
		 (lp-err msg)
	       (lp-err2 expected found)))))))

(defun lp-c (c)
  "Parse a single character C."
  (let ((next (char-after)))
    (if (and (not (eobp))
	     (char-equal next c))
	(progn (forward-char 1)
	       (char-to-string c))
      (lp-stop :expected (char-to-string c)
	       :found (lp-eof-or-char-s)))))

(defun lp-c* ()
  "Parse any character."
  (if (not (eobp))
      (prog1 (char-to-string (char-after))
	(forward-char))
    (lp-stop :expected "char"
	     :found (lp-eof-or-char-s))))

(defun lp-sat (pred)
  "Parse any character satisfying predicate PRED."
  (let ((next (char-after)))
    (if (and (not (eobp))
	     (funcall pred next))
	(progn (forward-char 1)
	       (char-to-string next))
      (lp-stop :expected (format "%s" pred)
	       :found (lp-eof-or-char-s)))))

(defun lp-re (re)
  "Parse the input matching regexp RE."
  (if (looking-at re)
      (progn (goto-char (match-end 0))
	     (match-string 0))
    (lp-stop :expected re
	      :found (lp-eof-or-char-s))))

(defun lp-make-alt (ch)
  (let ((re-head "")
	(re-str "")
	(re-end "")
	caret-p)
    (dolist (c ch)
      (cond
       ((char-equal c ?\]) (setq re-head "]"))
       ((char-equal c ?-) (setq re-end "-"))
       ((char-equal c ?^) (setq caret-p t))
       (t (setq re-str (concat re-str (char-to-string c))))))
    (when caret-p
      (if (and
	   (string= re-end "-")
	   (string= re-head "")
	   (string= re-str ""))
	  (setq re-end "-^")
	(setq re-str (concat re-str "^"))))
    (concat re-head re-str re-end)))

(defun lp-c-l (chars)
  "Return the parsed character if the current char is in the list of
CHARS, else return nil."
  (let* ((sexp '(lp-or))
	 (chars (mapcar (lambda (c) (list #'lp-c (lp-c-de c))) chars)))
    (append sexp chars)))

(defmacro lp-c-in (&rest ch)
  "Return the current character if it is a member of CHARS."
  (let ((sexp '(lp-or))
	(parsers (mapcar (lambda (c) (list #'lp-c c)) ch)))
    (append sexp parsers)))

(defun lp-c-not-in (&rest ch)
  "Return the parsed character if the current char is not in the
list of supplied CHARS, else return nil."
  (lp-re (format "[^%s]" (lp-make-alt ch))))

(defsubst lp-str (str)
  "Parse STR and only consume the input for an exact match.
Return the parsed string.

Note this function's behavior is different from the `string'
function of Haskll's Parsec.  Use `lp-string' if you want the
same behavior as in Haskell."
  (lp-re (regexp-quote str)))

(defsubst lp-string (str)
  "Parse STR and consume the input even for a partial match.
Return the parsed string.

It is equivalent to calling `lp-c' multiples times so the
input will be consumed if the parser fails in the middle of the
STR.  This function has the same behavior as the `string' function
of Haskell's Parsec.  See also `lp-str'."

  (mapc (lambda (c) (lp-c c)) str))

(defsubst lp-n-is (n)
  "Parse the number N and return the parsed number as a string."
  (lp-re (regexp-quote (number-to-string n))))

(defsubst lp-n ()
  "Parse any decimal number and return the number as a string.
Does not account for other notations (hex, scientific, binary)"
  (lp-re "-?[0-9]+\\.?[0-9]*"))

(defsubst lp-a ()
  "Parse any English letter."
  (lp-re "[a-zA-Z]"))

(defsubst lp-uuid ())

(defsubst lp-url ())

(defsubst lp-d ()
  "Parse any digit."
  (lp-re "[0-9]"))

(defmacro lp-or (&rest pars)
  "Try each parser in PARS sequentially.
Return the result of the first parser that succeeds. This
function will continue trying parsers until a parser fails with
consumed input or there are no more parsers to try."
  (let ((par (make-symbol "parser"))
	(err (make-symbol "err"))
	(err-lst (make-symbol "err-list")))
    `(let (,err-lst ,par ,err)
       (catch 'lp-or-failed
	 ,@(mapcar
	    (lambda (p)
	      `(lp-protect-atom lp-or
				(lp-start
				 (throw 'lp-or-failed
					(lp-unwrap-err ,err
						       (lp-atom lp-or ,p)
						       (push (lp-err-s ,err) ,err-lst))))))
	    pars)
	 (lp-stop
	  :msg
	  (replace-regexp-in-string
	   "\n" "\n\t"
	   (concat "All parsers failed:\n"
		   (mapconcat #'identity ,err-lst "\n"))))))))

(defalias 'lp-and 'progn)

(defalias 'lp-ret 'prog1)

(defalias 'lp-coll 'list)

(defun lp-coll* (&rest args)
  "Return the non-nil results of all parsers in ARGS."
  (delq nil (apply #'lp-coll args)))

(defmacro lp-coll-s (&rest args)
  "Return the results of all parsers in ARGS as a string."
  `(lp-l2s (lp-coll ,@args)))

(defmacro lp-try (par)
  "Try parser PAR, and pretend that no input is consumed on error."
  (let ((pt (make-symbol "pt"))
	(err (make-symbol "lp-err")))
    `(let ((,pt (point)))
       (lp-unwrap-err ,err
		      (lp-and ,par)
		      (goto-char ,pt)))))

(defmacro lp-lookahead (par)
  "Try parser PAR, and pretend that no input is consumed on success."
  (let ((pt (make-symbol "pt")))
    `(let ((,pt (point)))
       (lp-ret ,par
	       (goto-char ,pt)))))

(defsubst lp--atom-tag (name)
  (intern (format "lp-failed-at-half-%s" name)))

(defmacro lp-protect-atom (name par)
  "Use this with `lp-atom'"
  (declare (indent 1))
  (let ((tag (lp--atom-tag name)))
    `(catch 'lp-protect-atom-failed
       (lp-throw (catch ',tag
		   (throw 'lp-protect-atom-failed ,par))))))

(defmacro lp-atom (name par)
  (let ((pt (make-symbol "pt"))
	(err (make-symbol "err"))
	(tag (lp--atom-tag name)))
    `(let ((,pt (point)))
       (lp-unwrap-err ,err
		      ,par
		      (unless (= (point) ,pt)
			(throw ',tag ,err))))))

(defmacro lp-unwrap-err (err par &rest cb)
  (declare (indent 2))
  `(catch 'lp-unwrap-err-failed
     (let ((,err (lp-start
		  (throw 'lp-unwrap-err-failed ,par))))
       ,@cb
       (lp-throw ,err))))

(defmacro lp-with-err (msg &rest body)
  "Use MSG as the error message on error in BODY."
  (declare (indent 1))
  `(lp-unwrap-err _
       (lp-and ,@body)
     (lp-throw (lp-err ,msg))))

(defmacro lp-ensure (&rest body)
  "Eval BODY and panic on error."
  (let ((err (make-symbol "err")))
    `(lp-unwrap-err ,err
	 (lp-and ,@body)
       (error "%s" (lp-err-s ,err)))))

(defmacro lp-ensure-with-err (msg &rest body)
  "Panic with MSG as the error message on error in BODY."
  (declare (indent 1))
  `(lp-ensure
    (lp-with-err ,msg
		 (lp-and ,@body))))

(defmacro lp-many (parser)
  "Apply the PARSER zero or more times and return a list of the results."
  (let ((res-sym (make-symbol "results")))
    `(let (,res-sym)
       (lp-protect-atom lp-many
         (lp-start
          (while (not (eobp))
            (push (lp-atom lp-many ,parser) ,res-sym))))
       (nreverse ,res-sym))))

(defmacro lp-many1 (parser)
  "Apply the PARSER one or more times and return a list of the results."
  `(cons ,parser (lp-many ,parser)))

(defsubst lp-l2s (l)
  (if (stringp l)
      l
    (mapconcat #'identity l "")))

(defmacro lp-many-as-string (parser)
  "Apply the PARSER zero or more times and return the results as a string."
  `(mapconcat #'identity (lp-many ,parser) ""))

(defalias 'lp-many-s 'lp-many-as-string)

(defmacro lp-many1-as-string (parser)
  "Apply the PARSER one or more times and return the results as a string."
  `(mapconcat #'identity (lp-many1 ,parser) ""))

(defalias 'lp-many1-s 'lp-many1-as-string)

(defmacro lp-many-till (parser end &optional type)
  "Apply PARSER zero or more times until END succeeds.
The return value is determined by TYPE.  If TYPE is `:both', return
the cons `(many . end)'.  If TYPE is `:end', return the result of END.
In other cases, return the result of PARSER.

Used to scan comments:

> (lp-and
>   (lp-str \"<--\")
>   (lp-many-till (lp-c*) (lp-str \"-->\")))"

  (let ((res-sym (make-symbol "results"))
        (end-res-sym (make-symbol "end-result")))
    `(let ((,res-sym nil) ,end-res-sym)
       (setq ,end-res-sym
             (catch 'lp-failed-many-till
               (while t
                 (lp-or (throw 'lp-failed-many-till ,end)
                            (push ,parser ,res-sym)))))
       (setq ,res-sym (nreverse ,res-sym))
       ,(cond
         ((eq type :both) `(cons ,res-sym ,end-res-sym))
         ((eq type :end) end-res-sym)
         (t res-sym)))))

(defmacro lp-many-till-as-string (parser end &optional type)
  "Apply PARSER zero or more times until END succeeds.
Return the result of PARSER or END as a string.  TYPE has the same
meaning as `lp-many-till'."
  (let ((res-sym (make-symbol "results")))
    (cond
     ((eq type :both)
      `(let ((,res-sym (lp-many-till ,parser ,end ,type)))
         (cons (lp-l2s (car ,res-sym))
               (lp-l2s (cdr ,res-sym)))))
     (t
      `(lp-l2s (lp-many-till ,parser ,end ,type))))))

(defalias 'lp-many-till-s 'lp-many-till-as-string)

(defmacro lp-until (parser &optional type)
  "Parse any characters until PARSER succeeds.
TYPE has the same meaning as `lp-many-till'."
  `(lp-many-till (lp-c*) ,parser ,type))

(defmacro lp-until-as-string (parser &optional type)
  "Parse any characters until PARSER succeeds.
Return the result of either part as a string.  TYPE has the same
meaning as `lp-many-till'."
  `(lp-many-till-as-string (lp-c*) ,parser ,type))

(defalias 'lp-until-s 'lp-until-as-string)

(defmacro lp-not-followed-by (parser)
  "Succeed only when PARSER fails.  Consume no input."
  (let ((res-sym (make-symbol "results")))
    `(catch 'lp-failed-not-followed-by-out
       (lp-try
        (let ((,res-sym
               (catch 'lp-failed-not-followed-by-in
                 (throw 'lp-failed-not-followed-by-out
                        (lp-or (throw 'lp-failed-not-followed-by-in (lp-try ,parser))
                                   nil)))))
          (lp-stop :msg (format "Unexpected followed by: %s" ,res-sym)))))))

(defmacro lp-endby (parser end)
  "Parse zero or more occurrences of PARSER, separated and ended by END.
Return a list of values returned by PARSER."
  `(lp-many (lp-ret ,parser
                  ,end)))

(defmacro lp-sepby (parser separator)
  "Parse zero or more occurrences of PARSER, separated by SEPARATOR.
Return a list of values returned by PARSER."
  `(lp-or
    (cons ,parser (lp-many (lp-and ,separator ,parser)))
    nil))

(defmacro lp-between (open close parser)
  "Parse OPEN, followed by PARSER and CLOSE.
Return the value returned by PARSER."
  `(lp-and
     ,open
     (lp-ret ,parser
       ,close)))

(defmacro lp-count (n parser)
  "Parse N occurrences of PARSER.
Return a list of N values returned by PARSER."
  (let ((res-sym (make-symbol "results")))
    `(let (,res-sym)
       (dotimes (_ ,n ,res-sym)
         (push ,parser ,res-sym))
       (nreverse ,res-sym))))

(defmacro lp-count-as-string (n parser)
  "Parse N occurrences of PARSER.
Return the N values returned by PARSER as a string."
  `(lp-l2s (lp-count ,n ,parser)))

(defalias 'lp-count-s 'lp-count-as-string)

(defmacro lp-option (opt parser)
  "Try to apply PARSER and return OPT if PARSER fails without comsuming input."
  `(lp-or ,parser ,opt))

(defmacro lp-optional (parser)
  "Apply PARSER zero or one time.  Fail if PARSER fails after consuming input.
Return the result of PARSER or nil.

Note this combinator doesn't discard the result of PARSER so it is
different from the `optional' function of Haskell's Parsec.  If
you want the Haskell's behavior, use `lp-optional*'."
  `(lp-or ,parser nil))

(defmacro lp-optional* (parser)
  "Apply PARSER zero or one time and discard the result.
Fail if PARSER fails after consuming input.

This combinator has the same behavior as the `optional' function of
Haskell's Parsec."
  `(lp-and ,parser nil))

(defmacro lp-peek (parser)
  "Apply PARSER without consuming any input.
When PARSER succeeds, the result of the PARSER is returned.
Otherwise, the return value is an error.  Use `lp-err-p' on
the return value to see whether the PARSER fails or not.  Use
`lp-peek-p' if you want nil to be returned when PARSER fails.

This is a shortcut of combining `lp-start', `lp-try' and
`lp-lookahead'.  Since arbitrary parser is allowed, this
function can be viewed as a more powerful version of `looking-at'
in Emacs Lisp."
  `(lp-start
    (lp-try
     (lp-lookahead ,parser))))

(defmacro lp-peek-p (parser)
  "Same as `lp-peek' except a nil is returned when the PARSER fails."
  (let ((res-sym (make-symbol "res")))
    `(let ((,res-sym (lp-peek ,parser)))
       (unless (lp-err-p ,res-sym)
         ,res-sym))))

(defmacro lp-query (parser &rest args)
  "Get an alternative return value of the PARSER specified by the ARGS.

The args can be in the following forms:

    :beg      --> return the point before applying the PARSER
    :end      --> return the point after applying the PARSER
    :nil      --> return nil
    :groups N --> return Nth group for `lp-re'."
  (let ((orig-pt-sym (make-symbol "orig-pt"))
        (res-sym (make-symbol "results")))
    `(let ((,orig-pt-sym (point))
           (,res-sym ,parser))
       ,(cond
         ((memq :beg args) orig-pt-sym)
         ((memq :end args) '(point))
         ((memq :nil args) nil)
         ((and (memq :group args)
               (consp parser)
               (eq (car parser) 'lp-re))
          (let ((group
                 (cl-loop named outer for arg on args
                          when (eq (car arg) :group) do
                          (cl-return-from outer (cadr arg)))))
            (if (and group (integerp group))
                `(match-string ,group)
              (error "Invalid query :group %s" group))))
         (t res-sym)))))

(defsubst lp-just (x) (cons 'Just x))

(defconst lp-nothing 'Nothing)

(defun lp-maybe-p (x)
  (or (eq x lp-nothing)
      (and
       (consp x)
       (eq (car x) 'Just))))

(defun lp-from-maybe (x)
  "Retrieve the value from Maybe monad X.
If X is `(Just . p)', return p. Otherwise return nil."
  (and (consp x)
       (eq (car x) 'Just)
       (cdr x)))

(defmacro lp-optional-maybe (parser)
  "Apply PARSER zero or one time and return the value in a Maybe monad.
If PARSER fails without consuming any input, return `lp-nothing'.
Otherwise, return `(Just . p)' where p is the result of PARSER."
  (let ((res-sym (make-symbol "result")))
    `(let ((,res-sym (lp-optional ,parser)))
       (if ,res-sym
           (lp-just ,res-sym)
         lp-nothing))))

;;; Lines
(defun lp-newline ()
  "Parse a newline character \"\\n\"."
  (lp-c ?\n))

(defun lp-crlf ()
  "Parse a carriage return (\'\\r\') followed by a newline \"\\n\"."
  (lp-and (lp-c ?\r) (lp-c ?\n)))

(defun lp-eol ()
  "Parse a newline or a CRLF and return \"\\n\"."
  (lp-or (lp-newline) (lp-crlf)))

(defun lp-eob ()
  "Indicate the end of file (buffer)."
  (unless (eobp)
    (lp-stop :expected "`EOF'"
                 :found (lp-eof-or-char-as-string))))

(defalias 'lp-eof 'lp-eob)

(defun lp-eol-or-eof ()
  "Indicate either eol or eof."
  (lp-or (lp-eol) (lp-eof)))

;;; Coercions

(defun lp-c-de (n)
  "Decode codepoint N using `lp-charset'"
  (decode-char lp-charset n))

(defun lp-c-en (c)
  "Encode character C to codepoint using `lp-charset'."
  (encode-char c lp-charset))

(defun lp-s2l (s)
  "Return a list of chars in string S."
  (mapcar #'lp-c-de (append s nil)))

(defalias 'lp-s2v 'string-to-vector)
(defalias 'lp-s2c 'string-to-char)

;;; Entry
(defmacro lp-start (&rest body)
  "Eval BODY and return the results or an `lp-err'."
  `(catch 'lp-fail ,@body))

(defmacro lp-with-input (input &rest parsers)
  "With INPUT, start parsing by applying PARSERS sequentially."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create lp-input-buffer)
     (erase-buffer)
     (insert ,input)
     (goto-char (point-min))
     (lp-start
      ,@parsers)))

(provide 'lp)
;;; lp.el ends here
