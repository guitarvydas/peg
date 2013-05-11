;;; Copyright (c) 2013, Paul Tarvydas
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;    Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.

;;;    Redistributions in binary form must reproduce the above
;;;    copyright notice, this list of conditions and the following
;;;    disclaimer in the documentation and/or other materials provided
;;;    with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;; (unfinished) A PEG parser written using esrap.


#|
Grammar <- Spacing Definition+ EndOfFile
Definition <- Identifier LEFTARROW Expression
SyntacticCode <- '<' (!'>' .)* '>' Spacing
SemanticCode <- '{' (["] (!["] Char)* ["] / !'}' .)* '}' Spacing
Expression <- Sequence (SLASH Sequence)*
Sequence <- Prefix*
Prefix <- (AND / NOT)? Suffix
Suffix <- Primary (QUESTION / STAR / PLUS)?
Primary <- Identifier !LEFTARROW
	 / OPENPAREN Expression CLOSEPAREN
	 / Literal
	 / Class
	 / DOT
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / '-' / [0-9]
Literal <- ['] (!['] Char)* ['] Spacing
         / ["] (!["] Char)* ["] Spacing
Class <- '[' (!']' Range)* ']' Spacing
Range <- Char '-' Char / Char
Char <- '\\' [nrt'"\[\]\\]
      / '\\' [0-2][0-7][0-7]
      / '\\' [0-7][0-7]?
      / !'\\' .
LEFTARROW <- '<-' Spacing
SLASH     <- '/' Spacing
AND       <- '&' Spacing
NOT       <- '!' Spacing
QUESTION  <- '?' Spacing
STAR      <- '*' Spacing
PLUS      <- '+' Spacing
OPENPAREN <- '(' Spacing
CLOSEPAREN <- ')' Spacing
OPENBRACE  <- '{' Spacing
CLOSEBRACE <- '}' Spacing
DOT       <- '.' Spacing


Spacing <- (Space / Comment)*
Comment <- '#' (!EndOfLine .)* (EndOfLine / EndOfFile)
Space   <- ' ' / '\t' / EndOfLine
EndOfLine <- '\r\n' / '\n' / '\r'
EndOfFile <- !.
|#

; (progn (ql:quickload "esrap") (ql:quickload "cl-heredoc"))

(defpackage :peg-grammar
  (:use :cl :esrap :cl-heredoc))

(in-package :peg-grammar)

(defrule grammar (and spacing (+ definition) EndOfFile)
  (:destructure (spc def eof)
   (declare (ignore spc eof))
  `(progn
     (defpackage :rpeg-grammar
       (:use :cl :esrap :cl-heredoc))
     (in-package :rpeg-grammar)
     ,@def)))

(defrule definition (and identifier LEFTARROW expression spacing (? semanticCode))
  (:destructure (id arr e spc code)
   (declare (ignore arr spc))
   (if (null code)
       `(defrule ,(intern (string-upcase id)) ,e)
     `(defrule ,(intern (string-upcase id)) ,e ,code))))
       
(defrule semanticCode (and OPENBRACE (+ notbrace) CLOSEBRACE)
  (:destructure (lb code rb)
   (declare (ignore lb rb))
   (read-from-string (text code))))

(defrule notbrace (and (! #\}) character)
  (:lambda (x)
    x))

(defrule Expression (and Sequence (* SLASHSequence))
  (:destructure (seq seqs)
   (if seqs
       `(or ,seq ,@seqs)
     seq)))

(defrule SLASHSequence (and SLASH Sequence)
  (:destructure (sl seq)
   (declare (ignore sl))
   seq))

(defrule Sequence (* Prefix)
  (:destructure (&rest pref)
   (if pref
       (if (and (consp pref) (> (length pref) 1))
           `(and ,@pref)
         (first pref))
     (values))))
       

(defrule Prefix (and (? (or AND NOT)) Suffix)
  (:destructure (pref suff)
   (if pref
       (list pref suff)
     suff)))

(defrule Suffix (and Primary (? (or QUESTION  STAR  PLUS)))
  (:destructure (prim suff)
   (if suff
       (list suff prim)
     prim)))

(defrule Primary (or
                  P1 ;(and Identifier (! LEFTARROW))
                  P2 ;(and OPENPAREN Expression CLOSEPAREN)
                  Literal
                  Class
                  DOT)
  (:lambda (x) x))

(defrule P1 (and Identifier (! LEFTARROW))
  (:function first))

(defrule P2 (and OPENPAREN Expression CLOSEPAREN)
  (:function second))

(defrule Identifier (and StringIdentifier)
  (:lambda (x) (intern (string-upcase (first x)))))

(defrule StringIdentifier (and IdentStart (* IdentCont) Spacing)
  (:text t))

(defrule IdentStart (character-ranges (#\a #\z) (#\A #\Z) #\_))
(defrule IdentCont (or IdentStart #\- (character-ranges (#\0 #\9))))

(defrule Literal (or (and #\' (* NotSingle) #\' Spacing)
                     (and #\" (* NotDouble) #\" Spacing))
  (:destructure (q1 string q1 spc)
   (declare (ignore q1 q2 spc))
   (text string)))

(defrule NotSingle (and (! #\') Char)
  (:function second))

(defrule NotDouble (and (! #\") Char)
  (:function second))

(defrule NotRB (and (! #\]) Range)
  (:function second))

(defrule Class (and #\[ (* NotRB) #\] Spacing)
  (:destructure (lb range rb spc)
   (declare (ignore lb rb spc))
   (if (and (consp range)
            (or (not (= 2 (length range)))
                (or (consp (first range))
                    (consp (second range)))))
       `(character-ranges ,@range)
     `(character-ranges ,range))))

(defrule Range (or CharRange SingleChar))

(defrule CharRange (and Char #\- Char)
  (:destructure (c1 dash c2)
   (declare (ignore dash))
   (list c1 c2)))
  
(defrule SingleChar (and Char)
  (:destructure (c)
   c))

(defrule Char (or EscChar NumChar1 NumChar2 AnyChar))

(defrule EscChar (and #\\ (or #\n #\r #\t #\' #\" #\[ #\] #\\))
  (:destructure (sl c)
   (declare (ignore sl))
   ;(format t "esc ~A type=~A~%" c (type-of c))
   (case (char c 0)
     (#\n #\newline)
     (#\r #\return)
     (#\t #\tab)
     (otherwise (char c 0)))))

(defrule NumChar1 (and #\\
                       (character-ranges #\0 #\2)
                       (character-ranges #\0 #\7)
                       (character-ranges #\0 #\7))
  (:destructure (sl n1 n2 n3)
   (declare (ignore sl))
   (code-char (parse-integer (concatenate 'string n1 n2 n3) :radix 8))))

(defrule NumChar2 (and #\\ (character-ranges #\0 #\7) (? (character-ranges #\0 #\7)))
  (:destructure (sl n1 n2)
   (declare (ignore sl))
   (code-char (parse-integer (concatenate 'string n1 n2) :radix 8))))

(defrule AnyChar (and (! #\\) character)
  (:destructure (sl c)
   (declare (ignore sl))
   c))

(defrule LEFTARROW (and "<-" Spacing)
  (:lambda (list) (declare (ignore list))
    (values)))
(defrule SLASH (and #\/ Spacing)
  (:lambda (list) (declare (ignore list))
    (values)))
(defrule AND (and #\& Spacing)
  (:lambda (list) (declare (ignore list))
    'and))
(defrule NOT (and #\! Spacing)
  (:lambda (list) (declare (ignore list))
    '!))
(defrule QUESTION (and #\? Spacing)
  (:lambda (list) (declare (ignore list))
    '?))
(defrule STAR (and #\* Spacing)
  (:lambda (list) (declare (ignore list))
    '*))
(defrule PLUS (and #\+ Spacing)
  (:lambda (list) (declare (ignore list))
    '+))
(defrule OPENPAREN (and #\( Spacing)
  (:lambda (list) (declare (ignore list))
    (values)))
(defrule CLOSEPAREN (and #\) Spacing)
  (:lambda (list) (declare (ignore list))
    (values)))
(defrule OPENBRACE (and #\{ Spacing)
  (:lambda (list) (declare (ignore list))
    (values)))
(defrule CLOSEBRACE (and #\} Spacing)
  (:lambda (list) (declare (ignore list))
    (values)))
(defrule DOT (and #\. Spacing)
  (:lambda (list) (declare (ignore list))
    'character))


(defrule Spacing (* (or Space Comment))
    (:lambda (list) (declare (ignore list))
      (values)))

(defrule Comment (and #\# (* (and (! EndOfLine) char1 ))
                      (or EndOfLine  EndOfFile))
    (:lambda (list) (declare (ignorable list))
      (values)))

(defrule char1 (and character)
  (:lambda (c)
    c))

(defrule Space (or #\Space #\Tab EndOfLine)
    (:lambda (list) (declare (ignore list))
      (values)))

(defrule EndOfLine (or (and #\Return #\Newline) #\Newline #\Return)
    (:lambda (list) (declare (ignore list))
      (values)))

(defrule EndOfFile (! character)
    (:lambda (list) (declare (ignore list))
      (values)))

