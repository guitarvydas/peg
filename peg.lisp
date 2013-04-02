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

(defpackage :peg-grammar
  (:use :cl :esrap))

(in-package :peg-grammar)

(defrule grammar (and spacing (+ definition) end-of-file))
(defrule definition (and identifier LEFTARROW expression))
(defrule Expression (and Sequence (* SLASH Sequence)))
(defrule Sequence (* Prefix))
(defrule Prefix ((? (or AND NOT) Suffix)))
(defrule Suffix (and Primary (? (or QUESTION  STAR  PLUS)))
(defrule Primary (or
                  (and Identifier (! LEFTARROW))
                  (and OPENPAREN Expression CLOSEPAREN)
                  Literal
                  Class
                  DOT))
(defrule Identifier (and IdentStart (* IdentCont) Spacing))
(defrule IdentStart (character-ranges (#\a #\z) (#\A #\Z) #\_))
(defrule IdentCont (or IdentStart #\- (character-ranges (#\0 #\9))))
(defrule Literal (or (and #\' (* (and (! #\') Char)) #\' Spacing)
                     (and #\" (* (and (! #\") Char)) #\" Spacing)))
(defrule Class (and #\[ (* (and (! #\]) Range)) #\] Spacing))
(defrule Range (or (and Char #\- Char) Char))
(defrule Char (or (and #\\ (or #\n #\r #\t #\' #\" #\[ #\] #\\))
                  (and #\\ (and (character-ranges #\0 #\2)
                                (character-ranges #\0 #\7)
                                (character-ranges #\0 #\7)))
                  (and #\\ (character-ranges #\0 #\7) (? (character-ranges #\0 #\7)))
                  (and (! #\\) (character-ranges (char 0) (char 255)))))
(defrule LEFTARROW (and "<-" Spacing))
(defrule SLASH (and #\/ Spacing))
(defrule AND (and #\& Spacing))
(defrule NOT (and #\! Spacing))
(defrule QUESTION (and #\? Spacing))
(defrule STAR (and #\* Spacing))
(defrule PLUS (and #\+ Spacing))
(defrule OPENPAREN (and #\( Spacing))
(defrule CLOSEPAREN (and #\) Spacing))
(defrule OPENBRACE (and #\{ Spacing))
(defrule CLOSEBRACE (and #\} Spacing))
(defrule DOT (#\. Spacing))


(defrule Spacing (* (or Space Comment)))
(defrule Comment (and #\# (* (and (! EndOfLine) (character-ranges (char 0) (char 255))))
                      (or EndOfLine  EndOfFile)))
(defrule Space (or #\Space #\Tab EndOfLine))
(defrule EndOfLine (or (and #\Return #\Newline) #\Newline #\Return))
(defrule EndOfFile (! (character-ranges (char 0) (char 255))))

