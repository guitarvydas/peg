(in-package :peg-grammar)

; do this in the listener (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defun pegself ()
  (pprint (parse 'grammar
#>%peg>
#;;; All rights reserved.

#;;; Redistribution and use in source and binary forms, with or without
#;;; modification, are permitted provided that the following conditions
#;;; are met:

#;;;    Redistributions of source code must retain the above copyright
#;;;    notice, this list of conditions and the following disclaimer.

#;;;    Redistributions in binary form must reproduce the above
#;;;    copyright notice, this list of conditions and the following
#;;;    disclaimer in the documentation and/or other materials provided
#;;;    with the distribution.

#;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
#;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
#;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
#;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
#;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
#;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
#;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#;;; SUCH DAMAGE.

#;; the (PEG) grammar for a PEG parser

Grammar <- Spacing Definition+ EndOfFile {
  (:destructure (spc def eof)
   (declare (ignore spc eof))
   `(progn
      (defpackage :rpeg-grammar
        (:use :cl :esrap :cl-heredoc))
      (in-package :rpeg-grammar)
      ,@def)) }

Definition <- Identifier LEFTARROW Expression {
  (:destructure (id arr e spc code)
   (declare (ignore arr spc))
   (if (null code)
       `(defrule ,(intern (string-upcase id)) ,e)
     `(defrule ,(intern (string-upcase id)) ,e ,code))) }

SemanticCode <- '{' notbrace+ '}' Spacing  {
  (:destructure (lb code rb)
   (declare (ignore lb rb))
   (read-from-string (text code))) }

notbrace <- ! '}' . {
  (:lambda (x)
    x) }

Expression <- Sequence (SLASHSequence)*  {
  (:destructure (seq seqs)
   (if seqs
       `(or ,seq ,@seqs)
     seq)) }

SLASHSequence <- '/' Sequence  {
  (:destructure (seq seqs)
   (if seqs
       `(or ,seq ,@seqs)
     seq)) }

Sequence <- Prefix*  {
  (:destructure (&rest pref)
   (if pref
       (if (and (consp pref) (> (length pref) 1))
           `(and ,@pref)
         (first pref))
     (values)))) }

Prefix <- (AND / NOT)? Suffix {
  (:destructure (pref suff)
   (if pref
       (list pref suff)
     suff))) }

Suffix <- Primary (QUESTION / STAR / PLUS)? {
  (:destructure (prim suff)
   (if suff
       (list suff prim)
     prim))) }

Primary <- P1
	 / P2
	 / Literal
	 / Class
	 / DOT  {
  (:lambda (x) x)) }

P1 <- Identifier !LEFTARROW  {
  (:function first)) }

P2 <- OPENPAREN Expression CLOSEPAREN {
  (:function second)) }

Identifier <- StringIdentifier {
  (:lambda (x) (intern (string-upcase (first x))))) }

StringIdentifier <- IdentStart IdentCont* Spacing {
  (:text t)) }

IdentStart <- [a-zA-Z_]

IdentCont <- IdentStart / '-' / [0-9]

Literal <- ['] NotSingle* ['] Spacing
         / ["] NotDouble* Spacing  {
  (:destructure (q1 string q1 spc)
   (declare (ignore q1 q2 spc))
   (text string))) }

NotSingle <- !['] Char  { (:function second) }

NotDouble <- !["] Char  { (:function second) }

Class <- '[' NotRB* ']' Spacing {
  (:destructure (lb range rb spc)
   (declare (ignore lb rb spc))
   (if (and (consp range)
            (or (not (= 2 (length range)))
                (or (consp (first range))
                    (consp (second range)))))
       `(character-ranges ,@range)
     `(character-ranges ,range)))) }

NotRB <- !']' Range  { (:function second) }

Range <- CharRange / SingleChar

CharRange <- Char '-' Char {
  (:destructure (c1 dash c2)
   (declare (ignore dash))
   (list c1 c2))) }

SingleChar <- Char

Char <- EscChar / NumChar1 / NumChar2 / AnyChar

EscChar <- '\\' ( 'n' / 'r' / 't' / ['] / '\"' / '[' / ']' / '\\' )  {
  (:destructure (sl c)
   (declare (ignore sl))
   ;(format t "esc ~A type=~A~%" c (type-of c))
   (case (char c 0)
     (#\n #\newline)
     (#\r #\return)
     (#\t #\tab)
     (otherwise (char c 0))))) }

NumChar1 <- '\\' [0-2][0-7][0-7]  {
  (:destructure (sl n1 n2 n3)
   (declare (ignore sl))
   (code-char (parse-integer (concatenate 'string n1 n2 n3) :radix 8)))) }

NumChar2 <- '\\' [0-7][0-7]?  {
  (:destructure (sl n1 n2)
   (declare (ignore sl))
   (code-char (parse-integer (concatenate 'string n1 n2) :radix 8)))) }

AnyChar <- !'\\' .  {
  (:destructure (sl c)
   (declare (ignore sl))
   c)) }

LEFTARROW <- '<-' Spacing {
  (:lambda (list) (declare (ignore list))
    (values))) }

SLASH     <- '/' Spacing {
  (:lambda (list) (declare (ignore list))
    (values))) }

AND       <- '&' Spacing {
  (:lambda (list) (declare (ignore list))
    'and) }

NOT       <- '!' Spacing {
  (:lambda (list) (declare (ignore list))
    '!) }

QUESTION  <- '?' Spacing {
  (:lambda (list) (declare (ignore list))
    '?) }

STAR      <- '*' Spacing {
  (:lambda (list) (declare (ignore list))
    '*) }

PLUS      <- '+' Spacing {
  (:lambda (list) (declare (ignore list))
    '+) }

OPENPAREN <- '(' Spacing {
  (:lambda (list) (declare (ignore list))
    (values)) }

CLOSEPAREN <- ')' Spacing {
  (:lambda (list) (declare (ignore list))
    (values)) }

OPENBRACE  <- '{' Spacing {
  (:lambda (list) (declare (ignore list))
    (values)) }

CLOSEBRACE <- '}' Spacing {
  (:lambda (list) (declare (ignore list))
    (values)) }

DOT       <- '.' Spacing {
  (:lambda (list) (declare (ignore list))
    'character) }


Spacing <- (Space / Comment)* {
  (:lambda (list) (declare (ignore list))
      (values)) }

Comment <- '#' (!EndOfLine .)* (EndOfLine / EndOfFile) {
  (:lambda (list) (declare (ignore list))
    'character) }

Space   <- ' ' / '\t' / EndOfLine {
  (:lambda (list) (declare (ignore list))
    'character) }

EndOfLine <- '\r\n' / '\n' / '\r' {
  (:lambda (list) (declare (ignore list))
    'character) }

EndOfFile <- !. {
  (:lambda (list) (declare (ignore list))
    'character) }

%peg
)))


(defun test200 ()
  (pprint (parse 'grammar "Spacing <- (Space / Comment)*
  {(:lambda (list) (declare (ignore list))
      (values)) }")))

