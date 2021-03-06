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


;; A simple use of esrap (a PEG parser - see Bryan Ford) to compile simple arithmetic 
;; expressions into corresponding Common Lisp exprs.  Again, an early experiment, YMMV.

;; use: 
;; > (ql:quickload :esrap)
;; compile and load this file
;; > (in-package :arith-grammar)
;; > (test)
;; > #{1+2}   ; should return 3
;; > '#{1+2}  ; should return (+ 1 2)


(defpackage :arith-grammar
  (:use :cl :esrap))

(in-package :arith-grammar)

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule ws (? whitespace))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule num (and ws integer)
  (:destructure (ign int)
   (declare (ignore ign))
   int))

(defrule paren-expr (and ws #\( expr ws #\) )
  (:destructure (ws1 lp e ws2 rp)
   (declare (ignore ws1 ws2 lp rp))
   e))

(defrule primary (or paren-expr num))

(defun build (list a)
  (if (null list)
      a
    (let ((term (first list)))
      `(,(intern (second term)) ,(build (cdr list) a) ,(third term)))))

(defrule factor (and primary (* (and ws (or #\* #\/) primary)))
  (:destructure (a list)
   (build (reverse list) a)))

(defrule expr (and factor (* (and ws (or #\+ #\-) factor)))
  (:destructure (a list)
   (build (reverse list) a)))

(defun test ()
  (set-dispatch-macro-character #\# #\{
     #'(lambda (s c n)
         (declare (ignore c n))
         (parse 'expr
                (with-output-to-string (str)
                  (loop for c = (read-char s nil 'eof)
                        until (or (eq c 'eof) (char= c #\}))
                        do (write-char c str)))))))
