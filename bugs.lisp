;; a collection of edge-cases that, at one time, broke the PEG parser

(defun bugs ()
  (pprint (esrap:parse 'peg-grammar::grammar "x <- [\']"))
  (pprint (esrap:parse 'peg-grammar::grammar "x <- '1' {(format nil \"~a\" 1)}"))
  (pprint (esrap:parse 'peg-grammar::grammar "x <- '1' {(format nil \"\\{~a\\}\" 1)}"))
  (pprint (esrap:parse 'peg-grammar::grammar "x <- '1' {(format nil \"{~a}\" 1)}")))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :cl-user)
  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pprint '#.(esrap:parse 'peg-grammar::grammar #>%bugs>

x <- [\']
x <- '1' {(format nil "~a" 1)}
x <- '1' {(format nil "{~a}" 1)}

%bugs
)))
