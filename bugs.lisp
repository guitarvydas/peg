;; a collection of edge-cases that, at one time, broke the PEG parser

(defun bugs ()
  (pprint (esrap:parse 'peg-grammar-r1::grammar "x <- '1' {(format nil \"~a\" 1)}"))
  (pprint (esrap:parse 'peg-grammar-r1::grammar "x <- '1' {(format nil \"\\{~a\\}\" 1)}"))
  (pprint (esrap:parse 'peg-grammar-r1::grammar "x <- '1' {(format nil \"{~a}\" 1)}")))

