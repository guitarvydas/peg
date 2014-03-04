(asdf:defsystem #:peg
  :serial t
  :depends-on (#:cl-heredoc #:esrap)
  :components ((:file "package")
               (:file "peg")))