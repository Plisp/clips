;;;; clips.asd

(asdf:defsystem #:clips
  :description "Describe clips here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:yacc)
  :components ((:file "c-lexer")
               (:file "c-parser")
               (:file "clips")))
