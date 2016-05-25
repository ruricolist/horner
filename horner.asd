;;;; horner.asd

(asdf:defsystem #:horner
  :description "Describe horner here"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (:alexandria :serapeum :infix-math)
  :components ((:file "package")
               (:file "horner" :depends-on ("package"))
               (:file "goertzel" :depends-on ("package"))
               (:file "evalpoly" :depends-on ("package"))))
