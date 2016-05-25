;;;; package.lisp

(defpackage #:horner
  (:use #:cl #:alexandria #:serapeum #:infix-math)
  (:import-from :infix-math :$)
  (:export
   :evalpoly
   :horner :horner/even :horner/odd
   :goertzel))
