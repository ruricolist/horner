(in-package #:horner)

;; https://GitHub.com/JuliaLang/julia/issues/7033
;; https://github.com/JuliaLang/julia/bhornerlob/master/base/math.jl

(defmacro goertzel (z &body p)
  (once-only (z)
    (with-gensyms (x y r s)
      `(let* ((,x (realpart ,z))
              (,y (imagpart ,z))
              (,r (+ ,x ,x))
              (,s ($ ,x * ,x + ,y * ,y)))
         ,(let ((a (car (last p)))
                (b (car (last p 2)))
                (as '()))
            (loop for i downfrom (- (length p) 2) to 0
                  for ai = (gensym (fmt "A~a" i))
                  do (push `(,ai ,a) as)
                     (setf a `($ ,b + ,r * ,ai)
                           b `($ ,(nth i p) - ,s * ,ai)))
            (let ((ai (gensym "A0")))
              (push `(,ai ,a) as)
              `(let* ,(reverse as)
                 ($ ,b + ,ai * ,x))))))))
