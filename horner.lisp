;;;; horner.lisp

(in-package #:horner)

;;; "horner" goes here. Hacks and glory await!

(defun eval-horner (x coeffs)
  "Eval COEFFS at run time, for testing."
  (reduce (op (+ (* _2 x) _1))
          coeffs
          :from-end t))

(defmacro horner/simple (x &body coeffs)
  (once-only (x)
    (reduce (op `(+ (* ,_2 ,x) ,_1))
            coeffs
            :from-end t)))

(defmacro horner (x &body coeffs)
  "Compute a polynomial using Horner's rule.
Note that the coefficients COEFFS are provided in ascending order of
powers: p0 + p1*x + p2*x^2..."
  (setf coeffs (mapcar (op (if (constantp _1) (eval _1) _1)) coeffs))
  ;; TODO Estimate the error bounds for this particular polynomial?
  (if (every #'realp coeffs)
      `(horner/precond ,x ,@coeffs)
      `(horner/coeffs ,x ,@coeffs)))

(defmacro horner/coeffs (x &body coeffs)
  (let ((last1 (lastcar coeffs)))
    (if (and (numberp last1)
             (= last1 1))
        `(horner/monic ,x ,@(butlast coeffs))
        `(horner/simple ,x ,@coeffs))))

(defmacro horner/precond (x &body coeffs)
  (assert (every #'realp coeffs))
  (case (length coeffs)
    ;; NB Horner form is known to be optimal for degree <= 4.
    (5 `(quartic ,x ,@coeffs))
    (6 `(quintic ,x ,@coeffs))
    (7 `(sextic  ,x ,@coeffs))
    ;; TODO Pan has 8.
    (otherwise
     `(horner/simple ,x ,@coeffs))))

(defmacro horner/even (x &body coeffs)
  "Evaluate polynomial with even powers only: p0 + p1*x^2 + p2*x^4..."
  (once-only (x)
    `(horner (* ,x ,x) ,@coeffs)))

(defmacro horner/odd (x &body coeffs)
  "Evaluate polynomial with odd powers only: p0*x + p1*x^3 + p2*x^5..."
  (once-only (x)
    `(* ,x (horner (* ,x ,x) ,@coeffs))))

;; TODO This may be needless.
(defmacro horner/monic (x &body coeffs)
  "Like `horner' when the last coefficient is 1 (and omitted).
Cf. p1evl in Cephes."
  (once-only (x)
    (reduce (op `(+ (* ,_2 ,x) ,_1))
            (butlast coeffs)
            :from-end t
            :initial-value `(+ ,x ,(lastcar coeffs)))))

(defmacro quartic (x u0 u1 u2 u3 u4)
  "Better-than-Horner computation of the quartic.
See Higham 2002, Knuth v.2."
  (assert (not (= u4 0)))
  (let* ((a0 ($ 1/2 * (u3 / u4 - 1)))
         (b  ($ u2 / u4 - a0 * (a0 + 1)))
         (a1 ($ u1 / u4 - a0 * b))
         (a2 ($ b - 2 * a1))
         (a3 ($ u0 / u4 - a1 * (a1 + a2)))
         (a4 u4))
    (with-gensyms (y)
      (once-only (x)
        `(let ((,y ($ (,x + ,a0) * ,x + ,a1)))
           ($ ((,y + ,x + ,a2) * ,y + ,a3) * ,a4))))))

(defmacro quintic (x u0 u1 u2 u3 u4 u5)
  (once-only (x)
    `($ (quartic ,x ,u1 ,u2 ,u3 ,u4 ,u5)
       * ,x
       + ,u0)))

(defun monic (&rest coefficients)
  "Convert coefficients u_0, u_1... u_n to monic form and return them
as multiple values."
  (let ((last1 (lastcar coefficients)))
    (if (= last1 1)
        (values-list coefficients)
        (values-list
         (append (mapcar (op (/ _ last1))
                         (butlast coefficients))
                 (list 1))))))

(defmacro sextic (x u0 u1 u2 u3 u4 u5 u6)
  "Optimized sextic using Pan's method."
  (mvlet* ((a6 u6)
           (u0 u1 u2 u3 u4 u5 (monic u0 u1 u2 u3 u4 u5 u6))
           (a0 (/ u5 3))
           (a1 ($ u1 - a0 * u2 + a0 ^ 2 * u3 - a0 ^ 3 * u4 + 2 * a0 ^ 5
                 -----------------------------
                 u3 - 2 * a0 * u4 + 5 * a0 ^ 3))
           (b1 (* 2 a0))
           (b2 ($ u4 - a0 * b1 - a1))
           (b3 ($ u3 - a0 * b2 - a1 * b1))
           (b4 ($ u2 - a0 * b3 - a1 * b2))
           (a3 ($ 1/2 * (b3 - (a0 - 1) * b2 + (a0 - 1) * (a0 ^ 2 - 1)) - a1))
           (a2 ($ b2 - (a0 ^ 2 - 1) - a3 - 2a1))
           (a4 ($ b4 - (a2 + a1) * (a3 + a1)))
           (a5 ($ (u0 - a1 * b4))))
    (once-only (x)
      (with-gensyms (z w)
        `(let* ((,z ($ (,x + ,a0) * ,x + ,a1))
                (,w ($ ,z + ,x + ,a2)))
           ($ (((,z - ,x + ,a3) * ,w + ,a4) * ,z + ,a5) * ,a6))))))
