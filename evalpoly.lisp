(in-package :horner)

(defmacro evalpoly (z &body p &environment env)
  (setf p
        (loop for coeff in p
              if (constantp coeff)
                collect (eval coeff)
              else collect coeff))
  (flet ((generate-dispatch (z p)
           ;; Goertzel is only better than Horner for N>=3.
           (if (<= (length p) 3)
               `(horner ,z ,@p)
               `(if (complexp ,z)
                    (goertzel ,z ,@p)
                    (horner ,z ,@p)))))
    (if (every (op (constantp _ env)) p)
        (generate-dispatch z p)
        ;; Prevent multiple evaluation of coefficients.
        (let ((temps (make-gensym-list (length p))))
          `(let ,(mapcar #'list temps p)
             ,(generate-dispatch z temps))))))
