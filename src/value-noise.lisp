(in-package :value-noise)

(deftype octave () '(integer 0 20))

(sera:-> hash (fixnum fixnum fixnum fixnum)
         (values (unsigned-byte 8) &optional))
(defun hash (x y z seed)
  (declare (optimize (speed 3)))
  (macrolet ((expand (init &rest coords)
               (reduce
                (lambda (acc c)
                  `(aref +hash+ (logand (+ ,c ,acc) 255)))
                coords :initial-value init)))
    (expand seed x y z)))

(declaim (inline rand))
(sera:-> rand (fixnum fixnum fixnum fixnum)
         (values (single-float 0.0 1.0) &optional))
(defun rand (x y z seed)
  (aref +grid+ (hash x y z seed)))

(declaim (inline interpolate))
(sera:-> interpolate (single-float single-float single-float)
         (values single-float &optional))
(defun interpolate (v1 v2 x)
  (+ v1 (* x (- v2 v1))))

(defmacro quotient-remainder (bindings &body body)
  (flet ((make-binding (binding forms)
           (let ((result (gensym)))
             (destructuring-bind (quotient remainder numerator denominator)
                 binding
               `(let ((,result (/ ,numerator ,denominator)))
                  (multiple-value-bind (,quotient ,remainder)
                      (floor ,result)
                    ,forms))))))
    (reduce #'make-binding bindings
            :from-end t
            :initial-value `(progn ,@body))))

(sera:-> octave-noise (single-float
                       single-float
                       single-float
                       octave fixnum)
         (values (single-float 0.0 1.0) &optional))
(defun octave-noise (x y z octave seed)
  (declare (optimize (speed 3)))
  (let ((divisor (/ (float (expt 2 octave)))))
    (quotient-remainder ((qx δx x divisor)
                         (qy δy y divisor)
                         (qz δz z divisor))
      (let* ((v000 (rand (+ qx 0) (+ qy 0) (+ qz 0) seed))
             (v001 (rand (+ qx 1) (+ qy 0) (+ qz 0) seed))
             (v010 (rand (+ qx 0) (+ qy 1) (+ qz 0) seed))
             (v011 (rand (+ qx 1) (+ qy 1) (+ qz 0) seed))

             (v100 (rand (+ qx 0) (+ qy 0) (+ qz 1) seed))
             (v101 (rand (+ qx 1) (+ qy 0) (+ qz 1) seed))
             (v110 (rand (+ qx 0) (+ qy 1) (+ qz 1) seed))
             (v111 (rand (+ qx 1) (+ qy 1) (+ qz 1) seed))

             (v00 (interpolate v000 v001 δx))
             (v01 (interpolate v010 v011 δx))
             (v10 (interpolate v100 v101 δx))
             (v11 (interpolate v110 v111 δx))

             (v0  (interpolate v00  v01  δy))
             (v1  (interpolate v10  v11  δy))

             (v   (interpolate v0   v1   δz)))
        v))))

(sera:-> value-noise (single-float
                      single-float
                      single-float
                      &key (:octaves octave) (:seed fixnum))
         (values (single-float 0.0 1.0) &optional))
(defun value-noise (x y z &key (octaves 5) (seed 1))
  "Generate value noise in the range \\([0, 1]\\). @c(x), @c(y) and
@c(z) must be non-negative @c(single-float) values. @c(octaves)
specifies the number of high-frequency components in the
noise. @c(seed) is used to generate a unique examplar of noise."
  (declare (optimize (speed 3)))
  (loop for octave fixnum below octaves
        for noise single-float = (octave-noise x y z octave seed)
        sum (/ noise (expt 2 octave)) into acc single-float
        finally (return
                  (let ((exp (expt 2 octaves)))
                    (/ (* acc exp)
                       (* 2.0 (1- exp)))))))
