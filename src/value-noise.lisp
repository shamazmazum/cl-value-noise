(in-package :value-noise)

(declaim (optimize (speed 3)))
(deftype octave () '(integer 0 20))

(declaim (inline round32))
(defun round32 (x) (logand x #xffffffff))

;; I am not good at RNGs, this can be improved by a skilled person
(declaim (inline lolrng))
(sera:-> lolrng (fixnum fixnum fixnum fixnum)
         (values (single-float 0.0 1.0) &optional))
(defun lolrng (x y z seed)
  (let* ((r1 (round32 (* x #x1b873593)))
         (r2 (round32 (* y #x19088711)))
         (r3 (round32 (* z #xb2d05e13)))
         (r  (round32 (+ seed r1 r2 r3))))
    (/
     (round32 (* (logxor r (ash r -5))
                 #xcc9e2d51))
     #.(float #xffffffff))))

(declaim (inline interpolate))
(sera:-> interpolate (single-float single-float single-float)
         (values single-float &optional))
(defun interpolate (v1 v2 x)
  (declare (type single-float v1 v2 x))
  (+ v1 (* x (- v2 v1))))

(defmacro quotient-remainder (bindings &body body)
  (flet ((make-binding (binding forms)
           (let ((result (gensym)))
             (destructuring-bind (whole fraction number divisor)
                 binding
               `(let ((,result (/ ,number ,divisor)))
                  (declare (type (single-float #.(float most-negative-fixnum)
                                               #.(float most-positive-fixnum))
                                 ,result))
                  (multiple-value-bind (,whole ,fraction)
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
  (let ((divisor #+nil (expt 2.0 (- octave))
                 (float (/ (ash 1 octave)))))
    (quotient-remainder ((qx δx x divisor)
                         (qy δy y divisor)
                         (qz δz z divisor))
      (let* ((v000 (lolrng (+ qx 0) (+ qy 0) (+ qz 0) seed))
             (v001 (lolrng (+ qx 1) (+ qy 0) (+ qz 0) seed))
             (v010 (lolrng (+ qx 0) (+ qy 1) (+ qz 0) seed))
             (v011 (lolrng (+ qx 1) (+ qy 1) (+ qz 0) seed))

             (v100 (lolrng (+ qx 0) (+ qy 0) (+ qz 1) seed))
             (v101 (lolrng (+ qx 1) (+ qy 0) (+ qz 1) seed))
             (v110 (lolrng (+ qx 0) (+ qy 1) (+ qz 1) seed))
             (v111 (lolrng (+ qx 1) (+ qy 1) (+ qz 1) seed))

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
  "Generate value noise in the range [0, 1]. @c(x), @c(y) and @c(z)
must be non-negative @c(single-float) values. @c(octaves) specifies
the number of high-frequency components in the noise. @c(seed) is used
to generate a unique examplar of noise."
  (loop for octave fixnum below octaves
        for noise single-float = (octave-noise x y z octave seed)
        sum (/ noise (ash 1 octave)) into acc single-float
        finally (return
                  (let ((exp (ash 1 octaves)))
                    (/ (* acc exp)
                       (* 2.0 (1- exp)))))))
