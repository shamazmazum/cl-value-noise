(in-package :cl-value-noise-doc)

(defun prepare-image (scale-x scale-y octaves seed)
  (let ((image (make-array '(500 500) :element-type 'grayscale-pixel)))
    (array-operations/utilities:nested-loop (i j)
        (array-dimensions image)
      (setf (aref image i j)
            (make-gray
             (floor
              (* 255
                 (value-noise (/ i scale-x)
                              (/ j scale-y)
                              0.0
                              :octaves octaves
                              :seed seed))))))
    (make-instance 'grayscale-image :pixels image)))

(defun write-images ()
  (write-image (prepare-image 100.0 100.0 5 324)
               (asdf:system-relative-pathname :cl-value-noise/doc
                                              #p"docs/image1.jpg"))
  (write-image (prepare-image 200.0 200.0 10 124)
               (asdf:system-relative-pathname :cl-value-noise/doc
                                              #p"docs/image2.jpg")))

(defun build-manual ()
  (write-images)
  (codex:document :cl-value-noise))
