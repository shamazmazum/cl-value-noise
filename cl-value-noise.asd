(defsystem :cl-value-noise
  :name :cl-value-noise
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :description "Three-dimensional value noise"
  :licence "2-clause BSD"
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "value-noise")))

(defsystem :cl-value-noise/doc
  :name :cl-value-noise/doc
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :description "Document builder for cl-value-noise"
  :licence "2-clause BSD"
  :pathname "docs/"
  :serial t
  :components ((:file "package")
               (:file "build"))
  :depends-on (:imago/jpeg-turbo
               :codex
               :array-operations
               :cl-value-noise))
