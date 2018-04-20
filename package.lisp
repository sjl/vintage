(defpackage :boots
  (:use :cl :iterate :losh :lj.quickutils)
  (:export))

(defpackage :lj
  (:use :cl :iterate :losh :boots :lj.quickutils)
  (:export))
