(defpackage :boots
  (:use :cl :iterate :losh :lj.quickutils)
  (:shadow :fill)
  (:export
    :with-boots

    :with-layer
    :stack
    :shelf
    :canvas

    :read-event
    :read-event-no-hang

    :draw
    :clear
    :fill
    :blit

    :width
    :height))

(defpackage :lj
  (:use :cl :iterate :losh :lj.quickutils)
  (:export))
