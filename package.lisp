(defpackage :boots
  (:use :cl :iterate :losh :vintage.quickutils)
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

(defpackage :vintage
  (:use :cl :iterate :losh :vintage.quickutils)
  (:export))
