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
    :border

    :width
    :height

    :move-cursor

    :*global-input-hook*

    ))

(defpackage :vintage.wrap
  (:use :cl :iterate :losh :vintage.quickutils)
  (:export
    :word-wrap))

(defpackage :vintage
  (:use :cl :iterate :losh :vintage.quickutils
    :beast
    :vintage.wrap)
  (:export
    :run
    :toplevel))
