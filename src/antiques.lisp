(in-package :vintage)


;;;; Dates --------------------------------------------------------------------
(defun random-age-in-years ()
  (abs (random-gaussian-integer 0 400)))

(defun random-date ()
  (-<> (local-time:today)
    (local-time:timestamp- <> (random-age-in-years) :year)
    (local-time:timestamp- <> (random-range 0 365) :day))) ; close enough


;;;; Colors -------------------------------------------------------------------
(defun to-screen-color (color)
  (ecase color
    (:red +red-black+)
    (:green +green-black+)
    (:blue +blue-black+)
    (:yellow (values +yellow-black+ +bold+))
    (:gold (values +yellow-black+ +bold+))
    (:brown +yellow-black+)
    (:pink +pink-black+)
    (:white (values +white-black+ +bold+))
    (:purple +blue-black+)))


;;;; Vase ---------------------------------------------------------------------
(define-entity vase (loc flavor renderable antique solid))

(chancery:define-rule vase-color
  :blue
  :red
  :white
  :gold)

(chancery:define-rule (vase-material :distribution :zipf)
  :ceramic
  :glass
  :steel
  :bronze)

(defun make-vase (row col)
  (let ((color (vase-color))
        (material (vase-material)))
    (multiple-value-bind (screen-color screen-attrs)
        (to-screen-color color)
      (create-entity 'vase
        :loc/row row
        :loc/col col
        :flavor/name (format nil "~(a ~A ~A vase~)" color material)
        :renderable/glyph #\v
        :renderable/color screen-color
        :renderable/attrs (or screen-attrs 0)
        :antique/date (random-date)
        :antique/color color
        :antique/material material
        :antique/condition (random-range 0.0 1.0)))))


;;;; Main ---------------------------------------------------------------------
(chancery:define-rule random-antique
  'make-vase)


(defun generate-antique (row col)
  (funcall (random-antique) row col))


