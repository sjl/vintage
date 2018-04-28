(in-package :vintage)


;;;; Dates --------------------------------------------------------------------
(defun random-age-in-years ()
  (abs (random-gaussian-integer 0 400)))

(defun random-date ()
  (-<> (local-time:today)
    (local-time:timestamp- <> (random-age-in-years) :year)
    (local-time:timestamp- <> (random-range 0 365) :day))) ; close enough


;;;; Colors -------------------------------------------------------------------
(defun to-screen-color (material)
  (ecase material
    (:ceramic (values +white+ +bold+))
    (:glass +cyan+)
    (:steel +white+)
    (:bronze +yellow+)))


;;;; Vase ---------------------------------------------------------------------
(define-entity vase (loc flavor renderable antique solid carryable containable))


(chancery:define-rule (vase-material :distribution (:zipf :exponent 1.5))
  :ceramic
  :glass
  :steel
  :bronze)


(defun make-vase (row col)
  (let ((material (vase-material)))
    (multiple-value-bind (screen-color screen-attrs)
        (to-screen-color material)
      (create-entity 'vase
        :loc/row row
        :loc/col col
        :flavor/name (format nil "~(~A vase~)" material)
        :renderable/glyph #\v
        :renderable/fg-color screen-color
        :renderable/attrs (or screen-attrs 0)
        :antique/manufactured (random-date)
        :antique/material material
        :antique/condition (random-range 0.0 1.0)))))


;;;; Main ---------------------------------------------------------------------
(chancery:define-rule random-antique
  'make-vase)


(defun generate-antique (row col)
  (funcall (random-antique) row col))


