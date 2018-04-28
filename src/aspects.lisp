(in-package :vintage)

;;;; Location -----------------------------------------------------------------
(define-aspect loc
  (row :type (or null (integer 0)) :initform nil)
  (col :type (or null (integer 0)) :initform nil))

(defmacro with-loc (entity-and-symbols &body body)
  (destructuring-bind (entity &optional (row-symbol 'row) (col-symbol 'col))
      (ensure-list entity-and-symbols)
    `(with-accessors ((,row-symbol loc/row)
                      (,col-symbol loc/col))
       ,entity
       ,@body)))

(defun in-loc-bounds-p (row col)
  (and (in-range-p 0 col *map-width*)
       (in-range-p 0 row *map-height*)))

(defun initialize-locations ()
  (setf *locations* (make-array (list *map-height* *map-width*)
                     :initial-element nil)))

(defun loc-insert (entity)
  (with-loc entity
    (push entity (aref *locations* row col))))

(defun loc-remove (entity)
  (with-loc entity
    (deletef (aref *locations* row col) entity)))

(defun move (entity new-row new-col)
  (with-loc entity
    (when row (loc-remove entity))
    (setf row new-row
          col new-col)
    (when new-row (loc-insert entity))))

(defun entities-at (row col)
  (when (in-loc-bounds-p row col)
    (aref *locations* row col)))

(defmethod entity-created :after ((entity loc))
  (with-loc entity
    (when row (loc-insert entity))))

(defmethod entity-destroyed :after ((entity loc))
  (with-loc entity
    (when row (loc-remove entity))))


;;;; Renderable ---------------------------------------------------------------
(define-aspect renderable
  (glyph :type character)
  (attrs :type (integer 0) :initform 0)
  (fg-color :type color :initform +white+)
  (bg-color :type color :initform +black+))


(defgeneric rendering-data (entity))

(defmethod rendering-data ((entity renderable))
  (values (renderable/glyph entity)
          (renderable/fg-color entity)
          (renderable/bg-color entity)
          (renderable/attrs entity)))

(defun render (entity)
  (check-type entity renderable)
  (when (renderp entity)
    (nest
      (with-loc entity)
      (when row)
      (multiple-value-bind (glyph fg bg attrs) (rendering-data entity))
      (with-color (*render-canvas* fg bg attrs))
      (boots:draw *render-canvas* row col
                  (string glyph)))))

(defgeneric renderp (entity))

(defmethod renderp ((entity renderable))
  t)


;;;; Solid --------------------------------------------------------------------
(define-aspect solid)


;;;; Flavor -------------------------------------------------------------------
(define-aspect flavor
  (name :type string)
  (article :type (member :definite :indefinite :none) :initform :indefinite))

(defun name-with-article (object)
  (let ((name (flavor/name object)))
    (ecase (flavor/article object)
      (:none name)
      (:definite (concatenate 'string "the " name))
      (:indefinite (chancery:a name)))))


;;;; Carrying -----------------------------------------------------------------
(define-aspect carryable)

(define-aspect carrier
  (holding :initform nil :type (or null (and loc carryable))))

(define-with-macro (carrier :conc-name carrier/) holding)

(defun pick-up (carrier carryable)
  (with-carrier (carrier)
    (assert (null holding) ()
      "Carrier ~S cannot pick up ~S because it is already holding ~S."
      carrier carryable holding)
    (move carryable nil nil)
    (setf holding carryable)))

(defun put-down (carrier row col)
  (with-carrier (carrier)
    (assert (not (null holding)) ()
      "Carrier ~S is not holding anything to put down." carrier)
    (prog1 holding
      (move holding row col)
      (setf holding nil))))


;;;; Antique ------------------------------------------------------------------
(define-aspect antique
  (condition :type (single-float 0.0 1.0))
  (material :type keyword)
  (manufactured :type local-time:timestamp))


;;;; Containable --------------------------------------------------------------
(define-aspect containable)


;;;; Container ----------------------------------------------------------------
(define-aspect container
  (contents :type list :initform nil)
  (capacity :type (integer 1))
  (on-or-in :type string))

(defun fullp (container)
  (= (length (container/contents container))
     (container/capacity container)))

(defun emptyp (container)
  (zerop (length (container/contents container))))

(defun containsp (container object)
  (ensure-boolean (member object (container/contents container))))

(defun place-in (container object)
  (check-type container container)
  (check-type object containable)
  (assert (not (fullp container)))
  (when (loc? object)
    (move object nil nil))
  (push object (container/contents container)))

(defun remove-from (container object)
  (check-type container container)
  (check-type object containable)
  (assert (containsp container object))
  (removef (container/contents container) object))


;;;; Purse --------------------------------------------------------------------
(define-aspect purse
  (dollars :type (integer 0)))


;;;; Ticking ------------------------------------------------------------------
(define-aspect ticking)

(defgeneric tick (entity))


;;;; Brain --------------------------------------------------------------------
(define-aspect brain
  (state :type keyword :initform :start))

(defgeneric brain-state! (entity state))

(defun act (entity)
  (message (brain/state entity))
  (brain-state! entity (brain/state entity)))

(defun transition-brain (entity state &optional immediately)
  (setf (brain/state entity) state)
  (when immediately
    (funcall #'brain-state! entity state)))
