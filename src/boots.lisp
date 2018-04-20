(in-package :boots)

(declaim (optimize (debug 2) (safety 3)))


;;;; State --------------------------------------------------------------------
(defvar *layers* nil)
(defvar *screen-width* nil)
(defvar *screen-height* nil)


;;;; Types --------------------------------------------------------------------
(deftype dimension-designator ()
  '(or (eql :auto)
       (and fixnum (integer 0))
       (float 0.0 1.0)))

(deftype dimension ()
  '(and fixnum (integer 0)))

(deftype coordinate ()
  '(and fixnum (integer 0)))

(deftype function-designator ()
  '(or function symbol))


;;;; Data Model ---------------------------------------------------------------
(defclass* (base :conc-name "") ()
  ((desired-height :type dimension-designator)
   (desired-width :type dimension-designator)
   (actual-height :type dimension)
   (actual-width :type dimension)
   (row :type coordinate)
   (col :type coordinate)))


(defclass* (layer :conc-name "") (base)
  ((content :type (and base (not layer)))))


(defclass* (container :conc-name "") (base)
  ((contents :type sequence)))


(defclass* (stack :conc-name "") (container) ())
(defclass* (shelf :conc-name "") (container) ())

(defclass* (widget :conc-name "") (base) ())

(defclass* (canvas :conc-name "") (widget)
  ((drawing-function :type function-designator)
   (panel :type charms:panel)
   (window :type charms:window)))


;;;; Layers -------------------------------------------------------------------
(defun make-layer (width height content)
  (let ((layer (make-instance 'layer
                 :content content
                 :desired-width width
                 :desired-height height)))
    (resize-layer layer)
    layer))

(defun push-layer (layer)
  (push layer *layers*))

(defun pop-layer (layer)
  (let ((l (pop *layers*)))
    (assert (eq layer l) () "Expected to pop ~A, but popped ~A instead."
            layer l)
    (destroy layer)))


(defmacro with-layer ((&optional (width :auto) (height width))
                      content
                      &body body)
  (once-only (content)
    (with-gensyms (layer)
      `(let ((,layer (make-layer ,width ,height ,content)))
         (push-layer ,layer)
         (unwind-protect (progn ,@body)
           (pop-layer ,layer))))))


;;;; Constructors -------------------------------------------------------------
(defun make-stack (width height &rest contents)
  (make-instance 'stack
    :desired-height height
    :desired-width width
    :contents contents))

(defun make-shelf (width height &rest contents)
  (make-instance 'shelf
    :desired-height height
    :desired-width width
    :contents contents))

(defun make-canvas (width height drawing-function)
  (let* ((window (charms:make-window 1 1 0 0))
         (panel (charms:make-panel window)))
    (charms:enable-extra-keys window)
    (make-instance 'canvas
      :desired-height height
      :desired-width width
      :drawing-function drawing-function
      :panel panel
      :window window)))


(defmacro stack ((&key (width :auto) (height :auto)) &rest contents)
  `(make-stack ,width ,height ,@contents))

(defmacro shelf ((&key (width :auto) (height :auto)) &rest contents)
  `(make-shelf ,width ,height ,@contents))

(defmacro canvas ((&key (width :auto) (height :auto)) drawing-function)
  `(make-canvas ,width ,height ,drawing-function))


;;;; Destructors --------------------------------------------------------------
(defgeneric destroy (object))

(defmethod destroy ((layer layer))
  (destroy (content layer)))

(defmethod destroy ((container container))
  (map nil #'destroy (contents container)))

(defmethod destroy ((canvas canvas))
  (charms:destroy-panel (panel canvas))
  (charms:destroy-window (window canvas)))


;;;; Sizing -------------------------------------------------------------------
(defgeneric resize (object row col width height))

(defmethod resize ((object base) row col width height)
  (setf (row object) row
        (col object) col
        (actual-width object) width
        (actual-height object) height))


(defun normalize-float (total value)
  (if (floatp value)
    (round (* value total))
    value))

(defun normalize-floats (total values)
  (mapcar (curry #'normalize-float total) values))

(defun sum-fixed (values)
  (summation values :key (lambda (v)
                           (if (eq :auto v) 0 v))))

(defun count-autos (pairs)
  (count-if (curry #'eq :auto) pairs))


(defun distribute-autos (total values)
  (let ((n (count-autos values))
        (remaining (- total (sum-fixed values))))
    (multiple-value-bind (v extra) (floor remaining n)
      (iterate
        (with i = 0)
        (for value :in values)
        (collect (if (eq :auto value)
                   (if (<= (incf i) extra)
                     (1+ v)
                     v)
                   value))))))

(defun flush-stack (children heights row col width)
  (iterate
    (for r :first row :then (+ r h))
    (for child :in-whatever children)
    (for h :in heights)
    (resize child
            r col
            width h)))

(defun flush-shelf (children widths row col height)
  (iterate
    (for c :first col :then (+ c w))
    (for child :in-whatever children)
    (for w :in widths)
    (resize child
            row c
            w height)))


(defmethod resize ((stack stack) row col width height)
  (call-next-method)
  (flush-stack (contents stack)
               (-<> (contents stack)
                 (map 'list #'desired-height <>)
                 (normalize-floats height <>)
                 (distribute-autos height <>))
               row col width))

(defmethod resize ((shelf shelf) row col width height)
  (call-next-method)
  (flush-shelf (contents shelf)
               (-<> (contents shelf)
                 (map 'list #'desired-width <>)
                 (normalize-floats width <>)
                 (distribute-autos width <>))
               row col height))

(defmethod resize ((canvas canvas) row col width height)
  (call-next-method)
  (charms:resize-window (window canvas) width height)
  (charms:move-window (window canvas) col row)
  (charms:replace-panel-window (panel canvas) (window canvas)))


(defun resize-layer (layer)
  (flet ((resize-dimension (desired screen)
           (etypecase desired
             ((integer 0) (min desired screen))
             (float (round (* desired screen)))
             ((eql :auto) screen))))
    (let* ((h (resize-dimension (desired-height layer) *screen-height*))
           (w (resize-dimension (desired-width layer) *screen-width*))
           (r (truncate (- *screen-height* h) 2))
           (c (truncate (- *screen-width* w) 2)))
      (setf (actual-height layer) h
            (actual-width layer) w
            (row layer) r
            (col layer) c)
      (resize (content layer) r c w h))))

(defun resize-layers ()
  (map nil #'resize-layer *layers*))


;;;; Main ---------------------------------------------------------------------
(defun set-dimensions ()
  (setf (values *screen-width* *screen-height*)
        (charms:window-dimensions t)))


(defgeneric blit% (object))

(defmethod blit% ((layer layer))
  (blit% (content layer)))

(defmethod blit% ((container container))
  (map nil #'blit% (contents container)))

(defmethod blit% ((canvas canvas))
  (funcall (drawing-function canvas) canvas))

(defun blit ()
  (map nil #'blit% *layers*)
  (charms:update-panels)
  (charms:update))

(defun draw (canvas row col string &rest format-args)
  (nest
    (when (< row (actual-height canvas)))
    (let ((available-width (max 0 (- (actual-width canvas) col)))))
    (when (plusp available-width))
    (let* ((string (if format-args
                     (apply #'format nil string format-args)
                     string))
           (length (length string))
           (string (if (< length available-width)
                     string
                     (subseq string 0 available-width)))))
    (charms:write-string-at-point (window canvas) string col row)))

(defun clear (canvas)
  (charms:clear-window (window canvas)))


(defmacro with-boots (&body body)
  `(charms:with-curses ()
     (charms:disable-echoing)
     (charms:enable-raw-input :interpret-control-characters t)
     (charms:enable-extra-keys t)
     ;; (charms/ll:start-color)
     (charms/ll:curs-set 0)
     (charms:clear-window t)

     (set-dimensions)
     ,@body))


;;;; Scratch ------------------------------------------------------------------
(defvar *log* nil)

(defun l (&rest forms)
  (vector-push-extend forms *log*))

(defmacro with-logging (&body body)
  `(progn (setf *log* (make-array 16 :fill-pointer 0))
          ,@body))

(defparameter *title* "Hello!")

(defun fill-canvas (char canvas)
  (iterate
    (with s = (make-string (actual-width canvas) :initial-element char))
    (for row :from 0)
    (repeat (actual-height canvas))
    (ignore-errors (charms:write-string-at-point
                     (window canvas) s 0 row))))

(defun getch ()
  (charms:get-char t :ignore-error t))

(defun draw-title (canvas)
  (l 'drawing 'title *title*)
  (clear canvas)
  (draw canvas 0 0 *title*))

(defun draw-main (canvas)
  (l 'drawing 'main)
  (clear canvas)
  (draw canvas 0 0 "This is a test")
  (draw canvas 1 5 "This is another test"))

(defun title ()
  (with-layer ()
      (stack ()
        (canvas (:height 5) (curry #'fill-canvas #\x))
        (shelf ()
          (canvas (:width 0.1) (curry #'fill-canvas #\-))
          (canvas () (curry #'fill-canvas #\_))
          (canvas (:width 0.5) (curry #'fill-canvas #\#))))
    (with-layer (30)
        (shelf ()
          (canvas (:width 0.2) (curry #'fill-canvas #\!))
          (canvas () (curry #'fill-canvas #\$)))
      (blit)
      (getch))))

(defun run ()
  (with-logging
    (with-boots
      (title))))

