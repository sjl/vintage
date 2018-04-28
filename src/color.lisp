(in-package :vintage)

(deftype color ()
  '(integer 0 7))

(deftype color-pair ()
  '(unsigned-byte 6))

(defconstant +black+   0)
(defconstant +white+   1)
(defconstant +blue+    2)
(defconstant +cyan+    3)
(defconstant +yellow+  4)
(defconstant +green+   5)
(defconstant +red+     6)
(defconstant +magenta+ 7)

(defconstant +bold+ charms/ll:A_BOLD)

(defparameter *colors*
  `((,+black+   ,charms/ll:COLOR_BLACK)
    (,+white+   ,charms/ll:COLOR_WHITE)
    (,+blue+    ,charms/ll:COLOR_BLUE)
    (,+cyan+    ,charms/ll:COLOR_CYAN)
    (,+yellow+  ,charms/ll:COLOR_YELLOW)
    (,+green+   ,charms/ll:COLOR_GREEN)
    (,+red+     ,charms/ll:COLOR_RED)
    (,+magenta+ ,charms/ll:COLOR_MAGENTA)))

(declaim (ftype (function (color color) color-pair) colors-to-pair-number))
(defun colors-to-pair-number (fg bg)
  (declare (optimize speed (debug 1) (safety 1)))
  (logior (ash fg 3) bg))

(defun initialize-colors ()
  (assert (>= charms/ll:*color-pairs* (expt 2 6)) ()
    "Terminal does not support enough colors.")
  (iterate
    (for (fg fg-constant) :in *colors*)
    (iterate
      (for (bg bg-constant) :in *colors*)
      (charms/ll:init-pair (colors-to-pair-number fg bg)
                           fg-constant
                           bg-constant))))

(defmacro with-color ((canvas fg bg &rest attributes) &body body)
  (with-gensyms (window attrs)
    `(let ((,attrs (logior (charms/ll:color-pair (colors-to-pair-number ,fg ,bg))
                           ,@attributes))
           (,window (charms::window-pointer (boots::window ,canvas))))
       (unwind-protect
           (progn
             (charms/ll:wattron ,window ,attrs)
             (message ,attrs)
             ,@body)
         (charms/ll:wattroff ,window ,attrs)))))
