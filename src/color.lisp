(in-package :vintage)


(defmacro defcolors (&rest colors)
  `(progn
    ,@(iterate (for n :from 0)
               (for (constant nil nil) :in colors)
               (collect `(defconstant ,constant ,n)))
    (defun init-colors ()
      ,@(iterate
          (for (constant fg bg) :in colors)
          (collect `(charms/ll:init-pair ,constant ,fg ,bg))))))

(defcolors
  (+white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
  (+blue-black+   charms/ll:COLOR_BLUE    charms/ll:COLOR_BLACK)
  (+cyan-black+   charms/ll:COLOR_CYAN    charms/ll:COLOR_BLACK)
  (+yellow-black+ charms/ll:COLOR_YELLOW  charms/ll:COLOR_BLACK)
  (+green-black+  charms/ll:COLOR_GREEN   charms/ll:COLOR_BLACK)
  (+red-black+    charms/ll:COLOR_RED     charms/ll:COLOR_BLACK)
  (+pink-black+   charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)

  (+black-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)
  (+black-blue+   charms/ll:COLOR_BLACK charms/ll:COLOR_BLUE)
  (+black-cyan+   charms/ll:COLOR_BLACK charms/ll:COLOR_CYAN)
  (+black-yellow+ charms/ll:COLOR_BLACK charms/ll:COLOR_YELLOW)
  (+black-green+  charms/ll:COLOR_BLACK charms/ll:COLOR_GREEN)
  (+black-red+    charms/ll:COLOR_BLACK charms/ll:COLOR_RED)
  (+black-pink+   charms/ll:COLOR_BLACK charms/ll:COLOR_MAGENTA))

(defmacro with-color ((canvas color) &body body)
  (once-only (canvas color)
    `(unwind-protect
       (progn
         (charms/ll:wattron (charms::window-pointer (window ,canvas))
                            (charms/ll:color-pair ,color))
         ,@body)
       (charms/ll:wattroff (charms::window-pointer (window ,canvas))
                           (charms/ll:color-pair ,color)))))
