(in-package :vintage)


(defvar *player* nil)
(defvar *locations* nil)
(defvar *render-canvas* nil) ; todo rip this out


;;;; Messages -----------------------------------------------------------------
(defvar *messages* nil)
(defparameter *messages-limit* 20)


;;;; Assets -------------------------------------------------------------------
(defparameter *asset-intro* (read-lines "assets/intro" :omit-empty nil))
(defparameter *asset-title* (read-lines "assets/title" :omit-empty t))
(defparameter *asset-map* (read-lines "assets/map" :omit-empty t))


;;;; Looking ------------------------------------------------------------------
(defvar *looking* nil)
(defvar *look-row* nil)
(defvar *look-col* nil)


;;;; Cursor -------------------------------------------------------------------
(defvar *cursor-row* nil)
(defvar *cursor-col* nil)


;;;; Time ---------------------------------------------------------------------
(defvar *current-time* nil)
(defvar *since-last-tick* nil)
(defparameter *time-per-tick*
  (* 1/2 internal-time-units-per-second))


;;;; Terrain ------------------------------------------------------------------
(defvar *map-height* nil)
(defvar *map-width* nil)
(defvar *terrain* nil) ; array of strings


;;;; Map Locations ------------------------------------------------------------
(defvar *entrances* nil)
(defvar *in-front-of-register* nil)

