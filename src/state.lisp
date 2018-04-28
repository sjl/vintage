(in-package :vintage)

(defvar *player* nil)
(defvar *locations* nil)
(defvar *render-canvas* nil) ; todo rip this out
(defvar *messages* nil)
(defparameter *messages-limit* 20)

(defparameter *asset-intro* (read-lines "assets/intro" :omit-empty nil))
(defparameter *asset-title* (read-lines "assets/title" :omit-empty t))
(defparameter *asset-map* (read-lines "assets/map" :omit-empty t))

(defvar *map-height* nil)
(defvar *map-width* nil)

(defvar *terrain* nil) ; array of strings
(defvar *terrain-colors* nil)


(defvar *looking* nil)

(defvar *look-row* nil)
(defvar *look-col* nil)

(defvar *cursor-row* nil)
(defvar *cursor-col* nil)

(defvar *current-time* nil)

(defparameter *tick-time*
  (* 1))
