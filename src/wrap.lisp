(in-package :vintage.wrap)

(defun word-wrap-line (line width)
  (with-output-to-string (*standard-output*)
    (let ((pos 0)
          (spaces 0)
          (words (split-sequence:split-sequence #\space line)))
      (flet ((add (s)
               (incf pos (length s))
               (princ s))
             (linebreak ()
               (setf pos 0 spaces 0)
               (terpri)))
        (iterate
          (until (null words))
          (for word = (pop words))
          (for len = (length word))
          (cond
            ;; chomp leading whitespace
            ((and (zerop pos) (zerop len))
             nil)
            ;; if we have multiple spaces in a row, preserve them (maybe)
            ((zerop len)
             (incf spaces))
            ;; if we're dealing with a single word that's too long, reluctantly
            ;; split it into pieces
            ((and (zerop pos) (> len width))
             (add (subseq word 0 width))
             (linebreak)
             (push (subseq word width) words))
            ;; if this would send us beyond the limit, break
            ((> (+ spaces len pos) width)
             (linebreak)
             (push word words))
            ;; otherwise concat
            (t
             (add (make-string spaces :initial-element #\space))
             (add word)
             (setf spaces 1))))))))


(defun word-wrap-lines (strings width)
  (iterate (for s :in strings)
           (appending (split-sequence:split-sequence #\newline (word-wrap-line s width)))))

(defun word-wrap-string (string width)
  (format nil "~{~A~^~%~}"
          (iterate
            (for line in (split-sequence:split-sequence #\newline string))
            (collect (word-wrap-line line width)))))

(defun word-wrap (string-or-strings width)
  (etypecase string-or-strings
    (string (word-wrap-string string-or-strings width))
    (list (word-wrap-lines string-or-strings width))))
