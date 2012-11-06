;; -*- mode: lisp; coding: utf-8; -*-

;; based on http://en.wikipedia.org/wiki/Japanese_numerals
;; and a question about Java's setZeroDigit

(in-package #:cl-user)

(use-package '#:fiveam)

(defvar +japanese-numerals+
  "〇一二三四五六七八九十")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun range (from to)
    (loop for i from from to to collect i)))

(defun japanese-numeral (number)
  (case number
    (#.(range 0 10)
     (char +japanese-numerals+ number))
    (100 #\百)
    (1000 #\千)
    (10000 #\万)
    (#.(expt 10000 2) #\億)
    (#.(expt 10000 3) #\兆)
    (#.(expt 10000 4) #\京)
    (#.(expt 10000 5) #\垓)))

(defun write-japanese-numeral (number &optional (stream *standard-output*))
  (write-char (japanese-numeral number) stream))

(defun %print-small-japanese-numerals (number zeroes myriadp restp stream)
  (flet ((digit (digit)
           (write-japanese-numeral digit stream)))
    (multiple-value-bind (truncated remainder)
        (truncate number 10)
      (let ((recursep (not (zerop truncated))))
        (when recursep
          (%print-small-japanese-numerals
           truncated (1+ zeroes)
           myriadp (or restp (not (zerop remainder)))
           stream))
        (when (not (and (zerop remainder) recursep))
          (when (and (not (eql remainder 1)) (not (zerop zeroes)))
            (digit remainder))
          (when (and (>= zeroes 4)
                     (not recursep)
                     (eql remainder 1))
            (digit 1))
          (case zeroes
            (0
             (when (or (not (zerop remainder))
                       recursep)
               (digit remainder)))
            (1
             (digit 10))
            (2
             (digit 100))
            (3
             (when (and myriadp (not restp))
               (digit 1))
             (digit 1000))))))))

(defun %print-japanese-numerals (number zeroes myriadp restp stream)
  (loop
    for power from 5 downto 1
    for myriad = (expt 10000 power)
    do (multiple-value-bind (truncated remainder)
           (truncate number myriad)
         (unless (zerop truncated)
           (%print-small-japanese-numerals truncated 0 T NIL stream)
           (write-japanese-numeral myriad stream)
           (setf number remainder
                 restp T))))
  (%print-small-japanese-numerals
   number zeroes myriadp restp stream))

(defun print-japanese-numerals (number
                                &optional (stream *standard-output*))
  (typecase number
    (integer
     (if (< number 10000)
         (%print-small-japanese-numerals number 0 NIL NIL stream)
         (%print-japanese-numerals number 0 NIL NIL stream)))
    ;; ?
    (ratio)
    (float
     ;; traditional variant instead of place value system
     (let ((truncated (truncate number)))
       (%print-japanese-numerals truncated 0 NIL NIL stream)
       (write-char #\・ stream)
       ;; using the REMAINDER would give a different value
       (let ((float (princ-to-string number)))
         (%print-japanese-numerals (parse-integer float :start (1+ (position #\. float))) 0 NIL NIL stream)))))
  number)

(defun print-japanese-numerals-to-string (number)
  (with-output-to-string (stream)
    (print-japanese-numerals number stream)))

(defun compare-numbers (numbers)
  (dolist (list numbers)
    (is
     (string=
      (second list)
      (print-japanese-numerals-to-string
       (first list))))))

(def-test small-numbers ()
  (compare-numbers
   '((11 "十一")
     (17 "十七")
     (151 "百五十一")
     (302 "三百二")
     (469 "四百六十九")
     (2025 "二千二十五"))))

(def-test big-numbers ()
  (compare-numbers
   '((10000 "一万")
     (9836703 "九百八十三万六千七百三")
     (2036521801 "二十億三千六百五十二万千八百一"))))
