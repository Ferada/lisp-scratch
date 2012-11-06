(in-package #:cl-user)

(defun map-reduce (map-function reduce-function sequence &rest sequences)
  (reduce reduce-function (apply #'map (type-of sequence) map-function sequence sequences)))

(defun sum (function sequence &rest sequences)
  (apply #'map-reduce function #'+ sequence sequences))

(sqrt (reduce #'+ (map 'list (lambda (x y) (* (decf x y) x)) '(1 0 0) '(0 1 0))))
(sqrt (apply #'+ (mapcar (lambda (x y) (* (decf x y) x)) '(1 0 0) '(0 1 0))))

(sqrt (sum (lambda (x y) (* (decf x y) x)) '(1 0 0) '(0 1 0)))
