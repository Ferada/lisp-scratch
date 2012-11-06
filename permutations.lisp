;; scratch buffer created 2012-10-26 at 18:40:12

(in-package #:cl-user)

(use-package '#:fiveam)

(defun permutations (set &optional (n (length set)))
  (cond
    ((<= n 0))
    ((= n 1)
     (mapcar #'list set))
    (T
     (mapcan (lambda (rest)
               (mapcar (lambda (x) (cons x rest)) set))
             (permutations set (1- n))))))

(def-test permutations ()
  (is (null (permutations '())))
  (is (equal '((1)) (permutations '(1))))
  (is (null (set-difference '((1 1) (1 2) (2 1) (2 2)) (permutations '(1 2)) :test #'equal))))
