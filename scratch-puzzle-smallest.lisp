;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; https://www.b7j0c.org/blog/fb_interview_question.html

(in-package #:cl-user)

(use-package '(#:fiveam #:iterate))

(defun other-char (char1 char2)
  (case char1
    (#\a (if (eql char2 #\b) #\c #\b))
    (#\b (if (eql char2 #\a) #\c #\a))
    (#\c (if (eql char2 #\a) #\b #\a))))

;;; brute force solving
#+(or)
(defun solve-list (list &optional (length (length list)))
  (case length
    ((0 1)
     (values length list))
    (T
     (let ((best-length length)
           (best-list list))
       (iterate
         (for index from 0)
         (for (first second . rest) on list)
         (while second)
         (unless (eql first second)
           (multiple-value-bind (length list)
               (solve-list (append (subseq list 0 index)
                                   (list (other-char first second))
                                   rest))
             (when (< length best-length)
               (setf best-length length
                     best-list list)))))
       (values best-length best-list)))))

#+(or)
(solve-list (coerce string 'list) (length string))

;;; from the second solution to the problem:
;;; the minimum has to be a homogenous region

;;; a homogenous region is represented by a CONS pair with the CAR being
;;; the CHARACTER and the CDR being the INTEGER count

#+(or)
(deftype region ()
  '(cons character integer))

(defun single-char-region (char)
  (case char
    (#\a '(#\a . 1))
    (#\b '(#\b . 1))
    (#\c '(#\c . 1))))

(defun single-regions-from-string (string)
  "Returns a VECTOR of single regions, with each region being one character
of the given STRING."
  (map 'vector #'single-char-region string))

(defun combine-regions (region1 region2)
  "Returns a newly combined region or NIL."
  (destructuring-bind (char1 . count1) region1
    (destructuring-bind (char2 . count2) region2
      (cond
        ((eql char1 char2)
         (cons char1 (+ count1 count2)))
        ((and (eql 1 count1) (eql 1 count2))
         (single-char-region (other-char char1 char2)))))))

(defvar *cache*
  (make-hash-table :test 'equalp)
  "Caches results for a span of regions.  Keys are VECTORS of regions.")

(defun solve-regions (regions start end)
  "Returns a homogenous region or NIL if REGIONS is empty."
  (let ((length (- end start)))
    (case length
      (0)
      (1
       (aref regions start))
      (2
       (combine-regions
        (aref regions start)
        (aref regions (1+ start))))
      (T
       ;; it would be really nice to calculate the SXHASH from START to END instead of consing
       (let ((subseq (subseq regions start end)))
         (or (gethash subseq *cache*)
             (iterate
               (with best)
               (for index from (1+ start) below end)
               (let* ((first-half (solve-regions regions start index))
                      (second-half (solve-regions regions index end))
                      (combined (combine-regions first-half second-half)))
                 (when (and combined
                            (or (not best)
                                (< (cdr combined) (cdr best))))
                   (setf best combined)))
               (finally
                (return (setf (gethash subseq *cache*) best))))))))))

(defun solve (string &optional clrhashp)
  "Solves the puzzle for STRING.  Returns either a reduction or NIL if
STRING was empty.  Supply CLRHASHP if the computation should start fresh
without reusing results from an earlier calculation."
  (when clrhashp (clrhash *cache*))
  (solve-regions (single-regions-from-string string) 0 (length string)))

(def-test chars ()
  (is (eql #\a (other-char #\b #\c)))
  (is (eql #\b (other-char #\a #\c)))
  (is (eql #\c (other-char #\a #\b))))

(def-test test.1 ()
  (is (null (solve "" T)))
  (is (equal '(#\a . 1) (solve "a" T)))
  (is (equal '(#\c . 1) (solve "ab" T)))
  (is (equal '(#\a . 2) (solve "aa" T)))
  (is (eql 2 (cdr (solve "cab" T))))
  (is (eql 1 (cdr (solve "bcab" T))))
  (is (eql 5 (cdr (solve "ccccc" T))))
  (is (eql 1 (cdr (solve "aabcbccbaacaccabcbcab" T)))))

#+(or)
(run! '(and chars test.1))
;; ...........
;;  Did 11 checks.
;;     Pass: 11 (100%)
;;     Skip: 0 ( 0%)
;;     Fail: 0 ( 0%)
