(require 'cl-lib)
(require 'mbe)

(cl-assert
 (equal
  '(1 2)
  (mbe-bind
   (a b)
   (list 1 2)
   (list a b))))

(cl-assert
 (equal
  '(1 (2 3 4 5))
  (mbe-bind
   (a b ...)
   (list 1 2 3 4 5)
   (list a b))))

(cl-assert
 (equal
  '((a b c) (1 2 3))
  (mbe-bind
   ((a b) ...)
   `((a 1) (b 2) (c 3))
   (list a b))))


(cl-assert
 (equal
  '((a b c) ((1 "alpha" "beta") (2) (3 "gamma")))
  (mbe-bind
   ((a b ...) ...)
   `((a 1 "alpha" "beta") (b 2) (c 3 "gamma"))
   (list a b))))

(cl-assert
 (equal '(7 6 (5 4))
  (mbe-bind
   (a b c ... 3 2 1)
   '(7 6 5 4 3 2 1)
   (list a b c))))

(cl-assert
 (equal '(1 2 (3 4 5 6 7) 8 9)
  (mbe-bind
   (a b c ... d e)
   '(1 2 3 4 5 6 7 8 9)
   (list a b c d e))))

(cl-assert
 (equal '(1 2 (3 4 5 6 7) 8 9 nil)
  (mbe-bind
   (a b c ... d e . f)
   '(1 2 3 4 5 6 7 8 9)
   (list a b c d e f))))

(cl-assert
 (equal '(1 2 (3 4 5 6) 7 8 9)
  (mbe-bind
   (a b c ... d e . f)
   '(1 2 3 4 5 6 7 8 . 9)
   (list a b c d e f))))

(mbe-defrule mylet (((var val) ...) body ...)
  (funcall (lambda (var ...) body ...) val ...))

(mbe-defrule mylet* (((var val) ...) body ...)
  (mylet*-helper (var ...) (val ...) (body ...)))

(mbe-defrules mylet*-helper
  ((nil nil body) (progn . body))
  (((var . vars) (val . vals) body)
   (mylet ((var val))
     (mylet*-helper vars vals body))))

(cl-assert
 (equal 5
  (mylet ((a 1) (b 2)) (+ b b a))))

(cl-assert
 (equal
  '(2 3 1)
  (mylet* ((a 1)
           (b 2)
           (c 3)
           (tmp a)
           (a b)
           (b c)
           (c tmp))
          (list a b c))))
