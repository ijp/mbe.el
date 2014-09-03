(assert
 (equal
  '(1 2)
  (mbe-destructuring-let
   (a b)
   (list 1 2)
   (list a b))))

(assert
 (equal
  '(1 (2 3 4 5))
  (mbe-destructuring-let
   (a b ...)
   (list 1 2 3 4 5)
   (list a b))))

(assert
 (equal
  '((a b c) (1 2 3))
  (mbe-destructuring-let
   ((a b) ...)
   `((a 1) (b 2) (c 3))
   (list a b))))


(assert
 (equal
  '((a b c) ((1 "alpha" "beta") (2) (3 "gamma")))
  (mbe-destructuring-let
   ((a b ...) ...)
   `((a 1 "alpha" "beta") (b 2) (c 3 "gamma"))
   (list a b))))                        ; 

(defrule mylet (((var val) ...) body ...)
  (funcall (lambda (var ...) body ...) val ...))

(defrule mylet* (((var val) ...) body ...)
  (mylet*-helper (var ...) (val ...) (body ...)))

(defrules mylet*-helper
  ((nil nil body) (progn . body))
  (((var . vars) (val . vals) body)
   (mylet ((var val))
     (mylet*-helper vars vals body))))

(assert
 (equal 5
  (mylet ((a 1) (b 2)) (+ b b a))))

(assert
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
