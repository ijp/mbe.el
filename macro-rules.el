;;; macro-rules.el --- Macros by Example

;; Copyright (C) 2014  Ian Price

;; Author: Ian Price <ianprice90@googlemail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Supports r5rs patterns, except for vector patterns i.e.
;;
;;   (<pattern> ...)
;;   (<pattern> <pattern> ... . <pattern>)
;;   (<pattern> ... <pattern> <ellipsis>)

;;; Code:
(require 'cl-lib)
(require 'pcase)

(defun mbe-self-evaluating-p (object)
  (or (numberp object)
      (stringp object)
      (booleanp object)
      (keywordp object)))

(defvar mbe-ellipsis-object '...)

(defun mbe-ellipsis-p (object)
  (eql object mbe-ellipsis-object))

(defun mbe-pattern-p (sexp)
  (pcase sexp
    (`(,p ,(pred mbe-ellipsis-p))
     (mbe-pattern-p p))
    (`(,a . ,d)
     (and (mbe-pattern-p a) (mbe-pattern-p d)))
    ((pred symbolp) t)
    ((pred mbe-self-evaluating-p) t)
    (_ nil)))

(defun mbe-pattern-variables (sexp)
  ;; TODO: duplicate checking
  (pcase sexp
    (`(,p ,(pred mbe-ellipsis-p))
     (mbe-pattern-variables p))
    (`(,a . ,d)
     (append (mbe-pattern-variables a) (mbe-pattern-variables d)))
    (`() nil)
    ((pred symbolp) (list sexp))
    (_ nil)))

(defun mbe-compile-pattern-match (pattern match-var body)
  `(let ,(mbe-pattern-variables pattern)
     ,(mbe-compile-pattern-match* pattern match-var)
     ,@body))

(defun bad-match ()
  (message "bad match")
  (throw 'bad-match nil))

(defun mbe-compile-pattern-match* (pattern match-var)
  (pcase pattern
    (`(,p ,(pred mbe-ellipsis-p))
     `(progn
        (unless (listp ,match-var) (bad-match))
        (while ,match-var
          ,(let* ((mvar  (gensym match-var))
                  (pvars (mbe-pattern-variables p))
                  (gensyms (mapcar #'gensym pvars)))
             `(let ,gensyms
                (let ((,mvar (car ,match-var)) ,@pvars)
                  ,(mbe-compile-pattern-match* p mvar)
                  ,@(cl-map 'list (lambda (x y) `(setq ,x ,y)) gensyms pvars))
                ;; TODO: build in reverse, then reverse
                ,@(cl-map 'list (lambda (x y) `(setq ,x (append ,x (list ,y))))
                          pvars gensyms)
                (setq ,match-var (cdr ,match-var)))))))
    (`(,a . ,d)
     `(progn
        (unless (consp ,match-var) (bad-match))
        ,(let ((m1 (gensym match-var))
               (m2 (gensym match-var)))
           `(let ((,m1 (car ,match-var))
                  (,m2 (cdr ,match-var)))
              ,(mbe-compile-pattern-match* a m1)
              ,(mbe-compile-pattern-match* d m2)))))
    (`()
     `(when ,match-var (bad-match)))
    ((pred symbolp)
     `(setq ,pattern ,match-var))
    ((pred mbe-self-evaluating-p)
     ;; TODO: make equality configurable?
     `(unless (equal ,match-var ,pattern) (bad-match)))
    (_ (throw 'bad-pattern pattern))))


(defmacro mbe-destructuring-let (pattern val &rest body)
  (let ((value (gensym)))
    `(let ((,value ,val))
       (catch 'bad-match
         ,(mbe-compile-pattern-match pattern value body)))))


(mbe-destructuring-let
 (a b)
 (list 1 2)
 (list a b)) ; (1 2)

(mbe-destructuring-let
 (a b ...)
 (list 1 2 3 4 5)
 (list a b)) ; (1 (2 3 4 5))

(mbe-destructuring-let
 ((a b) ...)
 `((a 1) (b 2) (c 3))
 (list a b)) ; ((a b c) (1 2 3))


(mbe-destructuring-let
 ((a b ...) ...)
 `((a 1 "alpha" "beta") (b 2) (c 3 "gamma"))
 (list a b)) ; ((a b c) ((1 "alpha" "beta") (2) (3 "gamma")))




(provide 'macro-rules)
;;; macro-rules.el ends here
