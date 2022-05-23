;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;;
;;; This file is part of Emacsy.
;;;
;;; Emacsy is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Emacsy is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.
;;; <windows:Module>=
(define-module (emacsy vector-math)
  #:export (make-identity-matrix
            matrix.))

(define (make-identity-matrix dimension)
  (case dimension
    ((1) #(1))
    ((2) #(1 0 0 1))
    ((3) #(1 0 0 0 1 0 0 0 1))))

(define (matrix. a b)
  a)

;;; @ @enumerate
;;; @item vector-component-usage

;;;  The component of $\bv a$ in the $\bv b$ direction.
;;; @align*
;;;   \comp_\bv b \bv a &= \bv a \cdot \bhv b \\
;;;   &= \frac{\bv a \cdot \bv b}{||\bv b||}
;;; @end align*

;;; <<Vector Definitions>>=
(define (vector-component a b)
    ;(string-trim-both
    ;; #" <<vector-component-usage>> "#
    ;char-set:whitespace)
 (/ (vector-dot a b) (vector-norm b)))
;; @ Tried to define vector-component-usage to "Scalar projection"

;; @item Vector projection

;;   The vector projection of $\bv a$ on $\bv b$.
;;   @align*
;;     \proj_\bv b \bv a &= a_1 \bhv b \\
;;     a_1 &= \comp_\bv b \bv a
;;   @end align*

;; <<Vector Definitions>>=
(define (vector-projection a b)
 (vector* (vector-component a b) (vector-normalize b)))
;; @ @end enumerate
