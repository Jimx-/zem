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

;;; Commentary:

;; @node Advice
;; @section Advice

;; @quotation
;; Wise men don't need advice.  Fools won't take it.
;; @author Benjamin Franklin
;; @end quotation

;; @c @quotation
;; @c Nobody can give you wiser advice than yourself.
;; @c @author Marcus Tullius Cicero
;; @c @end quotation

;; @quotation
;; No enemy is worse than bad advice.
;; @author Sophocles
;; @end quotation

;; Emacs has a facility to define ``advice'' these are pieces of code
;; that run before, after, or around an already defined function.  This
;; @url{"http://electricimage.net/cupboard/2013/05/04/on-defadvice/",article}
;; provides a good example.

;;; Code:

(define-module (emacsy advice)
  #:use-module (srfi srfi-9))

;; How will this work?  Before we try to make the macro, let's focus on
;; building up the functions.  We want to have a function that we can
;; substitute for the original function which will have a number of
;; before, after, and around pieces of advice that can be attached to it.
(define-record-type <record-of-advice>
  (make-record-of-advice original before around after)
  record-of-advice?
  (original   advice-original)
  (before     advice-before    set-advice-before!)
  (around     advice-around    set-advice-around!)
  (after      advice-after     set-advice-after!))

;;.
(define-record-type <piece-of-advice>
  (make-piece-of-advice procedure name class priority flag)
  piece-of-advice?
  (procedure poa-procedure)
  (name      poa-name) ;; symbol not string
  (class     poa-class    set-poa-class!)
  (priority  poa-priority set-poa-priority!)
  (flag      poa-flag     set-poa-flag!))

(define next-advice-func (make-fluid))

(define (make-advising-function advice)
  (lambda args
    (let ((around-advices (append (advice-around advice)
                                  (list (make-piece-of-advice
                                         (advice-original
                                          advice)
                                         'original
                                         'bottom
                                         0
                                         'activate))))
          (result #f))
     (define (my-next-advice)
       (if (null? around-advices)
           (throw 'next-advices-drained)
           (let ((next-one-around (car around-advices)))
             (set! around-advices (cdr around-advices))
             (apply (poa-procedure next-one-around) args))))
     ;; This could be done more cleanly.  For instance,
     ;; If one calls (next-advice) more than once,
     ;; they drain all the advice rather than calling
     ;; the same advice again, which is probably
     ;; the more correct behavior.

     (for-each (lambda (before)
                 (apply (poa-procedure before) args))
               (advice-before advice))

     (set! result (with-fluid* next-advice-func my-next-advice
                               (lambda ()
                                 (next-advice))))
     (for-each (lambda (after)
                 (apply (poa-procedure after) result args))
               (advice-after advice))
     result)))

(define (next-advice)
  (if (fluid-bound? next-advice-func)
      ((fluid-ref next-advice-func))
      (throw 'no-next-advice-bound)))
