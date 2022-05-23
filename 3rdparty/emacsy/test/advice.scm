;;; <file:advice-test.scm>=                                                 
;;; @subsection Legal Stuff                                                
;;;                                                                         
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
(use-modules (emacsy advice)
             (emacsy event)
             (emacsy klecl)
             (oop goops)
             (srfi srfi-11))

(eval-when (compile load eval)
           ;; Some trickery so we can test private procedures.
           (module-use! (current-module) (resolve-module '(emacsy advice))))

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; To test this functionality, we're going to make some counter            
;;; procedures.                                                             
;;;                                                                         
;;;                                                                         
;;; <advice:test>=                                                          
(define (my-orig-func x)
  (+ x 1))

(define (make-counter)
  (let ((x 0))
    (lambda args
      (if (and (= (length args) 1) (eq? (car args) 'count))
          x
          (begin (set! x (+ x 1))
                 (car args))))))

(define a-before (make-counter))
;;; Let's make an identity advice procedure.  It does nothing, but it does  
;;; wrap around the function.                                               
;;;                                                                         
;;; <advice:test>=                                                          
(define advice (make-record-of-advice my-orig-func '() '() '()))

(define advised-func (make-advising-function advice))
(check (a-before 'count) => 0)
(check (my-orig-func 1) => 2)
(check (advised-func 1) => 2)
(check (a-before 'count) => 0)
;;; Let's test this with the simple functionality of having a piece of      
;;; before advice.                                                          
;;;                                                                         
;;;                                                                         
;;; <advice:test>=                                                          
(define advice (make-record-of-advice my-orig-func (list (make-piece-of-advice a-before 'a-before 'before 0 'activate)) '() '()))

(define advised-func (make-advising-function advice))
(check (a-before 'count) => 0)
(check (my-orig-func 1) => 2)
(check (advised-func 1) => 2)
(check (a-before 'count) => 1)
;;; Let's check the after advice.                                           
;;;                                                                         
;;; <advice:test>=                                                          
(define a-after (make-counter))
(define advice (make-record-of-advice my-orig-func '() '() 
                                      (list (make-piece-of-advice a-after 'a-after 'after 0 'activate))))

(define advised-func (make-advising-function advice))
(check (a-after 'count) => 0)
(check (my-orig-func 1) => 2)
(check (advised-func 1) => 2)
(check (a-after 'count) => 1)
;;; Let's check the after advice.                                           
;;;                                                                         
;;; <advice:test>=                                                          
(define a-around (lambda args
                  (next-advice)
                  1))
(define advice (make-record-of-advice my-orig-func '() (list (make-piece-of-advice a-around 'a-around 'around 0 'activate)) '()))

(define advised-func (make-advising-function advice))
(check (my-orig-func 1) => 2)
(check (advised-func 1) => 1)
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
