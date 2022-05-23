;;; <file:emacsy-test.scm>=                                                 
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
;;; Finally, let's provide this as our testing preamble.                    
;;;                                                                         
;;;                                                                         
;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))

;;; <emacsy:test functions>=                                                
(define unit-tests '())

(define (register-test name func)
  (set! unit-tests (acons name func unit-tests)))

;;; The function register-test does the work, but we don't want to require  
;;; the user to call it, so we'll define a macro that will automatically    
;;; call it.                                                                
;;;                                                                         
;;;                                                                         
;;; <emacsy:test macro>=                                                    
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name args ...) expr ...)
     (begin (define* (name args ...)
        expr ...)
     (register-test 'name name)))))
;;; Finally, now we just need a way to run all the unit tests.              
;;;                                                                         
;;;                                                                         
;;; <emacsy:run tests>=                                                     
(define test-errors '())
(define (run-tests)
  (catch 'first-error
    (lambda () (for-each (lambda (elt)
                           (display "TEST: ")
                           (pretty-print elt)
                 (catch #t
                   (lambda ()
                     (with-throw-handler #t
                                         (lambda ()
                                           (apply (cdr elt) '()))
                                         (lambda args
                                           (set! test-errors (cons (car elt) test-errors))
                                           (format #t "Error in test ~a: ~a" (car elt) args)

                                           (backtrace))))
                   (lambda args
                     ;(throw 'first-error)
                     #f
                     )))
               (reverse unit-tests)))
    (lambda args
      #f)))
;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
(eval-when (compile load eval)
           (module-use! (current-module) (resolve-module '(emacsy)))) 

 
;;; Let's run these tests at the end.                                       
;;;                                                                         
;;;                                                                         
;;; <+ Test Postscript>=                                                    

(run-tests)
(check-report)
(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))

;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1)) 
