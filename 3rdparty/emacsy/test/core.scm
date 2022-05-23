;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:core-test.scm>=                                                   
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
(use-modules (check)
             (emacsy core)
             (emacsy event)
             (emacsy klecl)
             (oop goops)
             (srfi srfi-11))

(use-private-modules (emacsy core))
(set! emacsy-interactive? #t)

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <core:test>=                                                            
(set! emacsy-interactive? #f)
(check (eval-expression '(+ 1 2)) => 3)
(set! emacsy-interactive? #t)
;;; One problem with this is I'd like to give completing-read a list of     
;;; objects that will be converted to strings, but I'd like to get the      
;;; object out rather than the string.  I want something like this:         
;;;                                                                         
;;;                                                                         
;;; <core:test>=                                                            
(check (let* ((symbols '(aa ab c d)))
   (let-values
       (((to-string from-string) (object-tracker symbol->string)))
     (map from-string (all-completions "a" (map to-string symbols))))) => '(aa ab))
;;; We need to be able to deal with exceptions gracefully where ever they   
;;; may pop up.                                                             
;;;                                                                         
;;;                                                                         
;;; <core:test>=                                                            
(define (good-hook)
  #t)
(define (bad-hook)
  (throw 'some-error))
(define my-hook (make-hook 0))

(check-throw (run-hook my-hook) => 'no-throw)
(check-throw (emacsy-run-hook my-hook) => 'no-throw)
(check (emacsy-run-hook my-hook) => #t)
(add-hook! my-hook good-hook)
(check-throw (emacsy-run-hook my-hook) => 'no-throw)
(add-hook! my-hook bad-hook)
(check-throw (run-hook my-hook) => 'some-error)
(check-throw (emacsy-run-hook my-hook) => 'no-throw)
(check (emacsy-run-hook my-hook) => #f)
;;; <core:test>=                                                            
(emacsy-discard-input!)
;(emacsy-key-event #\a)
(define mouse-event #f)
(agenda-schedule (colambda () 
                           (format #t "START~%")
                           (set! mouse-event (read-from-mouse))
                           (format #t "END~%")))
;(with-blockable )
;(block-tick)
;(check mouse-event => #f)
;(update-agenda)
(emacsy-mouse-event #(0 0) 1 'down)
(update-agenda)
(check-true mouse-event)
;(block-tick)
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
