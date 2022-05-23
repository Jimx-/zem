;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:keymap-test.scm>=                                                 
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
(use-modules (emacsy keymap)
             (emacsy event)
             (oop goops))

(eval-when (compile load eval)
           ;; Some trickery so we can test private procedures.
           (module-use! (current-module) (resolve-module '(emacsy keymap))))

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <keymap:test>=                                                          
(check-true (make <keymap>))
;;; The core functionality of the keymap is being able to define and look   
;;; up key bindings.                                                        
;;;                                                                         
;;; @subsection Lookup Key                                                 
;;;                                                                         
;;; The procedure [[lookup-key]] return a keymap or symbol for a given      
;;; list of keys.  Consider this test keymap                                
;;;                                                                         
;;;                                                                         
;;; <keymap:test>=                                                          
(define (self-insert-command) #f) ;; make a fake command
(define (mouse-drag-region) #f) ;; make a fake command
(define (find-file-at-point) #f) ;; make a fake command
(define k (make-keymap))
(define-key k "a"       'self-insert-command)
(define-key k "mouse-1" 'mouse-drag-region)
(define-key k "C-x C-f" 'find-file-at-point)
;;; \noindent [[lookup-key]] should behave in the following way.            
;;;                                                                         
;;;                                                                         
;;; <keymap:test>=                                                          
(define (lookup-key* . args)
  (let ((result (apply lookup-key args)))
    (if (procedure? result) 
        (procedure-name result)
        result)))
(check (lookup-key* k '("a")) => 'self-insert-command-trampoline)
(check (lookup-key* k "a") => 'self-insert-command-trampoline)
(check (lookup-key k '("b")) => #f)
(check (lookup-key k "M-x b") => #f)
(check-true (keymap? (lookup-key k '("C-x"))))
(check (lookup-key k "C-x C-f a b" #f) => 2)
;;; Because delivering the errors using booleans and numbers is a little    
;;; cumbersome (and perhaps should be replaced with exceptions?),           
;;; sometimes we just want to see if there is something in the keymap.      
;;;                                                                         
;;;                                                                         
;;; <keymap:test>=                                                          
(check (lookup-key? k "C-x") => #f)
(check (lookup-key? k "C-x C-f") => #t)
(check (lookup-key? k "a") => #t)
;;; @subsection Define Key                                                 
;;;                                                                         
;;; The procedure [[define-key]] may return a number indicating an error,   
;;; or a keymap indicating it worked.                                       
;;;                                                                         
;;;                                                                         
;;; <keymap:test>=                                                          
;(check (define-key k (kbd "C-x C-f C-a C-b") 'nope) => 2)
;;; <keymap:test>=                                                          
(check-true  (keymap? (make <keymap>)))
(check-false (keymap? 1))
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
