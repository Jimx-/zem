;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:klecl-test.scm>=                                                  
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
(use-modules (emacsy klecl)
             (emacsy event)
             (check)
             (oop goops))

(use-private-modules (emacsy klecl))

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <klecl:test>=                                                           
(define last-event #f)
(codefine (test-read-event)
  (set! last-event (read-event)))
;(with-blockable (test-read-event))
(agenda-schedule test-read-event)
;(check (blocking?) => #t)
(set! emacsy-interactive? #t)
(check last-event => #f)
;(block-tick)
(update-agenda)
(check last-event => #f)
(emacsy-key-event #\a)

(update-agenda)
;(check last-event => #f)
;(check (blocking?) => #f)
(check (command-char last-event) => #\a)
;(clear-agenda)
;;; Since we have no keymaps defined, [[read-key-sequence]] should quickly  
;;; return any single key inputs.                                           
;;;                                                                         
;;;                                                                         
;;; <klecl:test>=                                                           
(define last-key-seq #f)
(codefine (read-key-sequence*)
  (set! last-key-seq #f)
  (set! last-key-seq (map command-char (read-key-sequence))))
;(with-blockable (read-key-sequence*))
(agenda-schedule read-key-sequence*)
(emacsy-key-event #\a)
;(block-tick)
(update-agenda)
(check last-key-seq => '(#\a))
(update-agenda)
(check last-key-seq => '(#\a))
;;; \noindent However, if we add a keymap with only the sequence            
;;; \verb|a b c|, we will see that it'll behave differently.                
;;;                                                                         
;;;                                                                         
;;; <klecl:test>=                                                           
(define (no-command) #f)
(define test-keymap (make-keymap))
(set! default-klecl-maps (lambda () (list test-keymap)))
(define-key test-keymap "a b c" 'no-command)
;(with-blockable (read-key-sequence*))
(agenda-schedule read-key-sequence*)
(emacsy-key-event #\a)
;(block-tick)
(update-agenda)
(check last-key-seq => #f) ;; Not enough keys to return.
;;; Let's test a sequence that is not in the keymap.                        
;;;                                                                         
;;;                                                                         
;;; <klecl:test>=                                                           
(emacsy-key-event #\z)
(update-agenda)
(check last-key-seq => '(#\a #\z)) ;; No way "a z" is an actual key-sequence.
;;; <klecl:test>=                                                           
;(with-blockable (read-key-sequence*))
(agenda-schedule read-key-sequence*)
(emacsy-key-event #\a)
(emacsy-key-event #\b)
;(block-tick)
(update-agenda)
(check last-key-seq => #f) ;; Not enough keys to return yet.
(emacsy-key-event #\c)
(update-agenda)
(check last-key-seq => '(#\a #\b #\c)) ;; Got it!
;;; Let's test keyboard quitting.                                           
;;;                                                                         
;;;                                                                         
;;; <klecl:test>=                                                           
(define-key test-keymap "q" 'keyboard-quit)
;(with-blockable (read-key-sequence*))
(agenda-schedule read-key-sequence*)
(emacsy-key-event #\a)
;(block-tick)
(update-agenda)
(check last-key-seq => #f) ;; Not enough keys to return yet.
(emacsy-key-event #\q)
;(block-tick)
(update-agenda)
(check last-key-seq => '(#\a #\q)) ;; Got it!
;;; <klecl:test>=                                                           
(define my-command-count 0)
(define-interactive (my-command)
  (incr! my-command-count))
(define-key test-keymap "c" 'my-command)
(emacsy-key-event #\c)
(check my-command-count => 0)
(agenda-schedule (colambda () (primitive-command-tick)))
(update-agenda)
;(with-blockable (primitive-command-tick))
(check my-command-count => 1)
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
