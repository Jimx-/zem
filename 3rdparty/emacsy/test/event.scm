;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:event-test.scm>=                                                  
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
(use-modules (emacsy event)
             (oop goops)
             )

(eval-when (compile load eval)
           ;; Some trickery so we can test private procedures.
           (module-use! (current-module) (resolve-module '(emacsy event))))

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <event:test>=                                                           
(check-true (make <key-event> #:command-char #\a))
;;; <event:test>=                                                           
(check (strip-off-modifier-keys "C-a") => '((control) "a"))
(check (strip-off-modifier-keys "a") => '(() "a"))
(check (strip-off-modifier-keys "asdf") => '(() "asdf"))
;;; <event:test>=                                                           
(check (modifier-char->symbol #\S) => 'shift)
(check (modifier-char->symbol #\X) => #f)
;;; <event:test>=                                                           
(check-true (memq 'kbd-entry->key-event (alist-keys kbd-converter-functions)))
;;; One issue we have with the above is the following:                      
;;;                                                                         
;;;                                                                         
;;; <event:test>=                                                           
(check (modifier-keys (kbd-entry->key-event "C-C-C-x")) => '(control control control))
;;; Let's test our canonization of a properly formed but non-canonical event.
;;;                                                                         
;;;                                                                         
;;; <event:test>=                                                           
(let ((key-event (kbd-entry->event "S-C-C-S-a")))
  (check (modifier-keys key-event) => '(shift control control shift))
  (check (command-char key-event) => #\a)
  (canonize-event! key-event)
  (check (modifier-keys key-event) => '(control))
  (check (command-char key-event) => #\A))
;;; <event:test>=                                                           
(check (kbd "S-C-C-S-a") => '("C-A"))
(check (kbd "S-C-C-S-A") => '("C-A"))
;;; <event:test>=                                                           
(check (event->kbd (make <key-event> #:command-char #\a)) => "a")
;;; <event:test>=                                                           
(check (event->kbd (make <key-event> #:command-char #\a 
                         #:modifier-keys '(control))) => "C-a")
;;; <event:test>=                                                           
(check (kbd "mouse-1") => '("mouse-1"))
(check (kbd "S-S-mouse-1") => '("S-mouse-1"))
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
