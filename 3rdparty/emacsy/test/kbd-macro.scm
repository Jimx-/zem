;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:kbd-macro-test.scm>=                                              
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
(use-modules (emacsy kbd-macro)
             (emacsy event)
             (emacsy command)     
             (emacsy klecl)
             (oop goops)
             (check))

(use-private-modules (emacsy kbd-macro))

(set! emacsy-interactive? #t)

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; Let's set up a command to test our functionality with.                  
;;;                                                                         
;;;                                                                         
;;; <kbd-macro:test>=                                                       
(define test-command-called 0)
(define test-keymap (make-keymap))
(define-interactive (test-command)
  (incr! test-command-called))

(define-key test-keymap (kbd "a") 'test-command)
(set! default-klecl-maps (lambda () (list test-keymap)))

(check test-command-called => 0)
(kmacro-start-macro)
(emacsy-key-event #\a)
(emacsy-key-event #\b) ;; this executes no command.
(primitive-command-loop (lambda args #f))
(primitive-command-loop (lambda args #f))
(kmacro-end-macro)
(check test-command-called => 1)
(check (map command-char last-kbd-macro) => '(#\b #\a))
(execute-kbd-macro last-kbd-macro)
(check test-command-called => 2)
;;; <kbd-macro:test>=                                                       
(check test-command-called => 2)
(execute-temporal-kbd-macro last-kbd-macro)
(primitive-command-loop (lambda args #f))
(check test-command-called => 3)
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
