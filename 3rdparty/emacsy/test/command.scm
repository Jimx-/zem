;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:command-test.scm>=                                                
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
(use-modules (emacsy command)
             (emacsy event)
             (oop goops))

(eval-when (compile load eval)
           ;; Some trickery so we can test private procedures.
           (module-use! (current-module) (resolve-module '(emacsy command))))

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <command:test>=                                                         
(define test-cmd (lambda-cmd args 1))
(define (test-cmd-2) 2)
(define-cmd (test-cmd-3) 3)
(check (procedure-documentation test-cmd-3) => #f)
(check (test-cmd) => 1)
(check-true (command? test-cmd))
(check-true (command? test-cmd-2))
(check-true (command? test-cmd-3))
(check (assq-ref (procedure-properties test-cmd) 'command-name) => #f)
(check (assq 'command-name (procedure-properties test-cmd-2)) => #f)
(check (command-name test-cmd) => 'proc)
(check (command-name test-cmd-2) => 'test-cmd-2)
(check (command-name test-cmd-3) => 'test-cmd-3)
;;; <command:test>=                                                         
(define-cmd (test-who-am-i?) 
  "test-who-am-i? documentation"
  (let ((w (what-command-am-i?)))
    1
    w))
(check (command-name test-who-am-i?) => 'test-who-am-i?)
(check (test-who-am-i?) => 'test-who-am-i?)
(check (procedure-documentation test-who-am-i?) => "test-who-am-i? documentation")
;;; <command:test>=                                                         
(define-cmd (foo)
  (if (called-interactively?)
      'interactive
      'non-interactive))
(check (command? 'foo) => #f)
(check (command? foo) => #t)
(check (command-name foo) => 'foo)
(check-true (command->proc foo))

(check-throw (command-execute 'foo) => 'misc-error)
(check (command-execute foo) => 'non-interactive)
(check (call-interactively foo) => 'interactive)
;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
