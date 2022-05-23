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
(define-module (emacsy line-pragma)
  #:use-module (ice-9 rdelim))

(eval-when (compile load eval)
 (define line-pragma-handler ;;; BUG: The line pragma ends up littering the source with zero length      
                             ;;; strings, which often doesn't matter, but it can't be used everywhere    
                             ;;; especially within a particular form.  I'm not entirely sure how to fix  
                             ;;; that.                                                                   
                             ;;;                                                                         
                             ;;;                                                                         
                             ;;; <Line Pragma Handler>=                                                  
                             (lambda (char port)
                               (let ((ine (read port))
                                     (lineno (read port))
                                     (filename (read port)))
                                 (if (not (eq? ine 'ine))
                                     (error (format #f "Expected '#line <line-number> <filename>'; got '#~a~a ~a \"~a\"'." char ine lineno filename)))
                                 (set-port-filename! port filename)
                                 (set-port-line! port lineno)
                                 (set-port-column! port 0)
                                 ;; Return unspecified on purpose.
                                 *unspecified*
                                 )))
 (read-hash-extend #\l #f)
 (read-hash-extend #\l line-pragma-handler)
 #;(read-hash-extend #\" ;;; The above code will see a string "\#line 352 " followed by a bare       
                         ;;; symbol emacsy.w, which will not do.  To get around this, I implemented  
                         ;;; another reader extension that will strip out any \#l lines within it.   
                         ;;;                                                                         
                         ;;;                                                                         
                         ;;; <Liberal String Quote Reader>=                                          
                         (lambda (char port)
                           (let ((accum '()))
                             (let loop ((entry (read-char port)))
                               (if (or (eof-object? entry)
                                       (and (char=? #\" entry)
                                            (char=? #\# (peek-char port))
                                            (begin (read-char port)
                                                   #t)))
                                   ;; We're done
                                   (apply string (reverse accum))
                                   (begin
                                     (if (and (char=? #\# entry)
                                              (char=? #\l (peek-char port)))
                                         ;; Drop this line
                                         (begin (read-line port)
                                                (loop (read-char port)))
                                         (begin
                                           ;; Keep and loop
                                           (set! accum (cons entry accum))
                                           (loop (read-char port)))))))))))
