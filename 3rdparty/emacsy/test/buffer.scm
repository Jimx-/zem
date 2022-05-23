;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
             (emacsy mru-stack)
             (emacsy buffer)
             (emacsy command)
             (emacsy event)
             (emacsy keymap)
             (oop goops)
             (rnrs base))

(use-private-modules (emacsy buffer))

;;; <+ Test Preamble>=
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <buffer:test>=
(define b (make <buffer> #:name "*test-buffer*"))
(check (buffer-name b) => "*test-buffer*")
(check (object->string b) => "\"#<buffer '*test-buffer*'>\"")
(check (current-buffer) => #f)
;;; <buffer:test>=
(add-buffer! b)
(check (buffer-name) => "*test-buffer*")
(remove-buffer! b)
(check (current-buffer) => #f)

(add-buffer! b)
(warn 'buffer-list (buffer-list))
(define a (make <buffer> #:name "*a*"))
(add-buffer! a)
(check (current-buffer) => a)
(switch-to-buffer b)
(check (current-buffer) => b)
(switch-to-buffer a)
(check (current-buffer) => a)

;;; <+ Test Postscript>=
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
