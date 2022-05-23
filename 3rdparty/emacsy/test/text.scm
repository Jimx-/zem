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
             (emacsy text)
             (emacsy command)
             (emacsy event)
             (emacsy keymap)
             (oop goops)
             (rnrs base))

(use-private-modules (emacsy buffer))
(use-private-modules (emacsy text))

;;; <+ Test Preamble>=
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())

;;; Let's test this regex search in a gap buffer.
;;;
;;; <buffer:test>=
(define c (make <text-buffer> #:name "*test-regex*"))
(add-buffer! c)
(check (current-buffer) => c)
(check (buffer-modified?) => #f)
(check (buffer-modified-tick) => 0)
(insert "hellos these ard words!")
(check (buffer-modified?) => #t)
(check (buffer-modified-tick) => 1)
;;       1    7     13  17
(check (point) => (point-max))
(check (point-min) => 1)
(goto-char (point-min))
(check (gb-char-after (gap-buffer c) 1) => #\h)
(check (gb-char-before (gap-buffer c) 1) => #f)
(check (point) => 1)
(check (forward-word) => 7)
(check (point) => 7)
(check (char-before) => #\s)
(check (char-after) => #\space)

(check (forward-word 2) => 17)
(check (char-before) => #\d)
(check (char-after) => #\space)
(check (backward-word) => 14)
(check (char-before) => #\space)
(check (char-after) => #\a)

;;#(!sdrow dra eseht solleh (17 . 24))
;;  1      8   12    18
;; is               ^      ^
;; goto      ^
;; was     ^
;;; <+ Test Postscript>=
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
