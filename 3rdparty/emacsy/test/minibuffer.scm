;;; Layout for tests.
;;;
;;; <file:minibuffer-test.scm>=
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
(use-modules (emacsy minibuffer)
             (emacsy event)
             (emacsy klecl)
             (oop goops)
             (check))

(use-private-modules (emacsy minibuffer))

(set! emacsy-interactive? #t)
(set! aux-buffer minibuffer)

;;; <+ Test Preamble>=
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; <minibuffer:test>=
(check (buffer-string) => "")
(check (point) => 1)
(set! (minibuffer-prompt minibuffer) "What? ")
(check (buffer-string) => "What? ")
(check (point-min) => 7)
(with-buffer minibuffer
             (insert "Nothing."))
(check (buffer-string) => "What? Nothing.")
;;; <minibuffer:test>=
(set! default-klecl-maps (lambda () (list minibuffer-local-map)))
(set-buffer! minibuffer)
(delete-minibuffer-contents minibuffer)
(check (buffer-string) => "What? ")
(insert "A")
(agenda-schedule (colambda ()
                  (minibuffer-message " [Huh?]")))
;;(with-blockable (minibuffer-message " [Huh?]"))
(update-agenda)
(check (buffer-string) => "What? A [Huh?]")
(update-agenda)
(check (buffer-string) => "What? A")
(emacsy-key-event #\c)
(agenda-schedule (colambda () (command-tick)))
(update-agenda)
(check (buffer-string) => "What? Ac")
;(emacsy-key-event #\a)
;(block-tick)
;(check (buffer-string) => "What? Aa")
;;; Test regular input to minibuffer.
;;;
;;;
;;; <minibuffer:test>=
(emacsy-discard-input!)
(emacsy-key-event #\a)
(emacsy-key-event #\cr)
(check (read-from-minibuffer "What? ") => "a")
;;; Test quitting the minibuffer.
;;;
;;;
;;; <minibuffer:test>=
(emacsy-discard-input!)
(emacsy-key-event #\a)
(emacsy-key-event #\g '(control))
;(with-backtrace* (read-from-minibuffer "What?1 "))
;(set! emacsy-interactive? #f)
(check-throw ((colambda () (read-from-minibuffer "What?1 "))) => 'quit-command)
;((colambda () (read-from-minibuffer "What?1 ")))
;; Displaying the "Quit!" message causes a pause.
;(check-throw (update-agenda) => 'quit-command)
;(set! emacsy-interactive? #t)
;;; Test retaining history in the minibuffer.
;;;
;;;
;;; <minibuffer:test>=
(emacsy-discard-input!)
(emacsy-key-event #\h)
(emacsy-key-event #\i)
(emacsy-key-event #\cr)
(emacsy-key-event #\b)
(emacsy-key-event #\y)
(emacsy-key-event #\e)
(emacsy-key-event #\cr)
;(with-backtrace* (read-from-minibuffer "What?1 "))
(let ((h (make-history '())))
  (check (read-from-minibuffer "What?3 " #:history h) => "hi")
  (check (cursor-list->list h) => '("hi"))
  (check (read-from-minibuffer "What?4 " #:history h) => "bye")
  (check (cursor-list->list h) => '("hi" "bye"))
  )
;;; Test accessing history in the minibuffer.
;;;
;;;
;;; <minibuffer:test>=
(emacsy-discard-input!)
(emacsy-key-event #\p '(meta))
(emacsy-key-event #\cr)
(emacsy-key-event #\1)
(emacsy-key-event #\cr)
;(with-backtrace* (read-from-minibuffer "What?1 "))
(let ((h (make-history '("hi"))))
  (check (cursor-list->list h) => '("hi"))
  (check (read-from-minibuffer "What?3 " #:history h) => "hi")
  (check (cursor-list->list h) => '("hi" ""))
  (check (read-from-minibuffer "What?5 " #:history h) => "1")
  (check (cursor-list->list h) => '("hi" "1" ""))
)
;;; Test accessing history in the minibuffer using a symbol.
;;;
;;;
;;; <minibuffer:test>=
(emacsy-discard-input!)
(emacsy-key-event #\1)
(emacsy-key-event #\cr)
(emacsy-key-event #\p '(meta))
(emacsy-key-event #\cr)
(emacsy-key-event #\p '(meta))
(emacsy-key-event #\cr)
;(with-backtrace* (read-from-minibuffer "What?1 "))
(let ((h (make-history '("hi"))))
  (check (read-from-minibuffer "What?6 " #:history 'h1) => "1")
  (check (read-from-minibuffer "What?7 " #:history 'h1) => "1")

  (check (read-from-minibuffer "What?6 " #:history 'h2) => "")
)
;;; @subsection Tab Completion
;;;
;;; We want to offer
;;; \href{http://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Completion.html#Basic-Completion}{string
;;;   completion} similar to Emacs.
;;;
;;;
;;; <minibuffer:test>=
(check (try-completion "f" (list "foo" "foobar" "barfoo")) => "foo")
(check (try-completion "b" (list "foo" "foobar" "barfoo")) => "barfoo")

;; Try against readline completer
(check (try-completion "f" (make-completion-function (list "foo" "foobar" "barfoo"))) => "foo")
(check (try-completion "b" (make-completion-function (list "foo" "foobar" "barfoo"))) => "barfoo")
;;; It can also work with a procedure.
;;;
;;;
;;; <minibuffer:test>=
(check (try-completion "f" (lambda (string predicate all?) (if (string=? string "f") "blah" "huh"))) => "blah")
(check (try-completion "w" (lambda (string predicate all?) (if (string=? string "f") "blah" "huh"))) => "huh")
;;; <minibuffer:test>=
(check (sort (stream->list (readline-completer->stream command-completion-function "")) string<?) => (sort '("eval-expression" "execute-extended-command" "find-file" "kill-buffer" "load-file" "new-buffer" "quit-application" "switch-to-buffer" "universal-argument") string<?))
;;; <minibuffer:test>=
(check (all-completions "f" (list "foo" "foobar" "barfoo")) => (list "foo" "foobar"))
(check (all-completions "b" (list "foo" "foobar" "barfoo")) => (list "barfoo"))

(check (all-completions "f" (make-completion-function (list "foo" "foobar" "barfoo"))) => (list "foo" "foobar"))
(check (all-completions "b" (make-completion-function (list "foo" "foobar" "barfoo"))) => (list "barfoo"))
;;; <minibuffer:test>=
(chdir (format #f "~a/~a" (getenv "ABS_TOP_SRCDIR") "test/minibuffer-test-dir"))
(check (dirname "") => ".")
(check (dirname- "") => ".")
(check (dirname- "../now") => "..")
(check (dirname- "bin") => "bin/")
(check (dirname- "bin/") => "bin/")
(check (dirname- "mini") => ".")
(check (directory? ".") => #t)
(check (directory? "..") => #t)
(check (canonize-file-name ".") => "./")
;(check (files-in-dir ".") => '())
(for-each (lambda (file-name-completer)
            (check (file-name-completer "mini" (const #t) #t) => '("minibuffer-a" "minibuffer-b"))
            (check (file-name-completer "mini" (const #t) #f) => "minibuffer-")
            (check (file-name-completer "bi" (const #t) #f) => "bin/")
            (check (file-name-completer "bin" (const #t) #f) => "bin/")
            (check (file-name-completer "bin/" (const #t) #f) => "bin/")
            ;; Get rid of the dot files.
            (check (file-name-completer "bin" no-dot-files #f) => "bin/run-test")
            (check (file-name-completer "bin/" no-dot-files #f) => "bin/run-test")
            (check (file-name-completer "ex" (const #t) #f) => "exam/")
            (check (file-name-completer "em" (const #t) #f) => "empty-dir/"))
          (list file-name-completer
                ;; (lambda (string predicate all?)
                ;;  (if all?
                ;;      (stream->list (readline-completer->stream file-name-completion-function string))
                ;;      (file-name-completion-function string #f)))
                ))
(check (dirname "bin/") => ".")
(check (basename "bin/") => "bin/")
(check (basename "bin/f") => "f")
(check (dirname "bin/f") => "bin")
(check (dirname- "bin/") => "bin/")
(check (files-in-parent-dir "bin/") => '("bin/../" "bin/./" "bin/run-test"))
(check (files-in-parent-dir "bin") => '( "bin/../" "bin/./" "bin/run-test"))

(check (files-in-dir "bin/") => '("." ".." "run-test"))
(check (files-in-dir "bin") => '( "." ".." "run-test"))
(check (file-name-completer "bin/" no-dot-files #t) => '("bin/run-test"))

;(check (files-in-dir "..") => '())

(chdir (format #f "~a/~a" (getenv "ABS_TOP_SRCDIR") "test/minibuffer-test-dir/empty-dir"))
(check (file-name-completer "../ex" (const #t) #f) => "../exam/")
;;; <minibuffer:test>=
(let ((h (make-history '("3" "2" "1"))))
  (cursor-left! h)
    (check ( history-ref h) => "1")
    (history-set! h "a")
    (check ( history-ref h) => "a")
    (cursor-left! h)
    (check ( history-ref h) => "2")
    (cursor-right! h)
    (cursor-right! h)
    (check (cursor-list->list h) => '("3" "2" "a")))
;;; <+ Test Postscript>=
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
;;(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
