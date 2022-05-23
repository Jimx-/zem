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

;; We generate the file @file{example/hello-emacsy.c.x} by running the
;; command: @code{guile-snarf example/hello-emacsy.c}.  Emacsy can now
;; access and alter the application's internal state.
;;.

;; @subsection Changing the UI Now let's use these new procedures to
;; create interactive commands and bind them to keys by changing our
;; config file @file{example/hello-emacsy.scm}.
(use-modules (emacsy emacsy))

;;.
(define-interactive (incr-counter #:optional (n (universal-argument-pop!)))
 "Increment the counter."
 (set-counter! (+ (get-counter) n)))

;;.
(define-interactive (decr-counter #:optional (n (universal-argument-pop!)))
 "Decrement the counter."
 (set-counter! (- (get-counter) n)))

;; Bind @var{inc-counter} to @code{=}.
(define-key global-map "=" 'incr-counter)
;; Bind @var{inc-counter} to @code{-}.
(define-key global-map "-" 'decr-counter)

;; We can now hit @verb{|-|} and @verb{|=|} to decrement and increment the
;; @var{counter}. This is fine, but what else can we do with it?  We could
;; make a macro that increments 5 times by hitting
;; @verb{|C-x ( = = = = = C-x )|}, then hit @verb{|C-e|} to run that macro.
;; (set! debug-on-error? #t)

;; Let's implement another command that will ask the user for a number to
;; set the counter to.
;;.

;; Now we can hit @verb{|M-x change-counter|} and we'll be prompted for
;; the new value we want.  There we have it.  We have made the simplest
;; application ever more @emph{Emacs-y}.
(define-interactive (change-counter)
 "Change the counter to a new value."
 (set-counter!
   (string->number
     (read-from-minibuffer
       "New counter value: "))))

;; @subsection Changing it at Runtime
;;
;; We can add commands easily by changing and reloading the file.  But
;; we can do better.  Let's start a REPL we can connect to.
;; @file{example/hello-emacsy.scm}.
;;.

;; @example
;; (use-modules (system repl server))
;; (spawn-server)
;; @end example
;; Start a server on port 37146.
;;.

;; Start a server on port 37146.
(use-modules (system repl server))
(spawn-server)
