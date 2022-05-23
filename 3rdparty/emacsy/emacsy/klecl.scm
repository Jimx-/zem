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

(define-module (emacsy klecl)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs io ports)
  #:use-module (oop goops)
  #:use-module (system repl error-handling)
  #:use-module (emacsy util)
  #:use-module (emacsy keymap)
  #:use-module (emacsy event)
  #:use-module (emacsy command)
  #:use-module (emacsy block)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:use-module (emacsy text)
  #:autoload (emacsy minibuffer) (clear-minibuffer)
  #:export (command-loop)
  #:declarative? #f)

;;; Commentary:

;; @node KLECL
;; @section KLECL
;;
;; @quotation
;; A box without hinges, key, or lid, yet golden treasure inside is hid.
;; @author The Hobbit -- J. R. R. Tolkien
;; @end quotation

;; We finally have all the pieces to properly build the KLECL.  First, we
;; have to accept input.

;;; Code:

;;.
(define-public event-queue (make-q))

;;.
(define-public read-event-hook (make-hook 1))

;;.
(define-public emacsy-interactive? #f)

;; With the command loop I've also adopted a prefix of [[primitive-]]
;; which signifies that it does not do any error handling.  The command
;; loop sets up a fair amount of state.
;;.

;;.
(define-public this-command-event #f)

;;.
(define-public last-command-event #f)

;;.
(define-public pre-command-hook (make-hook))

;;.
(define-public post-command-hook (make-hook))

;;.
(define-public emacsy-ran-undefined-command? #f)

;; Each command loop is given a different number.
(define command-loop-count 0)

;;.
(define-public (emacsy-event event)
  (enq! event-queue event))

;; This is a convenience procedure to enqueue a key event.
(define*-public (emacsy-key-event char #:optional (modifier-keys '()))
  (emacsy-event (make <key-event>
                  #:modifier-keys modifier-keys
                  #:command-char char)))

;;.
(define*-public (emacsy-mouse-event position button state #:optional (modifier-keys '()))
  (emacsy-event
   (make <mouse-event>
     #:position position #:button button
     #:state state #:modifier-keys modifier-keys)))

;; And mainly for testing purposes we also want to discard all input.  Or
;; there are cases where we want to unread an event and push it to the
;; front of the queue rather than the rear.
(define-public (emacsy-discard-input!)
  (while (not (q-empty? event-queue))
    (deq! event-queue)))

;;.
(define-public (emacsy-event-unread event)
  (q-push! event-queue event))

;; @subsection read-event
;;
;; [[read-event]] is the lowest-level procedure for grabbing events.  It
;; will block if there are no events to read.
;;.

;;; (define*-public (read-event #:optional (prompt #f))
;;;   (if prompt
;;;       (message prompt))
;;;   ;;;;(if (q-empty? event-queue)
;;;   (block-while (lambda () (q-empty? event-queue)) #t)
;;;   (let ((event (deq! event-queue)))
;;;     (run-hook read-event-hook event)
;;;     event))

(define (raw-read-event prompt)
  (if prompt
      (message prompt))
  (let ((event (deq! event-queue)))
    (run-hook read-event-hook event)
    (emacsy-log-debug "RAW-READ-EVENT ~a~%" event)
    event))

;;.
(define*-public (read-event #:optional (prompt #f))
  (if emacsy-interactive?
      (if (q-empty? event-queue)
          (yield (lambda (resume)
                   (block-read-event prompt resume)))
          (raw-read-event prompt))
      ;; We're non-interactive. I need to read from stdin.

      (let ((input-string (read-line)))
        (if (and (eof-object? input-string)
                 (q-empty? event-queue))
            (throw 'read-event-eof)
            (begin
              (unless (eof-object? input-string)
                (map emacsy-event (kbd->events input-string)))
              (raw-read-event prompt))))))

(define reader-request-queue (make-q))

(define (block-read-event prompt resume)
  (emacsy-log-info "block-read-event ~a~%" (list prompt resume))
  (enq! reader-request-queue (list prompt resume)))

(codefine (fulfill-read-requests)
   (while #t
     (when (getenv "EMACSY_DEBUG") (format #t "fulfill-read-requests CHECK~%"))
     (when (and (not (q-empty? event-queue))
                (not (q-empty? reader-request-queue)))
       (emacsy-log-info "fulfill-read-requests DO~%")
       (match (deq! reader-request-queue)
         ((prompt resume)
          ;; Do I need to schedule this with the agenda to make it
          ;; behave properly?
          (resume (raw-read-event prompt)))))
     (wait)))

(agenda-schedule fulfill-read-requests)

;; @subsection read-key
;;
;; Read key is slightly more high level than [[read-event]].  It may do a
;; little processing of coalesing of events.  For instance, down and up
;; mouse events may be changed into click or drag events.

;;; @c\todo{There's probably a better way of handling disparate classes of events---use polymorphism!}

(define last-down-mouse-event #f)

;;.
(define*-public (read-key #:optional (prompt #f))
  (define* (new-mouse-state new-state event #:optional (event2 #f))
    (let ((e (make (if event2 <drag-mouse-event> <mouse-event>)
             #:modifier-keys (modifier-keys event)
             #:position (position event)
             #:button (button event)
             #:state new-state)))
      (if event2
          (slot-set! e 'rect (list (position event) (position event2))))
      e))
  ;; XXX Can this be refashioned into a nice handle-event method?
  ;; That would split up all these mouse key concerns.
  (let loop ((event (read-event prompt)))
    (if (is-a? event <dummy-event>)
        ;; Ignore it.
        (loop (read-event prompt))
        (if (down-mouse-event? event)
            (begin
              (set! last-down-mouse-event event)
              (loop (read-event prompt)))
            (if (and last-down-mouse-event
                     (down-mouse-event? last-down-mouse-event)
                     (up-mouse-event? event))
                (let ((new-event
                       (if (vector= (position last-down-mouse-event) (position event))
                           ;; Is it a click?
                           (new-mouse-state 'click event)
                           ;; Or a drag?
                           (new-mouse-state 'drag last-down-mouse-event event ))))
                  (set! last-down-mouse-event #f)
                  new-event)
                event)))))

;; @subsection read-key-sequence
;;
;; [[read-key-sequence]] is at a higher level than [[read-key]].  It
;; considers the currently active keymaps.  If a sequence is defined in
;; the keymap, it returns that event sequence.  If it doesn't match a
;; sequence yet, it continues to read more keys.  If it cannot match any
;; sequence, it returns that event sequence.

;;; @c\todo {Consider using values to return multiple values.}

;; read-key-sequence #:optional prompt #:key keymaps
;;.

(define*-public (read-key-sequence
                 #:optional
                 (prompt #f)
                 #:key
                 (keymaps (default-klecl-maps)))
  (define (read-discrete-key)
    (let mini-loop ((event (read-key prompt)))
      (emacsy-log-trace "EVENT ~a~%" event)
      (if (discrete-event? event)
          event
          (mini-loop (read-key prompt)))))
  (let loop ((events (list (read-key prompt))))
    (let* ((keys (reverse (map (compose event->kbd canonize-event!) events)))
           (last-key (rcar keys)))
     ;; Do we have enough keys?
     (if (or
          ;; Does one of the keymaps points to a command (or is it the
          ;; quit key)?
          (or (quit-key? last-key keymaps)
              (any (lambda (keymap)
                     (lookup-key? keymap keys))
                   keymaps))
          ;; OR does none of the keymaps point to a command or keymap (or
          ;; is the quit key)?
          (not (or (quit-key? last-key keymaps)
                   (any (lambda (keymap)
                          (lookup-key? keymap keys #t))
                        keymaps))))
         ;; Yes. We have enough keys.
         (reverse events)
         ;; No.  Let's get some more.
         (loop (cons (read-discrete-key) events))))))

;; We also check all the maps for a quit key, typically defined as @verb{|C-g|}.
(define-public (quit-key? aKey keymaps)
  (define (quit-key?* key keymap)
   (let ((result (lookup-key keymap (list key))))
     (and (not (keymap? result)) (lookup-key-entry? result)
          (eq? 'keyboard-quit result))))
  (any (lambda (keymap) (quit-key?* aKey keymap)) keymaps))

;;; \todo{Rename default-klecl-maps to current-active-maps.}

;;.
(define-public (default-klecl-maps)
  (list))

;; I find it convenient to begin emitting messages in case of error.
;; However, I would like for there to be a clean separation between
;; Emacsy and its KLECL such that someone may write a clean vim-y using
;; it if they so chose.  So this message will merely go to the stdout\;
;; however, it will be redefined later.
(define-public (message . args)
  (apply format #t args))

(define call-with-sigalrm
  (if (not (provided? 'posix))
      (lambda (thunk) (thunk))
      (lambda (thunk)
        (let ((handler #f))
          (dynamic-wind
            (lambda ()
              (set! handler
                    (sigaction SIGALRM
                      (lambda (sig)
                        ;;(block-yield)
                        (scm-error 'signal #f "Alarm interrupt" '()
                                   (list sig))))))
            thunk
            (lambda ()
              (if handler
                  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                  (sigaction SIGALRM (car handler) (cdr handler))
                  ;; restore original C handler.
                  (sigaction SIGALRM #f))))))))

;; primitive-command-tick #:optional prompt #:key keymaps undefined-command
;; XXX Rename this to klec, for Key-Lookup-Execute-Command (KLEC)---just
;; missing the loop component?
;;.

(define*-public (primitive-command-tick #:optional
                                        (prompt #f)
                                        #:key
                                        (keymaps (default-klecl-maps))
                                        (undefined-command undefined-command))
  "We do one iteration of the command-loop without any error handling."
  (call-with-sigalrm
   (lambda ()
     ((@@ (ice-9 top-repl) call-with-sigint)
      (lambda ()
        (let* ((events (read-key-sequence prompt #:keymaps keymaps))
               (key-sequence (map event->kbd events))
               (keymap (find (lambda (k) (lookup-key? k key-sequence)) keymaps)))
          (set! emacsy-ran-undefined-command? #f)
          (if keymap
              (begin
                (set! last-command-event this-command-event)
                (set! this-command-event (rcar events))
                ;; The command hooks might need to go into the command module.
                (in-out
                 (run-hook pre-command-hook)
                 (call-interactively (lookup-key keymap key-sequence))
                 (run-hook post-command-hook)))
              ;; Maybe this should be done by an undefined-command command?
              ;; I doubt we want this command to be executed by the user, so
              ;; we're going to leave it as a procedure.
              (undefined-command key-sequence events))))))))

(define* (undefined-command key-sequence events)
  (message "~a is undefined."
           (string-join key-sequence " "))
  (set! emacsy-ran-undefined-command? #t)
  (values #f 'no-such-command))

;;.
(define*-public (command-tick #:key (keymaps (default-klecl-maps)))
  "We do one iteration of command-tick and handle errors."

  (catch #t
    (lambda ()
      (if debug-on-error?
          (call-with-error-handling
           (lambda ()
             (primitive-command-tick #:keymaps keymaps))
           #:pass-keys
           ;; XXX what the hell is the story with all these quits?
           '(silent-quit quit-command-loop quit-command keyboard-quit))

          (with-backtrace* (lambda ()
                             (primitive-command-tick #:keymaps keymaps))
                           '(silent-quit quit-command-loop))))
    (lambda (key . args)
      (case key
        ((silent-quit)
         (emacsy-log-warning "GOT SILENT QUIT in command-tick\n"))
        ((quit-command-loop)
         (emacsy-log-warning "GOT QUIT-COMMAND-LOOP in command-tick\n")
         (apply throw key args))
        ((encoding-error)
         (emacsy-log-warning "ENCODING-ERROR '~a'" (pp-string event-queue)))
        (else
         (emacsy-log-error
                 "command-tick: Uncaught throw to '~a: ~a\n" key args))))))

;; Now let's write the command loop without any error handling.  This
;; seems a little messy with the continue predicate procedure being
;; passed along.  I'm not sure yet, how best to organize it.
(define*-public (primitive-command-loop #:optional (continue-pred (const #t)))
  "We iterate with command-tick but we do not handle any errors."
  (with-fluids ((continue-command-loop? #t))
    (let loop ((continue? (call-with-values
                              primitive-command-tick
                            continue-pred)))
      (if (and (fluid-ref continue-command-loop?) continue?)
          (loop (call-with-values
                    primitive-command-tick
                  continue-pred))
          (decr! command-loop-count)))))

;;; Finally, here's our command loop with error handling.
(define* (command-loop #:optional (continue-pred (const #t)))
  "We iterate with command-tick and handle errors."
  (catch #t
    (lambda ()
      (if debug-on-error?
          (call-with-error-handling
           (lambda ()
             (primitive-command-loop continue-pred))
           #:pass-keys
           '(silent-quit quit-command-loop quit-command keyboard-quit))

          (with-backtrace* (lambda ()
                             (primitive-command-loop continue-pred))
                           '(silent-quit quit-command-loop))))
    (lambda (key . args)
      (case key
        ((silent-quit)
         (emacsy-log-warning "GOT SILENT QUIT in command-loop"))
        ((quit-command-loop)
         (emacsy-log-warning "GOT QUIT-COMMAND-LOOP in command-loop")
         (apply throw key args))
        ((encoding-error)
         (emacsy-log-error "ENCODING-ERROR '~a'" (pp-string event-queue)))
        (else
         (emacsy-log-error
                 "command-loop: Uncaught throw to '~a: ~a\n" key args))))))

;;.
(define-interactive (keyboard-quit)
  (clear-minibuffer)
  (set-mark #f)
  (message "Quit!")
  (throw 'quit-command))

;; @subsection emacsy-event
;;
;; The embedding application will handle the actual IO, but it passes
;; events to Emacsy for processing which are stored in a queue.

;; We have finished the KLECL.  Note that although we have used
;; Emacs-like function names, we have not implemented the Emacs-like UI
;; yet. We have not defined any default key bindings.  I want to
;; encourage people to explore different user interfaces based on the
;; KLECL, and one can start from this part of the code.  If one wanted to
;; create a modal UI, one could use the [[(emacsy klecl)]] module and not
;; have to worry about any ``pollution'' of Emacs-isms.
;;.
