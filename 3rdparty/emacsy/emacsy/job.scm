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

(define-module (emacsy job)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:export (<job>
            job?
            make-job
            get-job-id
            suspend-job
            continue-job))

(define-record-type <job>
  (%make-job job-id job-state job-exit-value job-cont)
  job?
  (job-id job-id)
  (job-state job-state set-job-state!)
  (job-exit-value job-exit-value set-job-exit-value!)
  (job-cont job-cont set-job-cont!))

(set-record-type-printer! <job>
  (lambda (job port)
    (format port "#<job id: ~a state: ~a"
            (job-id job)
            (job-state job))
    (when (job-exit-value job)
      (format port " exit-value: ~a"
              (job-exit-value job)))
    (when (job-cont job)
      (format port " cont: ~a"
              (job-cont job)))
    (display #\> port)))



(define *current-job-list* '())

(define next-job-id 1)

(define (job-id->job jid)
  (find (lambda (job)
          (eq? (job-id job) jid))
        *current-job-list*))

(define (coerce-to-job job-or-jid)
  (if (job? job-or-jid)
      job-or-jid
      (job-id->job job-or-jid)))

(define* (make-job thunk)
  "Creates a coroutine that has some job control."
  (let ((job (%make-job next-job-id 'baby #f #f)))
   (define (handler cont key . args)
     (define (resume . args)
       ;; (format #t "resuming job ~a~%" (job-id job))
       ;; Call continuation that resumes the procedure.
       (call-with-prompt 'coroutine-prompt
                         (lambda () (apply cont args))
                         handler))
     (define (job-resume . args)
       (if (eq? (job-state job) 'running)
           (apply resume args)
           (begin
             (set-job-cont! job job-resume)
             ;; (format #t "job ~a unable to resume because it is ~a~%"
             ;;         (job-id job)
             ;; (job-state job))
             )))
     (case key
       ((callback)
        (when (procedure? (car args))
          (apply (car args) job-resume (cdr args))))
       ((user-data)
        (resume job))))
   (set! next-job-id (1+ next-job-id))
   (set! *current-job-list* (cons job *current-job-list*))
   (values
    (lambda ()
      (if (eq? (job-state job) 'baby)
          (begin
            (set-job-state! job 'running)
            ;;(format #t "starting job ~a~%" (job-id job))
            (call-with-prompt 'coroutine-prompt
                              (lambda () (job-exit (thunk)))
                              handler))
          (throw 'job-already-started)))
    job)))

(define (suspend-job job-or-jid)
  "Suspend a job that is currently running."
  (set-job-state! (coerce-to-job job-or-jid) 'suspended))

(define (continue-job job-or-jid)
  "Continue a suspended job and schedule it to be run."
  (let ((job (coerce-to-job job-or-jid)))
   (when (eq? (job-state job) 'suspended)
     (set-job-state! job 'running)
     (agenda-schedule (job-cont job))
     (set-job-cont! job #f))))

(define (wait-for-job job-or-jid)
  "Waits for a job to complete."
  ;; XXX This should be smarter than polling.
  (let ((job (coerce-to-job job-or-jid)))
    (while (not (eq? (job-state job) 'zombie))
      (wait))
    (job-exit-value job)))

(define (job-exit return-value)
  "Exit the job with the given return-value."
  (let ((job (couser-data)))
    (set-job-state! job 'zombie)
    (set-job-exit-value! job return-value))
  (yield (lambda (resume)
           return-value)))

(define (get-job-id)
  "Returns the job-id within the job."
  (job-id (couser-data)))
