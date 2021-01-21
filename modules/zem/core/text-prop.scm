(define-module (zem core text-prop)
  #:use-module (zem util avl)
  #:use-module (zem util plist)
  #:use-module (zem core faces)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9 gnu)
  #:export (set-text-properties
            add-text-properties
            put-text-property
            text-property-list))

(define-class <interval> ()
  (start #:init-keyword #:start #:accessor interval:start)
  (end #:init-keyword #:end #:accessor interval:end))

(define-method (<? (a <interval>) (b <interval>))
  (< (interval:start a) (interval:start b)))
(define-method (>? (a <interval>) (b <interval>))
  (> (interval:start a) (interval:start b)))
(define-method (equal? (a <interval>) (b <interval>))
  (= (interval:start a) (interval:start b)))

(define (make-interval start end)
  (make <interval> #:start start #:end end))

(define (purge-range/iter start end iter tree acc)
  (let ((iter-at (avl-iter-at iter)))
    (if (not iter-at)
        (cons tree acc)
        (match-let
         (((interval . props) iter-at))
         (if (>= (interval:start interval) end)
             (cons tree acc)
             ;; Remove the interval and then add back the non-overlapping sub-intervals
             (let* ((erase-cur (car (avl-erase tree interval)))
                    (add-left (if (< (interval:start interval) start)
                                  (avl-insert erase-cur
                                              (make-interval (interval:start interval) start)
                                              props)
                                  erase-cur))
                    (add-right (if (> (interval:end interval) end)
                                   (avl-insert add-left
                                               (make-interval end (interval:end interval))
                                               props)
                                   add-left))
                    (purged-interval (cons
                                      (make-interval (max (interval:start interval) start)
                                                     (min (interval:end interval) end))
                                      props)))
               (purge-range/iter start
                                 end
                                 (avl-iter-incr iter)
                                 add-right
                                 (cons purged-interval acc))))))))

(define (purge-range tree start end)
  (let* ((left (avl-start-iter-less-equal tree (make-interval start end)))
         (left-at (avl-iter-at left))
         (first (cond
                 ;; No intervals to the left
                 ((not left-at)
                  (avl-start-iter-greater tree (make-interval start end)))
                 ;; Left interval does not intersect with start..end
                 ((<= (interval:end (car left-at)) start)
                  (avl-iter-incr left))
                 (else left))))
    (purge-range/iter start end first tree '())))

(define* (set-text-properties start end props #:optional (object (current-buffer)))
  (match-let* (((tree . _) (purge-range (buffer-intervals object) start end))
               (new-tree (avl-insert tree (make-interval start end) props)))
              (set! (buffer-intervals object) new-tree)))

(define (prop-subset? a b)
  (match a
         ((prop val rest ...)
          (if (not (equal? val (plist-get b prop)))
              #f
              (prop-subset? rest b)))
         (_ #t)))

(define (reinsert-intervals tree intervals add-props)
  (if (null? intervals)
      tree
      (match-let* (((interval . props) (car intervals))
                   (new-props (apply plist-new props add-props)))
                  (reinsert-intervals
                   (if (prop-subset? new-props add-props)
                       tree
                       (avl-insert (car (purge-range tree
                                                     (interval:start interval)
                                                     (interval:end interval)))
                                   interval
                                   new-props))
                   (cdr intervals) add-props))))

(define* (add-text-properties start end props #:optional (object (current-buffer)))
  (match-let* (((tree . acc) (purge-range (buffer-intervals object) start end))
               (new-tree (avl-insert tree (make-interval start end) props)))
              (set! (buffer-intervals object)
                    (reinsert-intervals new-tree acc props))))

(define* (put-text-property start end prop value #:optional (object (current-buffer)))
  (add-text-properties start end (list prop value) object))

(define (scan-intervals/iter start end iter acc)
  (let ((iter-at (avl-iter-at iter)))
    (if (not iter-at)
        acc
        (match-let
         (((interval . props) iter-at))
         (if (>= (interval:start interval) end)
             acc
             (scan-intervals/iter start
                                  end
                                  (avl-iter-incr iter)
                                  (cons (cons (cons (max (interval:start interval) start)
                                                    (min (interval:end interval) end))
                                              props)
                                        acc)))))))

(define (scan-intervals tree start end)
  (let* ((left (avl-start-iter-less-equal tree (make-interval start end)))
         (left-at (avl-iter-at left))
         (first (cond
                 ;; No intervals to the left
                 ((not left-at)
                  (avl-start-iter-greater tree (make-interval start end)))
                 ;; Left interval does not intersect with start..end
                 ((<= (interval:end (car left-at)) start)
                  (avl-iter-incr left))
                 (else left))))
    (scan-intervals/iter start end first '())))

(define (text-property-list object start end)
  (reverse (scan-intervals (buffer-intervals object) start end)))
