(define-module (zem ui view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (<view>
            view:pos
            view:size
            view:active?
            view:scroll
            view:scroll-target
            view:draw
            view:update
            view:draw
            view:get-size-request
            view:get-visible-bbox
            view:get-scroll-limit
            view:mouse-position-callback
            view:mouse-press-callback
            view:mouse-release-callback
            view:mouse-scroll-callback))

(define-class <view> ()
  (pos #:init-keyword #:pos #:init-form '(0 . 0) #:accessor view:pos)
  (size #:init-keyword #:size #:init-form '(0 . 0) #:accessor view:size)
  (active? #:init-value #f #:accessor view:active?)
  (scroll #:init-form '(0 . 0) #:accessor view:scroll)
  (scroll-target #:init-form '(0 . 0) #:accessor view:scroll-target))

(define-public (update-view-layout view pos size)
  (set! (view:pos view) pos)
  (set! (view:size view) size))

(define (clamp-scroll-offset view scroll)
  (cons (car scroll) (min (max 0 (cdr scroll)) (view:get-scroll-limit view))))

(define-method (view:update (view <view>) delta)
  (when (not (equal? (view:scroll-target view) (view:scroll view)))
    (set! (view:scroll-target view)
          (clamp-scroll-offset view (view:scroll-target view)))
    (set! (view:scroll view) (view:scroll-target view))))

(define-method (view:draw (view <view>)))

(define-method (view:get-size-request (view <view>))
  (view:size view))

(define-method (view:get-visible-bbox (view <view>))
  (match-let (((sx . sy) (view:scroll view))
             ((w . h) (view:size view)))
             (list sx sy (+ sx w) (+ sy h))))

(define-method (view:get-scroll-limit (view <view>))
  +inf.0)

(define-method (view:mouse-position-callback (view <view>) x y)
  #f)

(define-public mouse-scroll-factor 50)

(define-method (view:mouse-press-callback (view <view>) button x y)
  #f)

(define-method (view:mouse-release-callback (view <view>) button)
  #f)

(define-method (view:mouse-scroll-callback (view <view>) y-offset)
  (match-let (((sx . sy) (view:scroll-target view)))
             (set! (view:scroll-target view) (cons sx (- sy (* y-offset mouse-scroll-factor))))))

(define-public (draw-view-background view color)
  (r:add-rect (view:pos view) (view:size view) color))
