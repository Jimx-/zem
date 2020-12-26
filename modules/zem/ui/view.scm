(define-module (zem ui view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module (oop goops)
  #:export (<view>
            view:pos
            view:size
            view:draw
            update-view-layout
            view:get-size-request
            draw-view-background))

(define-class <view> ()
  (pos #:init-keyword #:pos #:init-form '(0 . 0) #:accessor view:pos)
  (size #:init-keyword #:size #:init-form '(0 . 0) #:accessor view:size))

(define (update-view-layout view pos size)
  (set! (view:pos view) pos)
  (set! (view:size view) size))

(define-method (view:update (view <view>) delta))

(define-method (view:draw (view <view>)))

(define-method (view:get-size-request (view <view>))
  (view:size view))

(define (draw-view-background view color)
  (r:add-rect (view:pos view) (view:size view) color))
