(define-module (zem ui minibuffer-view)
  #:use-module ((zem api font) #:prefix f:)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module (zem ui view)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (<minibuffer-view>))

(define-class <minibuffer-view> (<view>)
  (message #:init-form "" #:accessor minibuffer-message))

(define (get-minibuffer-height)
  (floor (* (f:get-font-height style:font) 1.2)))

(define-method (view:draw (view <minibuffer-view>))
  (match-let (((x . y) (view:pos view))
              ((w . h) (view:size view)))
             (draw-view-background view style:background-color)
             (r:add-text style:font
                         (cons x (+ y (f:get-font-height style:font)))
                         (minibuffer-message view)
                         style:text-color
                         w)))

(define-method (view:get-size-request (view <minibuffer-view>))
  (set! (minibuffer-message view) (buffer:buffer-string minibuffer))
  (r:text-size-hint style:font
                    (minibuffer-message view)
                    (car (view:size view))))
