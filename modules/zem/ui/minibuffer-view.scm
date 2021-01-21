(define-module (zem ui minibuffer-view)
  #:use-module ((zem api font) #:prefix f:)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module (zem core faces)
  #:use-module (zem ui view)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (<minibuffer-view>))

(define-class <minibuffer-view> (<view>)
  (prompt #:init-form "" #:accessor minibuffer-view:prompt)
  (message #:init-form "" #:accessor minibuffer-view:message))

(define (get-text-y-offset)
  (floor (/ (f:get-font-height (face-attribute 'default ':font)) 4.0)))

(define-method (view:draw (view <minibuffer-view>))
  (match-let* (((x . y) (view:pos view))
               ((w . h) (view:size view))
               (font (face-attribute 'default ':font))
               (foreground (face-attribute 'default ':foreground))
               (prompt (minibuffer-view:prompt view))
               (prompt-foreground (face-attribute 'minibuffer-prompt ':foreground)))
              (draw-view-background view style:background-color)
              (r:add-text font
                          (cons (+ x (car style:padding)) (+ y (f:get-font-height font)))
                          (string-append
                           (make-string (string-length prompt) #\ )
                           (minibuffer-view:message view))
                          foreground
                          (- w (car style:padding)))
              (r:add-text font
                          (cons (+ x (car style:padding)) (+ y (f:get-font-height font)))
                          prompt
                          prompt-foreground
                          (- w (car style:padding)))))

(define-method (view:get-size-request (view <minibuffer-view>))
  (set! (minibuffer-view:prompt view) (minibuffer-prompt minibuffer))
  (set! (minibuffer-view:message view) (string-append
                                        (minibuffer-contents minibuffer)
                                        (minibuffer-message-string minibuffer)))
  (match-let (((w . h) (r:text-size-hint (face-attribute 'default ':font)
                                         (string-append
                                          (minibuffer-view:prompt view)
                                          (minibuffer-view:message view))
                                         (- (car (view:size view))
                                            (* 2 (car style:padding))))))
             (cons w (+ h (get-text-y-offset)))))
