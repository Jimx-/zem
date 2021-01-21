(define-module (zem ui root-view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module ((zem api system) #:prefix sys:)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module (zem ui view)
  #:use-module (zem ui minibuffer-view)
  #:use-module (zem ui buffer-view)
  #:use-module (zem core buffer)
  #:use-module (zem core faces)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:export (make-root-view))

(define-immutable-record-type <view-node>
  (make-view-node type pos size view split locked? left right)
  view-node?
  (type node-type)
  (pos node-pos)
  (size node-size)
  (view node-view set-node-view)
  (split node-split)
  (locked? node-locked?)
  (left node-left)
  (right node-right))

(define-class <empty-view> (<view>))

(define-method (view:draw (view <empty-view>))
  (draw-view-background view style:background-color))

(define-class <root-view> (<view>)
  (root-node #:init-form (make-leaf-node
                          '(0 . 0)
                          '(0 . 0)
                          (make <empty-view>)
                          #f)
             #:init-keyword #:root-node
             #:accessor root-node)
  (buffer-view #:init-keyword #:buffer-view #:accessor buffer-view)
  (mouse-pos #:init-form '(0 . 0) #:accessor mouse-pos)
  (active-view #:init-form (make <empty-view>) #:accessor active-view))

(define (make-leaf-node pos size view locked?)
  (make-view-node 'leaf pos size view 0 locked? '() '()))

(define* (split-node node dir #:key (view (make <empty-view>)) (split 0.5) (locked? #f))
  (let*-values (((new-child) (make-leaf-node '(0 . 0) '(0 . 0) view locked?))
                ((type left right) (case dir
                                     ((up) (values 'vsplit new-child node))
                                     ((down) (values 'vsplit node new-child))
                                     ((left) (values 'hsplit new-child node))
                                     ((right) (values 'hsplit node new-child))))
                ((new-node) (make-view-node type
                                          '(0 . 0)
                                          '(0 . 0)
                                          view
                                          split
                                          #f
                                          left
                                          right)))
    (update-node-layout new-node (node-pos node) (node-size node))))

(define (get-node-size-request node)
  (case (node-type node)
    ((leaf) (if (node-locked? node)
                (view:get-size-request (node-view node))
                '(0 . 0)))
    ((hsplit vsplit)
     (match-let (((left-w . left-h) (get-node-size-request (node-left node)))
                 ((right-w . right-h) (get-node-size-request (node-right node))))
                (if (eq? (node-type node) 'hsplit)
                    '((+ left-w right-w) left-h)
                    '(left-w (+ left-h right-h)))))
    (else '(0 . 0))))

(define (calc-vsplit-point split h sr1 sr2 ds)
  (cond
   ((positive? sr1) (+ sr1 ds))
   ((positive? sr2) (- h sr2))
   (else (* split h))))

(define (calc-vsplit-layout split pos size sr1 sr2)
  (match-let* (((x . y) pos)
               ((w . h) size)
               (split-point (calc-vsplit-point split h sr1 sr2 style:divider-size))
               (left-pos pos)
               (left-size (cons w (- split-point style:divider-size)))
               (right-pos (cons x (+ y split-point)))
               (right-size (cons w (- h split-point))))
              (list left-pos left-size right-pos right-size)))

(define (update-node-layout node pos size)
  (case (node-type node)
    ((leaf)
     (update-view-layout (node-view node) pos size)
     (set-fields node
                 ((node-pos) pos)
                 ((node-size) size)))
    ((vsplit hsplit)
     (match-let* ((flip (if (eq? (node-type node) 'hsplit)
                            (match-lambda
                             ((x . y) (cons y x)))
                            (lambda (x) x)))
                  (left-size-request (get-node-size-request (node-left node)))
                  (right-size-request (get-node-size-request (node-right node)))
                  ((left-pos left-size right-pos right-size)
                   (calc-vsplit-layout
                    (node-split node)
                    (flip pos)
                    (flip size)
                    (cdr (flip left-size-request))
                    (cdr (flip right-size-request)))))
                 (set-fields node
                             ((node-pos) pos)
                             ((node-size) size)
                             ((node-left) (update-node-layout
                                           (node-left node)
                                           (flip left-pos)
                                           (flip left-size)))
                             ((node-right) (update-node-layout
                                            (node-right node)
                                            (flip right-pos)
                                            (flip right-size))))))
    (else #f)))

(define (draw-node node)
  (case (node-type node)
    ((leaf)
     (r:push-clip-rect (node-pos node) (node-size node))
     (view:draw (node-view node))
     (r:pop-clip-rect))
    ((hsplit vsplit)
     (r:add-rect (node-pos node) (node-size node) (face-attribute 'window-divider ':background))
     (draw-node (node-left node))
     (draw-node (node-right node)))
    (else #f)))

(define (update-node node delta)
  (case (node-type node)
    ((leaf)
     (view:update (node-view node) delta))
    ((hsplit vsplit)
     (update-node (node-left node) delta)
     (update-node (node-right node) delta))
    (else #f)))

(define (hit-test node x y)
  (case (node-type node)
    ((leaf)
     node)
    ((hsplit)
     (hit-test (if (< x (car (node-pos (node-right node))))
                   (node-left node)
                   (node-right node))
               x
               y))
    ((vsplit)
     (hit-test (if (< y (cdr (node-pos (node-right node))))
                   (node-left node)
                   (node-right node))
               x
               y))))

(define (switch-view root-view view)
  (set! (view:active? (active-view root-view)) #f)
  (set! (view:active? view) #t)
  (set! (active-view root-view) view))

(define (make-root-view)
  (let* ((buffer-view (make <buffer-view>
                        #:buffer (current-buffer)))
         (buffer-node (make-leaf-node '(0 . 0)
                                      '(0 . 0)
                                      buffer-view
                                      #f))
         (minibuffer-node (split-node buffer-node
                                      'down
                                      #:view (make <minibuffer-view>)
                                      #:locked? #t))
         (root-view (make <root-view>
                      #:pos '(0 . 0)
                      #:size '(0 . 0)
                      #:root-node minibuffer-node
                      #:buffer-view buffer-view)))
    (switch-view root-view buffer-view)
    root-view))

(define (resize-root-view view width height)
  (set! (view:size view) (cons width height))
  (set! (root-node view)
        (update-node-layout (root-node view)
                            (view:pos view)
                            (view:size view))))

(define-method (view:update (view <root-view>) delta)
  (if (> delta 0.0)
      (set! ticks-per-second (/ 1.0 delta)))

  (unless (eq? (current-buffer) minibuffer)
    (switch-buffer-view-buffer (buffer-view view) (current-buffer)))

  (set! (root-node view)
        (update-node-layout (root-node view)
                            (view:pos view)
                            (view:size view)))

  (update-node (root-node view) delta))

(define-public (update-root-view view delta)
  (view:update view delta))

(define-method (view:draw (view <root-view>))
  (draw-node (root-node view))

  (r:add-text (face-attribute 'fixed-pitch ':font)
              '(0 . 10)
              (format #f "FPS: ~,2f" (or ticks-per-second 1))
              (face-attribute 'default ':foreground)))

(define-method (view:mouse-position-callback (view <root-view>) x y)
  (view:mouse-position-callback (active-view view) x y)
  (sys:set-cursor (view:cursor (active-view view)))
  (set! (mouse-pos view) (cons x y)))

(define-method (view:mouse-press-callback (view <root-view>) button x y)
  (let ((node (hit-test (root-node view) x y)))
    (switch-view view (node-view node))
    (view:mouse-press-callback (node-view node) button x y)))

(define-method (view:mouse-release-callback (view <root-view>) button)
  (view:mouse-release-callback (active-view view) button))

(define-method (view:mouse-scroll-callback (view <root-view>) y-offset)
  (view:mouse-scroll-callback (buffer-view view) y-offset))
