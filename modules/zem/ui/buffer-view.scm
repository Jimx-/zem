(define-module (zem ui buffer-view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module ((zem api font) #:prefix f:)
  #:use-module (zem core buffer)
  #:use-module (zem core commands)
  #:use-module (zem core text-prop)
  #:use-module (zem ui view)
  #:use-module (zem ui root-view)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module (zem util plist)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 gap-buffer)
  #:use-module (oop goops)
  #:export (<buffer-view>))

(define-class <buffer-view> (<view>)
  (buffer #:init-keyword #:buffer #:accessor buffer-view:buffer)
  (mouse-selecting? #:init-value #f #:accessor buffer-view:mouse-selecting?)
  (last-point #:init-form '(0 . 1) #:accessor buffer-view:last-point)
  (last-visible-point-min #:init-form '(0 . 1) #:accessor buffer-view:last-visible-point-min))

(define-method (initialize (view <buffer-view>) . args)
  (next-method)
  (set! (view:cursor view) 'i-beam))

(define show-caret? #f)
(define (blink-period)
  (if ticks-per-second
      (* 0.5 ticks-per-second)
      1))
(define (toggle-caret)
  (set! show-caret? (not show-caret?))
  (agenda-schedule toggle-caret (blink-period)))
(agenda-schedule toggle-caret (blink-period))

(define (get-line-height)
  (floor (* (f:get-font-height style:font) 1.1)))

(define (get-text-y-offset)
  (floor (/ (f:get-font-height style:font) 4.0)))

(define (get-text-x-offset line-max)
  (match-let* (((w . _) (r:text-size-hint style:font (number->string line-max))))
              (+ w (floor (* 1.5 (car style:padding))))))

(define (get-visible-line-range view line-max)
  (match-let* (((x0 y0 x1 y1) (view:get-visible-bbox view))
               (lh (get-line-height))
               (visible-line-min (max 1 (floor (/ y0 lh))))
               (visible-line-max (min line-max (1+ (floor (/ y1 lh))))))
              (cons (inexact->exact visible-line-min) (inexact->exact visible-line-max))))

(define (get-selection view)
  (let ((point (car (buffer-view:last-point view)))
        (mark (mark)))
    (and mark
         (cons (min mark point)
               (max mark point)))))

(define-method (view:get-scroll-limit (view <buffer-view>))
  (with-buffer (buffer-view:buffer view)
    (* (get-line-height) (- (count-lines (point-min) (point-max)) 1))))

(define (collect-line/recur)
    (let ((c (char-after)))
      (cond
       ((= (point) (point-max))
        '())
       ((eqv? c #\newline)
        (forward-char)
        '())
       (else
        (forward-char)
        (cons c (collect-line/recur))))))
(define (collect-line)
  (apply string (collect-line/recur)))

(define (syntax->color syn)
  (case syn
    ((keyword special) style:keyword-color)
    ((symbol) style:text-color)
    ((string) style:string-color)
    ((type) style:type-color)
    ((operator) style:operator-color)
    ((function) style:function-color)
    ((number) style:number-color)
    ((comment) style:comment-color)
    (else style:text-color)))

(define (draw-caret pos line-height)
  (when show-caret?
    (r:add-rect pos (cons style:caret-width line-height) style:caret-color)))

(define (draw-line-number num x y width)
  (r:add-text style:font
              (cons x
                    (- y (get-text-y-offset)))
              (format #f (string-append "~" (number->string width) "@a") num)
              style:line-number-color))

(define (intersect-selection view fragment start)
  (let ((selection (get-selection view))
        (len (string-length fragment)))
    (and selection
         (cond
          ((>= start (cdr selection)) #f)
          ((<= (+ start len) (car selection)) #f)
          (else
           (cons
                 (max 0 (min (- (car selection) start) len))
                 (max 0 (min (- (cdr selection) start) len))))))))

(define (draw-word view line col x y word color line-height cur-point point-line point-col)
  (let ((draw-caret? (and (view:active? view)
                          (= line point-line)))
        (new-col (+ (string-length word) col))
        (new-point (+ (string-length word) cur-point))
        (selection (intersect-selection view word cur-point)))
    (when selection
      (let ((lx (car (r:text-size-hint style:font (substring word 0 (car selection)))))
            (rx (car (r:text-size-hint style:font (substring word 0 (cdr selection))))))
        (r:add-rect (cons (+ x lx) (- y line-height))
                    (cons (- rx lx) line-height)
                    style:highlight2-color)))
    (when (and draw-caret?
               (>= point-col col)
               (< point-col new-col))
      (match-let* ((split (- point-col col))
                   (head (substring word 0 split))
                   ((hx . _) (if (string-null? head)
                                 (cons 0 0)
                                 (r:text-size-hint style:font head))))
                  (draw-caret (cons (+ x hx) (- y line-height))
                              line-height)))
    (list
     new-col
     (car (r:add-text style:font
                      (cons x
                            (- y (get-text-y-offset)))
                      word
                      color))
     new-point)))

(define (draw-mode-line x y width height mode-line)
  (r:add-rect (cons x y)
              (cons width height)
              style:mode-line-background-color)
  (r:add-text style:font
              (cons (+ x (car style:padding))
                    (- (+ y height) (get-text-y-offset)))
              mode-line
              style:text-color))


(define (draw-text-fragment view line col x y fragment color x-min line-height cur-point point-line point-col)
  (if (string-null? fragment)
      (list line col x y cur-point)
      (let ((nlidx (string-index fragment #\newline)))
        (if nlidx
            (match-let* ((word (substring fragment 0 (1+ nlidx)))
                         (rest (substring fragment (1+ nlidx)))
                         ((_ _ npoint) (draw-word view
                                                  line
                                                  col
                                                  x
                                                  y
                                                  word
                                                  color
                                                  line-height
                                                  cur-point
                                                  point-line
                                                  point-col)))
                        (draw-text-fragment view
                                            (1+ line)
                                            0
                                            x-min
                                            (+ y line-height)
                                            rest
                                            color
                                            x-min
                                            line-height
                                            npoint
                                            point-line
                                            point-col))
            (match-let (((new-col nx npoint) (draw-word view
                                                   line
                                                   col
                                                   x
                                                   y
                                                   fragment
                                                   color
                                                   line-height
                                                   cur-point
                                                   point-line
                                                   point-col)))
                       (list line new-col nx y npoint))))))

(define (draw-gutter/recur view lidx visible-line-max line-max line-x text-x view-width y text-height view-y line-height point-line)
    (if (and (< (point) (point-max))
             (< y (+ text-height line-height))
             (<= lidx visible-line-max))
        (let* ((line (collect-line))
               (line-y (+ view-y y)))
          (when (and
                 (view:active? view)
                 (= lidx point-line))
            ;; Highlight current line
            (r:add-rect (cons text-x (- line-y line-height))
                        (cons view-width line-height)
                        style:highlight-color))
          (draw-line-number lidx
                            line-x
                            line-y
                            (string-length (number->string line-max)))
          (cons line (draw-gutter/recur view
                       (1+ lidx)
                       visible-line-max
                       line-max
                       line-x
                       text-x
                       view-width
                       (+ y line-height)
                       text-height
                       view-y
                       line-height
                       point-line)))
          '()))

(define (draw-gutter view lidx visible-line-max line-max line-x text-x view-width y text-height view-y line-height point-line)
  (let ((ls (draw-gutter/recur view
                               lidx
                               visible-line-max
                               line-max
                               line-x
                               text-x
                               view-width
                               y
                               text-height
                               view-y
                               line-height
                               point-line)))
    (cons (point) (string-join ls "\n"))))

(define (draw-intervals view lines intervals line col tx ty text-x line-height hl-min cur-point point-line point-col last-end)
  (let ((lines-offset (lambda (pt)  ;; Map points to offsets within lines
                        (min (string-length lines)
                             (max 0 (- pt hl-min))))))
    (if (not (null? intervals))
        (match-let*
         ((((start . end) . props) (car intervals))
          (token-min (lines-offset start))
          (token-max (lines-offset end))
          (filler (substring lines last-end token-min))
          (text (substring lines token-min token-max))
          ((fline fcol fx fy fpoint)
           ;; Draw filler text between the previous and the current interval
           (draw-text-fragment view
                               line
                               col
                               tx
                               ty
                               filler
                               style:text-color
                               text-x
                               line-height
                               cur-point
                               point-line
                               point-col))
          ((nline ncol nx ny npoint)
           ;; Draw the current interval
           (draw-text-fragment view
                               fline
                               fcol
                               fx
                               fy
                               text
                               (syntax->color (plist-get props 'syntax))
                               text-x
                               line-height
                               fpoint
                               point-line
                               point-col)))
         (draw-intervals view
                         lines
                         (cdr intervals)
                         nline
                         ncol
                         nx
                         ny
                         text-x
                         line-height
                         hl-min
                         npoint
                         point-line
                         point-col
                         token-max))
        ;; Draw text at the end
        (draw-text-fragment view
                            line
                            col
                            tx
                            ty
                            (substring lines last-end)
                            style:text-color
                            text-x
                            line-height
                            cur-point
                            point-line
                            point-col))))

(define (draw-scrollbar view visible-line-min visible-line-max line-max)
  (match-let* (((view-x . view-y) (view:pos view))
               ((view-width . view-height) (view:size view))
               (x (- (+ view-x view-width) style:scrollbar-size))
               (y (+ view-y (* view-height (/ visible-line-min line-max))))
               (h (max 20 (/ (- visible-line-max visible-line-min) line-max))))
              (r:add-rect (cons x y) (cons style:scrollbar-size h) style:scrollbar-color)))

(define (move-to-visible-point-min view visible-line-min)
  (if (not (= visible-line-min (cdr (buffer-view:last-visible-point-min view))))
      (begin
        (goto-line visible-line-min)
        (move-beginning-of-line)
        (set! (buffer-view:last-visible-point-min view) (cons (point) visible-line-min)))
      (goto-char (car (buffer-view:last-visible-point-min view))))
  (point))

(define-method (view:draw (view <buffer-view>))
  (draw-view-background view style:background-color)

  (with-buffer
      (buffer-view:buffer view)
    (when (not (= (point) (car (buffer-view:last-point view))))
      (update-point view))
    (let ((point-line (max 1 (cdr (buffer-view:last-point view))))
          (point-col (current-column))
          (mode-line (emacsy-mode-line)))
      (save-excursion
          (match-let*
           (((view-x . view-y) (view:pos view))           ;; View position
            ((view-width . view-height) (view:size view)) ;; View size
            (lh (get-line-height))                        ;; Line height in pixels
            (text-height (- view-height lh))              ;; Text area height
            (line-max                                     ;; Last line of buffer
             (count-lines (point-min) (point-max)))       ;;
            ((visible-line-min . visible-line-max)        ;; Visible line range
             (get-visible-line-range view line-max))      ;;
            (line-x (+ (car style:padding) view-x))       ;; Left of the gutter
            (text-x (+ (get-text-x-offset line-max) view-x))
            (mode-line-y text-height)
            (hl-min (move-to-visible-point-min view visible-line-min)))
           ;; Draw background for gutter
           (r:add-rect (cons view-x view-y)
                       (cons text-x text-height)
                       style:line-number-background-color)
           ;; Iterate each line
           (match-let*
            (((hl-max . lines) (draw-gutter view
                                            visible-line-min
                                            visible-line-max
                                            line-max
                                            line-x
                                            text-x
                                            view-width
                                            lh
                                            text-height
                                            view-y
                                            lh
                                            point-line))
             (intervals (text-property-list (current-buffer)
                                            hl-min
                                            hl-max)))
            (draw-intervals view
                            lines
                            intervals
                            visible-line-min
                            0
                            text-x
                            lh
                            text-x
                            lh
                            hl-min
                            hl-min
                            point-line
                            point-col
                            0))
           (draw-scrollbar view visible-line-min visible-line-max line-max)
           (draw-mode-line view-x mode-line-y view-width lh mode-line))))))

(define-public (switch-buffer-view-buffer view buffer)
  (set! (buffer-view:buffer view) buffer))

(define (scroll-to-point view)
  (let* ((size (view:size view))
         (point-line (cdr (buffer-view:last-point view)))
         (y-min (* (get-line-height) (- point-line 2)))
         (y-max (- (* (get-line-height) (+ point-line 5)) (cdr size)))
         (scroll-target (view:scroll-target view)))
    (set! (view:scroll-target view)
          (cons (car scroll-target)
                (max y-max (min y-min (cdr scroll-target)))))
    point-line))

(define (update-point view)
  (set! (buffer-view:last-point view) (cons (point) (line-number-at-pos))))

(define-method (view:update (view <buffer-view>) delta)
  (with-buffer (buffer-view:buffer view)
    (when (not (= (point) (car (buffer-view:last-point view))))
      (update-point view)
      (scroll-to-point view)))
  (next-method))

(define (move-point-to view x y)
  (match-let* (((left . top) (view:pos view))
               (line-max (count-lines (point-min) (point-max)))
               ((visible-line-min . _) (get-visible-line-range view line-max))
               (line (+ visible-line-min
                        (inexact->exact
                             (floor (/ (- y top)
                                       (get-line-height))))))
               (line-text (begin
                            (goto-line line)
                            (move-beginning-of-line)
                            (save-excursion
                             (collect-line))))
               (text-x (+ left
                          (get-text-x-offset line-max)))
               (col (r:char-offset style:font line-text (- x text-x))))
              (forward-char col)
              (cons line col)))

(define-method (view:mouse-position-callback (view <buffer-view>) x y)
  (when (buffer-view:mouse-selecting? view)
    (with-buffer (buffer-view:buffer view)
                 (let ((old-point (point)))
                   (move-point-to view x y)
                   (unless (eq? (point) old-point)
                     (set! (buffer-view:mouse-selecting? view) 'moved))))))

(define-method (view:mouse-press-callback (view <buffer-view>) button x y)
  (case button
    ((left)
     (with-buffer (buffer-view:buffer view)
                  (move-point-to view x y)
                  (set-mark (point))
                  (set! (buffer-view:mouse-selecting? view) 'clicked)))))

(define-method (view:mouse-release-callback (view <buffer-view>) button)
  (next-method)
  (unless (eq? (buffer-view:mouse-selecting? view) 'moved)
    (set-mark #f))
  (set! (buffer-view:mouse-selecting? view) #f))
