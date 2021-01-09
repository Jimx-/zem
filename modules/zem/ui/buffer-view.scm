(define-module (zem ui buffer-view)
  #:use-module ((zem api renderer) #:prefix r:)
  #:use-module ((zem api font) #:prefix f:)
  #:use-module (zem core buffer)
  #:use-module (zem core text-prop)
  #:use-module (zem ui view)
  #:use-module (zem ui root-view)
  #:use-module ((zem ui style) #:prefix style:)
  #:use-module ((zem syntax tree-sitter) #:prefix ts:)
  #:use-module (zem util plist)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 gap-buffer)
  #:use-module (oop goops)
  #:export (<buffer-view>))

(define-class <buffer-view> (<view>)
  (buffer #:init-keyword #:buffer #:accessor buffer-view:buffer)
  (last-point #:init-value 0 #:accessor buffer-view:last-point))

(define show-caret? #f)
(define (blink-period)
  (if ticks-per-second
      (* 0.5 ticks-per-second)
      1))
(define (toggle-caret)
  (set! show-caret? (not show-caret?))
  (queue-redraw)
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

(define-method (view:get-scroll-limit (view <buffer-view>))
  (with-buffer (buffer-view:buffer view)
    (* (get-line-height) (- (count-lines (point-min) (point-max)) 1))))

(define (collect-line acc)
    (let ((c (char-after)))
      (cond
       ((= (point) (point-max))
        acc)
       ((eqv? c #\newline)
        (forward-char)
        acc)
       (else
        (forward-char)
        (collect-line (string-append acc (string c)))))))

(define (syntax->color syn)
  (case syn
    ((keyword special) style:keyword-color)
    ((symbol) style:text-color)
    ((string) style:string-color)
    ((type) style:type-color)
    ((operator) style:operator-color)
    ((function) style:function-color)
    ((number) style:number-color)
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

(define (draw-word line col x y word color line-height point-line point-col)
  (let ((draw-caret? (= line point-line))
        (new-col (+ (string-length word) col)))
    (if (and draw-caret?
             (>= point-col col)
             (< point-col new-col))
        ;; Caret is inside this fragment
        (match-let* ((split (- point-col col))
                     (head (substring word 0 split))
                     (tail (substring word split))
                     ((hx . _) (if (string-null? head)
                                   (cons x y)
                                   (r:add-text style:font
                                               (cons x
                                                     (- y (get-text-y-offset)))
                                               head
                                               color)))
                     ((nx . _) (if (string-null? tail)
                                   (cons hx y)
                                   (r:add-text style:font
                                               (cons hx
                                                     (- y (get-text-y-offset)))
                                               tail
                                               color))))
                    (draw-caret (cons hx (- y line-height))
                                line-height)
                    (cons new-col nx))
        ;; Normal draw
        (cons
         new-col
         (car (r:add-text style:font
                          (cons x
                                (- y (get-text-y-offset)))
                          word
                          color))))))

(define (draw-mode-line x y width height mode-line)
  (r:add-rect (cons x y)
              (cons width height)
              style:mode-line-background-color)
  (r:add-text style:font
              (cons (+ x (car style:padding))
                    (- (+ y height) (get-text-y-offset)))
              mode-line
              style:text-color))


(define (draw-text-fragment line col x y fragment color x-min line-height point-line point-col)
  (if (string-null? fragment)
      (list line col x y)
      (let ((nlidx (string-index fragment #\newline)))
        (if nlidx
            (let ((word (substring fragment 0 (1+ nlidx)))
                  (rest (substring fragment (1+ nlidx))))
              (draw-word line
                         col
                         x
                         y
                         word
                         color
                         line-height
                         point-line
                         point-col)
              (draw-text-fragment (1+ line)
                                  0
                                  x-min
                                  (+ y line-height)
                                  rest
                                  color
                                  x-min
                                  line-height
                                  point-line
                                  point-col))
            (match-let (((new-col . nx) (draw-word line
                                                   col
                                                   x
                                                   y
                                                   fragment
                                                   color
                                                   line-height
                                                   point-line
                                                   point-col)))
                       (list line new-col nx y))))))

(define (draw-gutter lidx visible-line-max line-max line-x text-x view-width y text-height view-y line-height point-line lines)
    (if (and (< (point) (point-max))
             (< y (+ text-height line-height))
             (<= lidx visible-line-max))
        (let* ((line (collect-line ""))
               (line-y (+ view-y y)))
          (when (= lidx point-line)
            ;; Highlight current line
            (r:add-rect (cons text-x (- line-y line-height))
                        (cons view-width line-height)
                        style:highlight-color))
          (draw-line-number lidx
                            line-x
                            line-y
                            (string-length (number->string line-max)))
          (draw-gutter (1+ lidx)
                       visible-line-max
                       line-max
                       line-x
                       text-x
                       view-width
                       (+ y line-height)
                       text-height
                       view-y
                       line-height
                       point-line
                       (string-append lines line (string #\newline))))
        (cons (point) lines)))

(define (draw-intervals lines intervals line col tx ty text-x line-height hl-min point-line point-col last-end)
  (let ((lines-offset (lambda (pt)  ;; Map points to offsets within lines
                        (min (string-length lines)
                             (max 0 (- (1+ pt) hl-min))))))
    (if (not (null? intervals))
        (match-let*
         ((((start . end) . props) (car intervals))
          (token-min (lines-offset start))
          (token-max (lines-offset end))
          (filler (substring lines last-end token-min))
          (text (substring lines token-min token-max))
          ((fline fcol fx fy)
           ;; Draw filler text between the previous and the current interval
           (draw-text-fragment line
                               col
                               tx
                               ty
                               filler
                               style:text-color
                               text-x
                               line-height
                               point-line
                               point-col))
          ((nline ncol nx ny)
           ;; Draw the current interval
           (draw-text-fragment fline
                               fcol
                               fx
                               fy
                               text
                               (syntax->color (plist-get props 'syntax))
                               text-x
                               line-height
                               point-line
                               point-col)))
         (draw-intervals lines
                         (cdr intervals)
                         nline
                         ncol
                         nx
                         ny
                         text-x
                         line-height
                         hl-min
                         point-line
                         point-col
                         token-max))
        ;; Draw text at the end
        (draw-text-fragment line
                            col
                            tx
                            ty
                            (substring lines last-end)
                            style:text-color
                            text-x
                            line-height
                            point-line
                            point-col))))

(define-method (view:draw (view <buffer-view>))
  (draw-view-background view style:background-color)

  (with-buffer
   (buffer-view:buffer view)
   (let ((point-line (max 1 (line-number-at-pos)))
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
        (hl-min (begin
                  ;; Move to the beginnin of the first visible line
                  (goto-line visible-line-min)
                  (move-beginning-of-line)
                  (point))))
       ;; Draw background for gutter
       (r:add-rect (cons view-x view-y)
                   (cons text-x text-height)
                   style:line-number-background-color)
       ;; Iterate each line
       (match-let*
        (((hl-max . lines) (draw-gutter visible-line-min
                                        visible-line-max
                                        line-max
                                        line-x
                                        text-x
                                        view-width
                                        lh
                                        text-height
                                        view-y
                                        lh
                                        point-line
                                        ""))
         (intervals (begin
                      (ts:highlight-region (current-buffer)
                                           hl-min
                                           hl-max)
                      (text-property-list (current-buffer)
                                          hl-min
                                          hl-max))))
        (draw-intervals lines
                        intervals
                        visible-line-min
                        0
                        text-x
                        lh
                        text-x
                        lh
                        hl-min
                        point-line
                        point-col
                        0))
       (draw-mode-line view-x mode-line-y view-width lh mode-line))))))

(define-public (switch-buffer-view-buffer view buffer)
  (set! (buffer-view:buffer view) buffer))

(define (scroll-to-point view)
  (let* ((size (view:size view))
         (point-line (line-number-at-pos))
         (y-min (* (get-line-height) (- point-line 2)))
         (y-max (- (* (get-line-height) (+ point-line 5)) (cdr size)))
         (scroll-target (view:scroll-target view)))
    (set! (view:scroll-target view)
          (cons (car scroll-target)
                (max y-max (min y-min (cdr scroll-target)))))))

(define-method (view:update (view <buffer-view>) delta)
  (with-buffer (buffer-view:buffer view)
    (if (not (= (point) (buffer-view:last-point view)))
        (scroll-to-point view)
        (set! (buffer-view:last-point view) (point))))
  (next-method))
