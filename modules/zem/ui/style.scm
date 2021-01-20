(define-module (zem ui style)
  #:use-module (zem api font))

(define-public padding '(7 . 7))

(define-public background-color #x272822)
(define-public mode-line-background-color #x606060)
(define-public text-color #xf8f8f2)
(define-public highlight-color #x333428)
(define-public highlight2-color #x414236)
(define-public keyword-color #xf92672)
(define-public operator-color #xf92672)
(define-public symbol-color #x66d9ef)
(define-public string-color #xe6db74)
(define-public type-color #x66d9ef)
(define-public number-color #xae81ff)
(define-public function-color #xa6e22e)
(define-public comment-color #x404040)
(define-public line-number-color #x404040)
(define-public line-number-background-color #x202020)

(define-public divider-color #x0f0f0f)
(define-public divider-size 1)

(define-public scrollbar-color #x404040)
(define-public scrollbar-size 6)

(define-public font (load-font
                     "monospace.ttf"
                     13))

(define-public caret-width 2)
(define-public caret-color #x808080)
