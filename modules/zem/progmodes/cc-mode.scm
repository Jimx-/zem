(define-module (zem progmodes cc-mode)
  #:use-module (zem core mode)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 regex)
  #:export (enter-c++-mode))

(define-derived-mode c++-mode fundamental-mode "C++")

(set! auto-mode-alist (cons `(,(make-regexp "\\.cpp\\'") . ,enter-c++-mode) auto-mode-alist))
