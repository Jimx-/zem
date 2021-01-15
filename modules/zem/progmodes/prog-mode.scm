(define-module (zem progmodes prog-mode)
  #:use-module (zem core mode)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 regex)
  #:export (enter-prog-mode))

(define-derived-mode prog-mode fundamental-mode "Prog")
