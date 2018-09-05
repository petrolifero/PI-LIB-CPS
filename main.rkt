#lang racket
(struct add (a b) #:transparent)
(struct sub (a b) #:transparent)
(struct mul (a b) #:transparent)
(struct div (a b) #:transparent)

(define (+& a b sucesso falha )
    (if (and (number? a) (number? b))
        (sucesso (+ a b))
        (falha "deu merda")))

(define (-& a b sucesso falha )
    (if (and (number? a) (number? b))
        (sucesso (- a b))
        (falha "deu merda")))
        
(define (*& a b sucesso falha )
    (if (and (number? a) (number? b))
        (sucesso (* a b))
        (falha "deu merda")))
        
(define (/& a b sucesso falha )
    (if (and (number? a) (number? b) (not (equal? b 0)))
        (sucesso (/ a b))
        (falha "deu merda")))

(struct ambiente (amb mem loca) #:transparent)

(define (eval-pi-lib exp ambiente sucesso falha)
    (match exp
        [(? boolean? a) (sucesso a)]
        [(? number? a) (sucesso a)]
        [(add a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (+& v1 v2 sucesso falha)) falha)) falha)]
        [(sub a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (-& v1 v2 sucesso falha)) falha)) falha)]
        [(mul a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (*& v1 v2 sucesso falha)) falha)) falha)]
        [(div a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (/& v1 v2 sucesso falha)) falha)) falha)]))

(eval-pi-lib (mul (div 1 3) 2) (ambiente (hash) (hash) '()) display display)
