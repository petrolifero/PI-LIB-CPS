#lang racket
(struct add (a b) #:transparent)
(struct sub (a b) #:transparent)
(struct mul (a b) #:transparent)
(struct div (a b) #:transparent)

(struct orS (a b) #:transparent)
(struct andS (a b) #:transparent)

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

(define (or& a b sucesso falha )
    (if (and (boolean? a) (boolean? b))
        (sucesso (or a b))
        (falha "deu merda")))

(define (and& a b sucesso falha )
    (if (and (boolean? a) (boolean? b))
        (sucesso (and a b))
        (falha "deu merda")))

(define (>=& a b sucesso falha )
    (if (and (number? a) (number? b))
        (sucesso (>= a b))
        (falha "deu merda")))


(struct ambiente (amb mem loca) #:transparent)

(define (eval-pi-lib exp ambiente sucesso falha)
    (match exp
        [(? boolean? a) (sucesso a)]
        [(? number? a) (sucesso a)]
        [(add a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (+& v1 v2 sucesso falha)) falha)) falha)]
        [(sub a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (-& v1 v2 sucesso falha)) falha)) falha)]
        [(mul a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (*& v1 v2 sucesso falha)) falha)) falha)]
        [(div a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (/& v1 v2 sucesso falha)) falha)) falha)]
	[(orS a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (or& v1 v2 sucesso falha)) falha)) falha)]
	[(andS a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (and& v1 v2 sucesso falha)) falha)) falha)]
	[(ge a b) (eval-pi-lib a ambiente (lambda (v1) (eval-pi-lib b ambiente (lambda (v2) (>=& v1 v2 sucesso falha)) falha)) falha)]
	[(gt a b) ()]
	[(lt a b)]
	[(neg a)]
	[(eq a b)]
	[(le a b)]))

