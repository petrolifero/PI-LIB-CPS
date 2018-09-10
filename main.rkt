#lang racket
(struct add (a b) #:transparent)
(struct sub (a b) #:transparent)
(struct mul (a b) #:transparent)
(struct div (a b) #:transparent)

(struct orS (a b) #:transparent)
(struct andS (a b) #:transparent)

(struct ge (a b) #:transparent)
(struct gt (a b) #:transparent)
(struct lt (a b) #:transparent)
(struct le (a b) #:transparent)

(struct location (a) #:transparent)


(define (+& a b sucesso)
        (sucesso (+ a b)))

(define (-& a b sucesso)
        (sucesso (- a b)))
        
(define (*& a b sucesso)
        (sucesso (* a b)))
        
(define (/& a b sucesso)
        (sucesso (/ a b)))

(define (or& a b sucesso)
        (sucesso (or a b)))

(define (and& a b sucesso)
        (sucesso (and a b)))

(define (>=& a b sucesso)
        (sucesso (>= a b)))

(define (>& a b sucesso)
        (sucesso (> a b)))

(define (<& a b sucesso)
        (sucesso (< a b)))

(define (<=& a b sucesso)
        (sucesso (<= a b)))

(define (eval-identifier& str ambiente memoria localização sucesso)
  (if (hash-has-key? ambiente str)
      (let ([content (hash-ref ambiente str)])
        (if (location? content)
            (sucesso (hash-ref memoria content))
            (sucesso content)))
      (raise "uso indevido de variavel não declarada")))

(define (eval-pi-lib exp ambiente memoria localização sucesso)
    (match exp
        [(? boolean? a) (sucesso a)]
        [(? number? a) (sucesso a)]
        [(? string? str) (eval-identifier& str ambiente memoria localização sucesso)]
        [(add a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (+& v1 v2 sucesso)))))]
        [(sub a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (-& v1 v2 sucesso)))))]
        [(mul a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (*& v1 v2 sucesso)))))]
        [(div a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (/& v1 v2 sucesso)))))]
	[(orS a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (or& v1 v2 sucesso)))))]
	[(andS a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (and& v1 v2 sucesso)))))]
        [(gt a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (>& v1 v2 sucesso)))))]
	[(ge a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (>=& v1 v2 sucesso)))))]
        [(lt a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (<& v1 v2 sucesso)))))]
        [(le a b) (eval-pi-lib a ambiente memoria localização (lambda (v1) (eval-pi-lib b ambiente memoria localização (lambda (v2) (<=& v1 v2 sucesso)))))]))





