#lang racket

(require 2htdp/image)

(define max-row 8)
(define max-column 8)
(define square-size 30)

(define (make-square row column piece)
  (define (get-row) row)
  (define (get-column) column)
  (define (get-piece) piece)
  (define (set-piece p) (if (or (eq? p 'none) (eq? p 'red) (eq? p 'black))
                            (begin (set! piece p)
                                   piece)
                            #f))
  (define (dispatch m)
    (cond ((eq? m 'get-row) (get-row))
          ((eq? m 'get-column) (get-column))
          ((eq? m 'get-piece) (get-piece))
          ((eq? m 'set-piece) set-piece)
          (else (error "Invalid option -- MAKE-SQUARE DISPATCH" m))))
  (if #t;(set-piece piece)
      dispatch
      (error "Invalid piece type -- MAKE-SQUARE" piece)))

(define make-board
  (lambda ()
    (define (make-row row)
      (define (make-column column)
        (if (> column max-column)
            '()
            (cons (make-square row column 'none) (make-column (+ column 1)))))
      (if (> row max-row)
          '()
          (cons (make-column 1) (make-row (+ row 1)))))
    (make-row 1)))

(define board (make-board))

(define (draw-row r) (foldr beside empty-image (map square-to-image r)))
(define (draw-board) (overlay (foldr above empty-image (map draw-row board))
                              (square (+ (* square-size 8) 2) 'solid 'black)))

(define (square-to-image sq) (underlay (square square-size 'solid (if (same-arity? (sq 'get-row) (sq 'get-column))
                                                                      'firebrick ;to make distinct from red piece color
                                                                      'white)) ;to make distinct from black piece color
                                       (let ((p (sq 'get-piece)))
                                         (if (eq? p 'none)
                                             empty-image
                                             (circle (- (/ square-size 2) (/ square-size 8)) 'solid p)))))

(define (same-arity? a b) (if (or (and (odd? a) (odd? b))
                                  (and (even? a) (even? b)))
                              #t
                              #f))
                                
;(foldr beside empty-image (list (square 30 'solid 'red) (square 30 'solid 'black) (square 30 'solid 'red) (square 30 'solid 'black)))