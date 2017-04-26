;; Checkers project
;; Functions for drawing and manipulating board/square objects

#lang racket

(provide (all-defined-out))

(require 2htdp/image)

(define max-row 8)
(define max-column 8)
(define square-size 30)
(define circle-radius (- (/ square-size 2) (/ square-size 8)))

(define (make-square row column piece is-king)
  (define (get-row) row)
  (define (get-column) column)
  (define (get-piece) piece)
  (define (set-piece p [king #f])
    (if (or (eq? p 'none) (eq? p 'red) (eq? p 'black))
        (begin (set! piece p)
               (cond ((eq? p 'none) (set-king #f))
                     (king (set-king king))
                     (else (void)))
               piece)
        #f))
  (define (king?) is-king)
  (define (set-king t/f) (begin (set! is-king t/f)
                                is-king))
  (define (dispatch m)
    (cond ((eq? m 'get-row) (get-row))
          ((eq? m 'get-column) (get-column))
          ((eq? m 'get-piece) (get-piece))
          ((eq? m 'set-piece) set-piece)
          ((eq? m 'king?) (king?))
          ((eq? m 'set-king) set-king)
          (else (error "Invalid option -- MAKE-SQUARE DISPATCH" m))))
  (if (set-piece piece)
      dispatch
      (error "Invalid piece type -- MAKE-SQUARE" piece)))

(define make-board
  (lambda ()
    (define (make-row row)
      (define (make-column column)
        (if (> column max-column)
            '()
            (cons (make-square row column 'none #f) (make-column (+ column 1)))))
      (if (> row max-row)
          '()
          (cons (make-column 1) (make-row (+ row 1)))))
    (make-row 1)))

(define board (make-board))

(define (draw-row r) (foldr beside empty-image (map square-to-image r)))

;; This is the top level function for creating the board image
(define (draw-board) (above/align 'right
                                   (beside (row-labels) (draw-board-without-labels))
                                   (column-labels)))

(define (draw-board-without-labels) (overlay (foldr above empty-image (map draw-row board))
                                             (square (+ (* square-size 8) 4) 'solid 'black)))

(define (square-to-image sq) (underlay (square square-size 'solid (if (red-square? sq)
                                                                      'firebrick ;to make distinct from red piece color
                                                                      'white)) ;to make distinct from black piece color
                                       (let ((p (sq 'get-piece)))
                                         (if (eq? p 'none)
                                             empty-image
                                             (overlay (if (sq 'king?)
                                                          (star (- circle-radius 1) 'solid 'gold)
                                                          empty-image)
                                                      (circle circle-radius 'solid p)
                                                      (circle (+ circle-radius 1) 'solid 'black))))))

(define (row-labels) (overlay (foldr above empty-image (map draw-char (list "1" "2" "3" "4" "5" "6" "7" "8")))
                              (rectangle (+ square-size 4) (+ (* square-size 8) 4) 'solid 'white)))

(define (column-labels) (overlay (foldr beside empty-image (map draw-char (list "A" "B" "C" "D" "E" "F" "G" "H")))
                                 (rectangle (+ (* square-size 8) 4) (+ square-size 4) 'solid 'white)))

(define (draw-char c) (overlay (text c (- square-size 5) 'black)
                               (square square-size 'solid 'white)))

(define (same-arity? a b) (if (or (and (odd? a) (odd? b))
                                  (and (even? a) (even? b)))
                              #t
                              #f))

(define (white-square? sq) (same-arity? (sq 'get-row) (sq 'get-column)))

(define (red-square? sq) (not (white-square? sq)))


(define (get-square row-num column-num board)
  (let ((row (get-list-item (- row-num 1) board)))
    (get-list-item (- column-num 1) row)))

(define (get-list-item index lst)
  (cond ((null? lst) lst)
        ((<= index 0) (car lst))
        (else (get-list-item (- index 1) (cdr lst)))))
