#lang racket

(provide (all-defined-out))

(require "board.rkt")
(require "xlsx-import-export.rkt")

(define (get-board-element row column) ;;should probably rename this
  (get-element (- column 1) (get-row (- row 1) board)))
  
(define (move-piece tile1 tile2)
  (cond ((or (equal? tile1 '()) (equal? tile2 '()))
         (displayln "Illegal move: invalid coordinates") 'illegal-move)
        ((equal? (tile1 'get-piece) 'red)
         (cond ((equal? (tile2 'get-piece) 'none)
                (begin
                  ((tile1 'set-piece) 'none)
                  ((tile2 'set-piece) 'red)
                  'piece-moved))
               ((equal? (tile2 'get-piece) 'black) (capture tile1 tile2))
               (else (displayln "Illegal move: destination occupied") 'illegal-move)))
        ((equal? (tile1 'get-piece) 'black)
         (cond ((equal? (tile2 'get-piece) 'none)
                (begin
                  ((tile1 'set-piece) 'none)
                  ((tile2 'set-piece) 'black)
                  'piece-moved))
               ((equal? (tile2 'get-piece) 'red) (capture tile1 tile2))
               (else (display "Illegal move: destination occupied") 'illegal-move)))
        (else (displayln "Illegal move: no piece at start") 'illegal-move)))

(define (capture tile1 tile2)
  (define tile3 (get-board-element (+ (tile2 'get-row) (- (tile2 'get-row) (tile1 'get-row)))
                                   (+ (tile2 'get-column) (- (tile2 'get-column) (tile1 'get-column)))))
  (cond ((equal? tile3 '()) "Invalid co-ordinates -- capture")
        ((equal? (tile3 'get-piece) 'none)
         (cond ((equal? (tile1 'get-piece) 'red)
                (begin
                  ((tile1 'set-piece) 'none)
                  ((tile2 'set-piece) 'none)
                  ((tile3 'set-piece) 'red)
                  ;'piece-captured))
                  (continue-capture tile3 'red)))
               ((equal? (tile1 'get-piece) 'black)
                (begin
                  ((tile1 'set-piece) 'none)
                  ((tile2 'set-piece) 'none)
                  ((tile3 'set-piece) 'black)
                  ;'piece-captured))
                  (continue-capture tile3 'black)))
               (else "no piece at start -- capture")))
        (else "destination is occupied -- capture")))

(define (continue-capture tile color)
  (let ((options (if (equal? color 'red)
                     (filter (lambda (t) (< (t 'get-row) (tile 'get-row))) (find-adjacent-opponents tile color)) ;tiles in row above with opponent piece
                     (filter (lambda (t) (> (t 'get-row) (tile 'get-row))) (find-adjacent-opponents tile color))))) ;in row below
    (cond ((null? options) 'capture-done)
          ((= 1 (length options)) (capture tile (car options)))
          (else (capture tile (choose-next-capture options))))))

(define (choose-next-capture lst)
  (begin (displayln (draw-board))
         (display "Choose next piece to capture: ")
         (map (lambda (t) (display (integer->char (+ (t 'get-column) 64))) (display (t 'get-row)) (display " ")) lst)
         (newline)
         (car lst))) ;change this - for now just selects the car instead of asking user

(define (find-adjacent-opponents tile color)
  (filter (lambda (t) (and (not (null? t))
                           (equal? (t 'get-piece)
                                   (if (equal? color 'red)
                                       'black
                                       'red))))
          (list (get-board-element (- (tile 'get-row) 1) (+ (tile 'get-column) 1))
                (get-board-element (- (tile 'get-row) 1) (- (tile 'get-column) 1))
                (get-board-element (+ (tile 'get-row) 1) (+ (tile 'get-column) 1))
                (get-board-element (+ (tile 'get-row) 1) (- (tile 'get-column) 1))))) 

(define (move-in-direction tile direction)
  (cond ((equal? direction 'north)
         (move-piece tile (get-board-element (- (tile 'get-row) 1) (tile 'get-column))))
        ((equal? direction 'south)
         (move-piece tile (get-board-element (+ (tile 'get-row) 1) (tile 'get-column))))
        ((equal? direction 'east)
         (move-piece tile (get-board-element (tile 'get-row) (+ (tile 'get-column) 1))))
        ((equal? direction 'west)
         (move-piece tile (get-board-element (tile 'get-row) (- (tile 'get-column) 1))))
        ((equal? direction 'northeast)
         (move-piece tile (get-board-element (- (tile 'get-row) 1) (+ (tile 'get-column) 1))))
        ((equal? direction 'northwest)
         (move-piece tile (get-board-element (- (tile 'get-row) 1) (- (tile 'get-column) 1))))
        ((equal? direction 'southeast)
         (move-piece tile (get-board-element (+ (tile 'get-row) 1) (+ (tile 'get-column) 1))))
        ((equal? direction 'southwest)
         (move-piece tile (get-board-element (+ (tile 'get-row) 1) (- (tile 'get-column) 1))))
        (else "invalid direction -- move-in-direction")))
