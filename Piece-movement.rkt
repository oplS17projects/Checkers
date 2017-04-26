#lang racket

(provide (all-defined-out))

(require "board.rkt")
(require "xlsx-import-export.rkt")

(define (get-board-element row column) ;;should probably rename this
  (get-element (- column 1) (get-row (- row 1) board)))
  
(define (move-piece tile1 tile2)
  (cond ((or (equal? tile1 '()) (equal? tile2 '()))
         (displayln "Illegal move: invalid coordinates") 'illegal-move)
        ((and (not (null? (pieces-that-can-capture (tile1 'get-piece))))
              (or (not (member tile1 (pieces-that-can-capture (tile1 'get-piece))))
                  (not (member tile2 (capturable-tiles tile1)))))
         (displayln "You must make an available capture!") 'illegal-move)
        ((equal? (tile1 'get-piece) 'red)
         (cond ((equal? (tile2 'get-piece) 'none)
                (begin
                  ((tile2 'set-piece) 'red (or (tile1 'king?) (= (tile2 'get-row) 1))) ;if red piece reached top row make it a king
                  ((tile1 'set-piece) 'none)
                  'piece-moved))
               ((equal? (tile2 'get-piece) 'black) (capture tile1 tile2))
               (else (displayln "Illegal move: destination occupied") 'illegal-move)))
        ((equal? (tile1 'get-piece) 'black)
         (cond ((equal? (tile2 'get-piece) 'none)
                (begin
                  ((tile2 'set-piece) 'black (or (tile1 'king?) (= (tile2 'get-row) max-row))) ;;if black piece reached bottow row make it a king
                  ((tile1 'set-piece) 'none)
                  'piece-moved))
               ((equal? (tile2 'get-piece) 'red) (capture tile1 tile2))
               (else (displayln "Illegal move: destination occupied") 'illegal-move)))
        (else (displayln "Illegal move: no piece at start") 'illegal-move)))

(define (capture tile1 tile2)
  (define tile3 (jump-destination-tile tile1 tile2))
  (cond ((equal? tile3 '()) (displayln "Illegal move: capture destination out of range") 'illegal-move)
        ((equal? (tile3 'get-piece) 'none)
         (cond ((equal? (tile1 'get-piece) 'red)
                (begin
                  ((tile3 'set-piece) 'red (tile1 'king?))
                  ((tile1 'set-piece) 'none)
                  ((tile2 'set-piece) 'none)
                  (if (and (= (tile3 'get-row) 1) (not (tile3 'king?))) ;if piece reached top row and not already a king, make it a king and stop there
                      (begin ((tile3 'set-king) #t)
                             'capture-done)
                      (continue-capture tile3))))
               ((equal? (tile1 'get-piece) 'black)
                (begin
                  ((tile3 'set-piece) 'black (tile1 'king?))
                  ((tile1 'set-piece) 'none)
                  ((tile2 'set-piece) 'none)
                  (if (and (= (tile3 'get-row) max-row) (not (tile3 'king?))) ;if piece reached bottom row and not already a king, make it a king and stop there
                      (begin ((tile3 'set-king) #t)
                             'capture-done)
                      (continue-capture tile3))))
               (else "no piece at start -- capture")))
        (else "destination is occupied -- capture")))

(define (continue-capture tile)
  (let ((options (capturable-tiles tile)))
    (cond ((null? options) 'capture-done)
          ((= 1 (length options)) (capture tile (car options)))
          (else (capture tile (choose-next-capture options))))))

(define (pieces-that-can-capture color)
  (filter (lambda (tile) (not (null? (capturable-tiles tile))))
          (cond ((equal? color 'red) (get-red-pieces))
                ((equal? color 'black) (get-black-pieces))
                (else '()))))

(define (display-tile-ID tile)
  (if (null? tile)
      (void)
      (begin 
        (display (integer->char (+ (tile 'get-column) 64)))
        (display (tile 'get-row))
        (display " "))))

(define (list-numbers start end)
  (if (> start end)
      '()
      (cons start (list-numbers (+ start 1) end))))

(define (display-enumerated-tile-list lst)
  (map (lambda (tile num) (begin (display "(")
                                 (display num)
                                 (display ")")
                                 (display-tile-ID tile)))
       lst
       (list-numbers 1 (length lst))))

(define (choose-next-capture lst)
  (begin (displayln (draw-board))
         (display "Choose next piece to capture (enter the number): ")
         (display-enumerated-tile-list lst)
         (newline)
         (list-ref lst (- (user-selection 1 (length lst)) 1))))

(define (user-selection min max)
  (let ((input (read)))
    (if (and (integer? input)
             (<= input max)
             (>= input min))
        input
        (begin (displayln "Invalid selection. Please enter a valid number: ")
               (user-selection min max)))))

;; Get the tile that would be the landing spot of a jump made by a piece on
;; tile1 over an opposing piece on tile2. This function may return a null value
;; if the destination would be outside the bounds of the board.
(define (jump-destination-tile tile1 tile2)
  (get-board-element (+ (tile2 'get-row) (- (tile2 'get-row) (tile1 'get-row)))
                     (+ (tile2 'get-column) (- (tile2 'get-column) (tile1 'get-column)))))

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

;; For a given tile, figure out if the piece on it can capture any opponent
;; pieces, and return a list of those tiles that have pieces it can capture.
;; Does not take into account restrictions on the direction a piece can move.
(define (capture-options tile)
  (if (not (or (equal? (tile 'get-piece) 'red)
               (equal? (tile 'get-piece) 'black)))
      ;no capture options for a tile with no piece on it
      '()
      ;else, generate a list of all the adjacent tiles that have opponent pieces
      ; and filter only the ones that have an empty space beyond them
      (filter (lambda (t) (and (not (null? (jump-destination-tile tile t)))
                               (equal? ((jump-destination-tile tile t) 'get-piece) 'none)))
              (find-adjacent-opponents tile (tile 'get-piece)))))

;; Restricts the results of capture-options based on how the piece on the tile
;; is allowed to move.
(define (capturable-tiles tile)
  (cond ((tile 'king?) (capture-options tile))
        ((equal? (tile 'get-piece) 'red)
         (filter (lambda (t) (< (t 'get-row) (tile 'get-row))) (capture-options tile))) ;only tiles in row above
        ((equal? (tile 'get-piece) 'black)
         (filter (lambda (t) (> (t 'get-row) (tile 'get-row))) (capture-options tile))) ;only tiles in row below
        (else '())))

(define (get-black-pieces)
  (define (get-black-pieces-recurse row)
    (if (= row max-row) '()
    (append (filter (lambda (tile) (equal? (tile 'get-piece) 'black)) (get-row row board))
          (get-black-pieces-recurse (add1 row)))))
  (get-black-pieces-recurse 0));generates list of all black tiles

(define (get-red-pieces)
  (define (get-red-pieces-recurse row)
    (if (= row max-row) '()
    (append (filter (lambda (tile) (equal? (tile 'get-piece) 'red)) (get-row row board))
          (get-red-pieces-recurse (add1 row)))))
  (get-red-pieces-recurse 0));generates list of all red tiles

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
