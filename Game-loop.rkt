#lang racket
(provide (all-defined-out))
(require "board.rkt")
(require "xlsx-import-export.rkt")
(require "piece-movement.rkt")

;player 1 = black pieces
;player 2 = red pieces

;Current commands:
;(exit) -> exits game
;(move (# #) (# #)) -> moves piece from first coordinate to second coordinate.
;currently allows for non-adjacent movement
;(move (# #) [direction]) -> moves piece from coordinate in the desired direction)
;(default-board) -> reverts board to default state
;(score) -> displays # of pieces remaining for each player

(define (declare-turn player)
  (if (= player 1) (begin
                     (display "It is player 1's turn!")
                     (newline))
      (begin
        (display "It is player 2's turn!")
        (newline))))

(define (player1-turn)
  (begin
    (display-board)
    (newline)
    (declare-turn 1)
    (display "Please enter a command: ")
    (let ((input (read)))
      (let ((output (process-command input 'black)))
        (if (equal? output 'failed) (player1-turn)
          output)))))

(define (player2-turn)
  (begin
    (display-board)
    (newline)
    (declare-turn 2)
    (display "Please enter a command: ")
    (let ((input (read)))
      (let ((output (process-command input 'red)))
        (if (equal? output 'failed) (player2-turn)
          output)))))
    
;(define (game-loop)
;  (if (equal? (player1-turn) 'exit)
;      (display "exiting game")
;      (if (equal? (player2-turn) 'exit)
;          (display "exiting game")
;          (game-loop))))

(define (start) (begin (revert-to-default)
                       (game-loop 'p1)))

(define (game-loop turn)
  (if (equal? turn 'exit)
      (display "exiting game")
      (game-loop (figure-out-next-turn turn (if (equal? turn 'p1)
                                                 (player1-turn)
                                                 (player2-turn))))))

(define (figure-out-next-turn this-turn turn-result)
  (cond ((equal? turn-result 'exit) turn-result)
        ((equal? turn-result 'reset) 'p1)
        (else (if (equal? this-turn 'p1)
                  'p2
                  'p1))))

(define (display-board)
  (display (draw-board))
  (newline))

(define (process-command command color)
  (cond ((not (pair? command)) (begin
                                 (display "Commands must be given in list form")
                                 (newline)
                                 'failed))
        ((equal? (car command) 'exit) 'exit)
        ((equal? (car command) 'default-board) (begin
                                                (revert-to-default)
                                                'reset))
        ((equal? (car command) 'load) (begin
                                        (update-from-file)
                                        'reset)) ;temporary, resets turn to p1
        ((equal? (car command) 'move) (let ((result (move-command (cdr command) color)))
                                        (if (equal? result 'illegal-move)
                                            'failed
                                            result)))
        ((equal? (car command) 'score) (begin
                                         (display-score)
                                         'failed))
        (else (begin
                (displayln "invalid command")
                'failed))))
       

(define (move-command command color)
  (cond ((or (equal? '() command) (< (length command) 2))
         (begin
           (display "You must enter starting co-ordinates and a direction/destination co-ordinates")
           (newline)
           'failed))
        ((not (valid-start-coord? (start-coord command) color))
         (begin
           (display "You do not have a piece on that square!")
           (newline)
           'failed))         
        ((coordinate? (cadr command))
         (if (valid-dest-coord? (start-coord command) (end-coord command))
             (move-piece (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                         (get-square (y-coord (end-coord command))(x-coord (end-coord command)) board))
             'failed))
        (else (if (valid-move-direction? color (direction command))
                  (move-in-direction (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                                     (direction command))
                  (begin (display "You cannot move in that direction! ")
                         (newline)
                         'failed)))))

(define (start-coord lst) (car lst))

(define (end-coord lst) (cadr lst))

(define (x-coord lst) (car lst))

(define (y-coord lst) (cadr lst))

(define (direction lst) (cadr lst))

(define (move-to lst) (cadr lst))

(define (coordinate? lst) (pair? lst))

(define (valid-start-coord? coord color)
  (equal? color ((get-square (y-coord coord)(x-coord coord) board) 'get-piece)))

(define (valid-dest-coord? start destination)
  #t);stub

(define (valid-move-direction? color direction)
  (if (equal? color 'red)
      (or (equal? direction 'northeast)
          (equal? direction 'northwest))
      (or (equal? direction 'southeast)
          (equal? direction 'southwest))))

(define (valid-direction? direction)
  (or
   ;(equal? direction 'north)
   ;(equal? direction 'south)
   ;(equal? direction 'east)
   ;(equal? direction 'west)
   (equal? direction 'northeast)
   (equal? direction 'northwest)
   (equal? direction 'southeast)
   (equal? direction 'southwest)
   ))

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

(define (get-P1-pieces)
  (length (get-black-pieces)))

(define (get-P2-pieces)
  (length (get-red-pieces)))

(define (display-score)
  (begin
  (display "player 1 pieces remaining: ")
  (display (get-P1-pieces))
  (newline)
  (display "player 2 pieces remaining: ")
  (display (get-P2-pieces))
  (newline)))
   
