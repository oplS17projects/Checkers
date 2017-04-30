#lang racket
(provide (all-defined-out))
(require "board.rkt")
(require "xlsx-import-export.rkt")
(require "piece-movement.rkt")

;player 1 = black pieces
;player 2 = red pieces

;Use (start) to start the game

;Current commands:
;Current commands:
;(exit) -> exits game
;(move (# #) [direction]) -> moves piece from coordinate in the desired direction)
;(default-board) -> reverts board to default state
;(score) -> displays # of pieces remaining for each player
;(save) -> saves to checkers.xlsx
;(save [filename]) -> saves to a file with the desired name
;(load) -> loads from checkers.xlsx
;(load [filename]) -> loads from specified file

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
    (let ((pieces (pieces-that-can-capture 'black)))
      (if (null? pieces)
          (void)
          (begin
            (display "You have pieces that can make a capture on: ")
            (map display-tile-ID pieces)
            (newline))))
    (display "Please enter a command: ")
    (let ((input (read)))
      (let ((output (process-command input 'black)))
        (cond ((equal? output 'failed) 'p1)
              ((equal? output 'p1) 'p1)
              ((equal? output 'p2) 'p2)
              ((equal? output 'reset) 'p1)
              ((equal? output 'exit) 'exit)
              (else 'p2))))))

(define (player2-turn)
  (begin
    (display-board)
    (newline)
    (declare-turn 2)
    (let ((pieces (pieces-that-can-capture 'red)))
      (if (null? pieces)
          (void)
          (begin
            (display "You have pieces that can make a capture on: ")
            (map display-tile-ID pieces)
            (newline))))
    (display "Please enter a command: ")
    (let ((input (read)))
      (let ((output (process-command input 'red)))
        (cond ((equal? output 'failed) 'p2)
              ((equal? output 'p1) 'p1)
              ((equal? output 'p2) 'p2)
              ((equal? output 'reset) 'p1)
              ((equal? output 'exit) 'exit)
              (else 'p1))))))

;; This is the top-most function. Call this to start the game.
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
        (else turn-result)))

(define (display-board)
  (display (draw-board))
  (newline))

(define (process-command command color)
  (cond ((not (pair? command)) (begin
                                 (display "Commands must be given in list form")
                                 (newline)
                                 'failed))
        ((equal? (tag command) 'exit) 'exit)
        ((equal? (tag command) 'default-board) (begin
                                                (revert-to-default)
                                                'reset))
        ((equal? (tag command) 'load) (begin
                                        (if (and (> (length command) 1) (string? (name command)))
                                            (update-from-file (string-append (name command) ".xlsx"))
                                            (update-from-file))))
        ((equal? (tag command) 'move) (let ((result (move-command (move-parameters  command) color)))
                                        (cond ((equal? result 'illegal-move) 'failed)
                                              ((equal? result 'capture-done) (check-winner))
                                              (else result))))
        ((equal? (tag command) 'score) (begin
                                         (display-score)
                                         'failed))
        ((equal? (tag command) 'save) (begin
                                        (if (and (> (length command) 1) (string? (name command)))
                                            (save-data color (string-append (name command) ".xlsx"))
                                            (save-data color))
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
         (if (not (coordinate? (start-coord command)))
             (begin
               (display "You must enter a valid coordinate")
               (newline)
               'failed)
             (begin
               (display "You do not have a piece on that square!")
               (newline)
               'failed)))        
        (else (if (valid-move-direction? (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                                         (direction command))
                  (move-in-direction (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                                     (direction command))
                  (cond ((coordinate? (direction command))
                         (begin (display "You must enter a direction (e.g (move ([#] [#]) [direction]))")
                                (newline)
                                'failed))
                         ((not(valid-direction? (direction command)))
                          (begin (display "You must enter a valid direction (e.g northeast, southwest, etc.)")
                                 (newline)
                                 'failed))
                         (else (begin (display "You cannot move in that direction")
                                      (newline)
                                      'failed)))))))
  (define (tag lst) (car lst))

(define (move-parameters lst) (cdr lst))

(define (destination lst) (cadr lst))

(define (start-coord lst) (car lst))

(define (end-coord lst) (cadr lst))

(define (x-coord lst)
  (let ((x (car lst)))
    (if (integer? x)
        x
        (- (convert-to-integer x) 64))))

(define (y-coord lst)
  (let ((y (cadr lst)))
    (if (integer? y)
        y
        (- (convert-to-integer y) 64))))

(define (name lst) (cadr lst))

(define (direction lst) (cadr lst))

(define (move-to lst) (cadr lst))

(define (coordinate? lst) (and (pair? lst) (= (length lst) 2))) 

(define (valid-start-coord? coord color)
  (and (coordinate? coord)
       (procedure? (get-square (y-coord coord)(x-coord coord) board))
       (equal? color ((get-square (y-coord coord)(x-coord coord) board) 'get-piece))))

(define (symbol->char sym)
  (char-upcase (string-ref (symbol->string sym) 0)))

(define (convert-to-integer sym)
  (char->integer (symbol->char sym)))

(define (valid-move-direction? tile direction)
  (cond ((tile 'king?) (or (equal? direction 'northeast)
                           (equal? direction 'northwest)
                           (equal? direction 'southeast)
                           (equal? direction 'southwest)))
        ((equal? (tile 'get-piece)  'red)
         (or (equal? direction 'northeast)
             (equal? direction 'northwest)))
        ((equal? (tile 'get-piece) 'black)
         (or (equal? direction 'southeast)
             (equal? direction 'southwest)))))

(define (valid-direction? direction)
  (or
   (equal? direction 'northeast)
   (equal? direction 'northwest)
   (equal? direction 'southeast)
   (equal? direction 'southwest)
   ))

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

(define (check-winner) (cond ((= (get-P1-pieces) 0)
                              (begin (display-board)
                                     (newline)
                                     (displayln "PLAYER 2 IS THE WINNER!!!")
                                     (display "With ")
                                     (let ((remaining (get-P2-pieces)))
                                       (begin
                                         (display remaining)
                                         (if (= remaining 1)
                                             (display " piece left!")
                                             (display " pieces remaining!"))
                                         (newline)))
                                     'exit))
                             ((= (get-P2-pieces) 0)
                              (begin (display-board)
                                     (newline)
                                     (displayln "PLAYER 1 IS THE WINNER!!!")
                                     (display "With ")
                                     (let ((remaining (get-P1-pieces)))
                                       (begin
                                         (display remaining)
                                         (if (= remaining 1)
                                             (display " piece left!")
                                             (display " pieces remaining!"))
                                             (newline)))
                                     'exit))
                             (else 'continue)))
