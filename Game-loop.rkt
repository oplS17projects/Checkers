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
        ((equal? (car command) 'exit) 'exit)
        ((equal? (car command) 'default-board) (begin
                                                (revert-to-default)
                                                'reset))
        ((equal? (car command) 'load) (begin
                                        (if (and (> (length command) 1) (string? (name command)))
                                            (update-from-file (string-append (name command) ".xlsx"))
                                            (update-from-file))))
        ((equal? (car command) 'move) (let ((result (move-command (cdr command) color)))
                                        (cond ((equal? result 'illegal-move) 'failed)
                                              ((equal? result 'capture-done) (check-winner))
                                              (else result))))
        ((equal? (car command) 'score) (begin
                                         (display-score)
                                         'failed))
        ((equal? (car command) 'save) (begin
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
         (begin
           (display "You do not have a piece on that square!")
           (newline)
           'failed))         
        ((coordinate? (cadr command))
         (if (valid-dest-coord? (start-coord command) (end-coord command))
             (move-piece (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                         (get-square (y-coord (end-coord command))(x-coord (end-coord command)) board))
             'failed))
        (else (if (valid-move-direction? (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                                         (direction command))
                  (move-in-direction (get-square (y-coord (start-coord command))(x-coord (start-coord command)) board)
                                     (direction command))
                  (begin (display "You cannot move in that direction! ")
                         (newline)
                         'failed)))))

(define (start-coord lst) (car lst))

(define (end-coord lst) (cadr lst))

(define (x-coord lst)
  (let ((x (car lst)))
    (if (integer? x)
        x
        (- (convert-to-integer x) 64))))

(define (y-coord lst) (cadr lst))

(define (name lst) (cadr lst))

(define (direction lst) (cadr lst))

(define (move-to lst) (cadr lst))

(define (coordinate? lst) (pair? lst))

(define (valid-start-coord? coord color)
  (equal? color ((get-square (y-coord coord)(x-coord coord) board) 'get-piece)))

(define (valid-dest-coord? start destination)
  #t);stub

(define (symbol->char sym)
  (string-ref (symbol->string sym) 0))

(define (convert-to-integer sym)
  (char->integer (symbol->char sym)))

(define (valid-move-direction? tile direction)
  (cond ((tile 'king?) (or (equal? direction 'northeast)
                           (equal? direction 'northwest)
                           (equal? direction 'southeast)
<<<<<<< HEAD
                           (equal? direction 'southwest))))
  ((equal? (tile 'get-piece)  'red)
      (or (equal? direction 'northeast)
          (equal? direction 'northwest)))
  ((equal? (tile 'get-piece) 'black)
      (or (equal? direction 'southeast)
          (equal? direction 'southwest))))
=======
                           (equal? direction 'southwest)))
        ((equal? (tile 'get-piece)  'red)
         (or (equal? direction 'northeast)
             (equal? direction 'northwest)))
        ((equal? (tile 'get-piece) 'black)
         (or (equal? direction 'southeast)
             (equal? direction 'southwest)))))
>>>>>>> refs/remotes/origin/PR

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
