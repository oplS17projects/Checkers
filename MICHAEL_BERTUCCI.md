# Checkers Game in Racket

## Michael Bertucci

### April 29, 2017

# Overview

This code implements the classic game of Checkers with the standard U.S. rule set. It is playable by two people and uses the Racket REPL as an interface for reading user input and displaying the game board after each player's move.

The function `start` begins execution of the game loop. Turns alternate between player 1 (black pieces) and player 2 (red pieces). The following table shows the list of valid commands a player may input on his/her turn. The only commands that end the player's turn are **move** (assuming it was a valid move, otherwise the player is prompted to try again), **load** (the current turn is determined by the file), and **default-board** (the turn is reset to player 1).

Input | Result
----- | ------
(move (C R) DIRECTION) | Moves the player's piece on column letter C and row number R in the indicated DIRECTION, which must be one of [northwest, northeast, southeast, southwest].
(save "FILENAME") | Saves the current game (state of the board and player whose turn it is) to a .xlsx file specified by FILENAME.
(load "FILENAME") | Loads a game from the file specified by FILENAME.
(default-board)| Starts a new game.
(score) | Displays a count of pieces remaining on each side.
(exit) | Quits the game.

The source code can be divided into four main areas:
1) Handling of player turns and interpreting player commands
2) Piece movement mechanics
3) Representation of board data and drawing images of the board
4) Importing/exporting game data to/from files

Of these four areas, I contributed some of **(1)**, much of **(2)** and the entirety of **(3)**.

# Libraries Used

Two libraries are used in this code:
```
(require simple-xlsx)
(require 2htdp/image) ; this is the one that I personally worked with
```
* `simple-xlsx` is used for reading and writing data to and from .xlsx files. 
  * Note for anyone wanting to download and run this project: the simple-xlsx library does not come installed with Racket and must be installed separately using the raco utility.
* `2htdp/image` is for creating image objects that can be drawn in the REPL and is used to display the board. As drawing the board was part of my division of the work, this was the library that I personally made use of.

# Key Code Excerpts

## 1. Object-Orientation Using Closures

The function `make-square` is the basis for the representation of the game board. The board is made up of a list of the `dispatch` procedure that results from a call to this function, each having been created in a separate environment. As such, each square on the board is represented by one of these environments and each has an independent copy of all of the parameters. Every square "object" is then the closure containing that square's row number, column number, piece type (which can be 'red, 'black, or 'none), and value indicating whether the piece is a king.

```
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
```

Each closure also contains the collection of functions defined within. There are accessor functions for each parameter, as well as setter functions for only the `piece` and `is-king` parameters. The ability to change these two parameters are the only instances of assignment being used in my code, and the state of the game (aside from whose turn it is) at any given time is determined by the values that they are set to. These functions are invoked by passing the corresponding symbol to the `dispatch` function.

To make the code for crowning a piece more elegant, I gave `set-piece` an optional additional parameter that allows the caller to simultaneously set the piece to a king. When the value passed to it is #f it doesn't do anything with it, since during a game once a piece becomes a king it never goes back to being a normal piece. `set-king` can be called separately to set `is-king` back to false for the purpose of loading a new game, etc. Setting the piece on a square to 'none also automatically sets `is-king` to false.

## 2. Recursive Data Structure Construction and Traversal

```
(define (make-board)
  (define (make-row row)
    (define (make-column column)
      (if (> column max-column)
          '()
          (cons (make-square row column 'none #f) (make-column (+ column 1)))))
    (if (> row max-row)
        '()
        (cons (make-column 1) (make-row (+ row 1)))))
  (make-row 1))
  
(define board (make-board))
```

```
(define (get-square row-num column-num board)
  (let ((row (get-list-item (- row-num 1) board)))
    (get-list-item (- column-num 1) row)))

(define (get-list-item index lst)
  (cond ((null? lst) lst)
        ((<= index 0) (car lst))
        (else (get-list-item (- index 1) (cdr lst)))))
```

## 3. Map and Fold Operations on Lists

```
(define (draw-row r) (foldr beside empty-image (map square-to-image r)))

(define (draw-board-without-labels) (overlay (foldr above empty-image (map draw-row board))
                                             (square (+ (* square-size 8) 4) 'solid 'black))) ;For black border around it
                                             
(define (row-labels) (overlay (foldr above empty-image (map draw-char (list "1" "2" "3" "4" "5" "6" "7" "8")))
                              (rectangle (+ square-size 4) (+ (* square-size 8) 4) 'solid 'white))) ;White border

(define (column-labels) (overlay (foldr beside empty-image (map draw-char (list "A" "B" "C" "D" "E" "F" "G" "H")))
                                 (rectangle (+ (* square-size 8) 4) (+ square-size 4) 'solid 'white))) ;White border

;; Putting them all together to make the final board image
(define (draw-board) (above/align 'right
                                   (beside (row-labels) (draw-board-without-labels))
                                   (column-labels)))
```

## 4. Functional Approach to Data Processing

```
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
```

```               
(define (capture-options tile)
  (if (not (or (equal? (tile 'get-piece) 'red)
               (equal? (tile 'get-piece) 'black)))
      '() ;no capture options for a tile with no piece on it
      (filter (lambda (t) (and (not (null? (jump-destination-tile tile t)))
                               (equal? ((jump-destination-tile tile t) 'get-piece) 'none)))
              (find-adjacent-opponents tile (tile 'get-piece)))))
```

```
(define (capturable-tiles tile)
  (cond ((tile 'king?) (capture-options tile))
        ((equal? (tile 'get-piece) 'red)
         (filter (lambda (t) (< (t 'get-row) (tile 'get-row))) (capture-options tile))) ;only tiles in row above
        ((equal? (tile 'get-piece) 'black)
         (filter (lambda (t) (> (t 'get-row) (tile 'get-row))) (capture-options tile))) ;only tiles in row below
        (else '())))
```

```
(define (pieces-that-can-capture color)
  (filter (lambda (tile) (not (null? (capturable-tiles tile))))
          (cond ((equal? color 'red) (get-red-pieces))
                ((equal? color 'black) (get-black-pieces))
                (else '()))))
```

