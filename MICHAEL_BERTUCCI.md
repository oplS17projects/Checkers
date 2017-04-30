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

Described in each of the four sections below are fundamental procedures or groups of procedures that embody core concepts of the OPL course.

All code listed below is code that I wrote myself.

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

Each closure also contains the collection of functions defined within. There are accessor functions for each parameter, as well as setter functions for only the `piece` and `is-king` parameters. The ability to change these two parameters are the only instances of assignment being used in my code, and the state of the game (aside from whose turn it is) at any given time is determined by the values that they are set to. These functions are invoked by passing the corresponding symbol (a.k.a. the "message") to the `dispatch` function.

To make the code for crowning a piece more elegant, I gave `set-piece` an optional additional parameter that allows the caller to simultaneously set the piece to a king. When the value passed to it is #f it doesn't do anything with it, since during a game once a piece becomes a king it never goes back to being a normal piece. `set-king` can be called separately to set `is-king` back to false for the purpose of loading a new game, etc. Setting the piece on a square to 'none also automatically sets `is-king` to false.

## 2. Recursive Data Structure Construction and Traversal

Some solid examples of using recursion are the construction of the structure representing the board, as well as the function `get-square` for accessing a specific square on the board.

As mentioned earlier, the board is a list of square objects. In fact, it is a list of lists. The outer list is a list of rows, and each inner list is a list of square objects that are the squares in that row of the board. The construction of the board is done in `make-board`, which internally defines the helper function `make-row` and calls it with a value of 1. `make-row` creates the function `make-column` which will have access to the `row` value of the `make-row` that created it. Then, it `cons`es together `(make-column 1)` with a call to itself with an incremented row number. This recursion ends when the row number exceeds `max-row`, which is defined to be 8.

`make-column` has a similar structure except it `cons`es together calls to `make-square`, initializing the objects with the current values of `row` and `column` and no pieces, with a call to itself after incrementing the column number. The result is a list of `dispatch` procedures of the individual squares. This function also caps off the list when the column number exceeds 8. 

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

At the top level the expansion of `make-board` is a chain of 8 nested `cons` operations on `(make-column 1)`. The key, however, is that each `make-column` procedure was created in a different call to `make-row` with a different argument for `row`, so each `make-column` will see a different value when it references that name.

In the end, the resulting 8x8 list is bound to the name `board`, so all 64 of the square objects that were created persist throughout the lifetime of the program. `board` is treated like a global object in the code. The `dispatch` function of any of the objects can be accessed and used by simply acquiring it from `board`. I wrote a generic list traversal function `get-list-item` that uses tail recursion to walk through the list and return the element at the specified index. I then used this to write the function `get-square` which takes the board and a row and column number and first gets the row of the board, then finds within that list the specific `dispatch` in the row.

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

The following functions are all involved in actually building up the displayable image of the board. What is most notable about them is that they all involve using `map` to create a list out of another list, and then using `foldr` on the result to reduce it to a single value. In this case, the value is an image type defined by the 2htdp/image library.

I began by writing `square-to-image`, a function that takes a single square object and creates the graphic for it, including the piece on it if there is one. I have not copied it here for the sake of avoiding clutter, as the point is to show how it is used in mapping.
Given a row `r`, using `map` to apply `square-to-image` to each square in the row generates a list of the correct images for those squares. 2htdp/image provides the function `beside` for placing two images next to each other horizontally to create a single image, so a natural way to create an image of the whole row was to use `foldr` to combine all the images in the mapped list using `beside`.

Since the board is a list of rows, drawing the board requires maping `draw-row` to `board` and then using `foldr` in a similar way as before except with the `above` function, which is the vertical analogue to `beside`. I found this to be a simple but elegant solution to the problem of drawing an image of the game board. Discovering this solution and how well it works also gave me new appreciation for the usefulness of `map` and `foldr`.

```
(define (draw-row r) (foldr beside empty-image (map square-to-image r)))

(define (draw-board-without-labels) (overlay (foldr above empty-image (map draw-row board))
                                             (square (+ (* square-size 8) 4) 'solid 'black))) ;For black border
```

The two functions below use a similar strategy for creating the graphics that label the row numbers along the left-hand side of the board and the column letters across the bottom of the board, using the lists 1-8 and A-H respectively. `draw-char` is mapped to each list, and then the images are combined according to their required orientation.

```
(define (row-labels) (overlay (foldr above empty-image (map draw-char (list "1" "2" "3" "4" "5" "6" "7" "8")))
                              (rectangle (+ square-size 4) (+ (* square-size 8) 4) 'solid 'white))) ;White border

(define (column-labels) (overlay (foldr beside empty-image (map draw-char (list "A" "B" "C" "D" "E" "F" "G" "H")))
                                 (rectangle (+ (* square-size 8) 4) (+ square-size 4) 'solid 'white)))

;; Putting them all together to make the final board image
(define (draw-board) (above/align 'right
                                   (beside (row-labels) (draw-board-without-labels))
                                   (column-labels)))
```

## 4. Functional Approach to Data Processing

This excerpt of the code highlights a sequence of functions that process a list of data using a functional approach, the result of each step being passed up to the next function. The goal of this sequence is to generate a list of a player's pieces that can legally capture another piece in the current state of the board. With the discouragement of declaring and assigning variables, as one would in an imperative approach, I was forced to break this problem down into smaller pieces, figure out where to start, and then build it back up to obtain the final result. I will highlight the functions in the order that they build up, ending with the `pieces-that-can-capture` function which returns the final list.

As a side note, these functions also demonstrate significant use of the `filter` operation on lists.

`find-adjacent-opponents` considers a single square (or tile, as it's referred to in the code) and begins by building a list of the tiles diagonally adjacent to it. Then, it filters that list for only the tiles that have a piece of the opposing color. An additional constraint in the filter is that the list element must not be null, since a piece on the edge of the board could have diagonals that are out of bounds and those would be returned as `'()` when constructing the original list.

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

Now that `find-adjacent-opponents` has provided the list of tiles adjacent to our tile of interest that have opponent pieces on them, the next step carried out by `capture-options` is to take that result and perform another filter. For each tile `t` in the list, the filter predicate takes `t` and the tile of interest `tile`, identifies the third tile that the piece on `tile` would land on if it captured the piece on `t`, and returns true only if that third tile has no piece on it. Otherwise, `t` is obviously not an option for capture because the jump destination is occupied.

```               
(define (capture-options tile)
  (if (not (or (equal? (tile 'get-piece) 'red)
               (equal? (tile 'get-piece) 'black)))
      '() ;no capture options for a tile with no piece on it
      (filter (lambda (t) (and (not (null? (jump-destination-tile tile t)))
                               (equal? ((jump-destination-tile tile t) 'get-piece) 'none)))
              (find-adjacent-opponents tile (tile 'get-piece)))))
```

The last step in figuring out the available captures for a single tile is to consider that with the exception of king pieces, red pieces can only move up the board and black pieces can only move down the board. With that in mind, `capturable-tiles` filters the results of `capture-options` based on whether the tile of interest has a red piece (filter out only the options in the row above it) or a black piece (filter out only the options in the row below it).

```
(define (capturable-tiles tile)
  (cond ((tile 'king?) (capture-options tile))
        ((equal? (tile 'get-piece) 'red)
         (filter (lambda (t) (< (t 'get-row) (tile 'get-row))) (capture-options tile))) ;only tiles in row above
        ((equal? (tile 'get-piece) 'black)
         (filter (lambda (t) (> (t 'get-row) (tile 'get-row))) (capture-options tile))) ;only tiles in row below
        (else '())))
```

Now that there is a process for computing the list of tiles that have pieces that can be captured by the piece on a given tile, `pieces-that-can-capture` takes that process and applies it to every tile that has a piece of the player's color on it (the lists generated by either `get-red-pieces` or `get-black-pieces`. A tile for which `capturable-tiles` returns an empty list gets filtered out, while a tile that generates a non-empty list gets filtered in. The result is a list of all of the squares of the board that have pieces of the specified color that are in a position to legally capture a piece of the opposite color.

`pieces-that-can-capture` is called at the beginning of each player's turn with that player's piece color to determine if they have any captures available and if so display the list of them to the player. If the player has pieces that can capture but they try to move one that is not in this list, the move is unsuccessful and the game forces that player to try their turn again.

```
(define (pieces-that-can-capture color)
  (filter (lambda (tile) (not (null? (capturable-tiles tile))))
          (cond ((equal? color 'red) (get-red-pieces))
                ((equal? color 'black) (get-black-pieces))
                (else '()))))
```

