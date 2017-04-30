# Checkers in Racket

## Alexander Pilozzi
### April 30, 2017

# Overview
The project implemented via the code is a game of checkers which behaves according to the standard US ruleset (forced capture, king pieces, etc.). The game is designed for two players
and is played entirely within the Racket REPL. Users enter the game loop via ```(start)```, and players can enter a variety of commands to conduct their desired action; as commands are made,
commands are processed and the appropriate procedure required to make the proper board modifications or access other fearless is called. The loop exchanges turns when a valid move is made until
one player is out of pieces, indicating that the other has won.

The project consists of four components: XML data processing, the command loop, movement mechanics and the board itself. I was primarily responsible for the command loop and entirely
responsible for the XML data processing components, though I did establish some of the more basic movement mechanisms.

# Libraries used
The code makes use of two ibaries:

``` racket
(require simple-xlsx) ;; I worked primarily with this libary
(require 2htdp/image)
```

* The ```simple-xlsx``` libary provides the basic procedures required to import and export data from an XML file. One must acquire the package via Dr. Racket for the procedures to function.
* The ```2dhtp/image``` libary provides the procedures required for the graphical representation of the board/pieces/etc. within the REPL

# Key code Excerpts

## 1. The Evaluator
The following code is the mini expression evaluator that calls the appropriate procedure relative to the user's command. The command is made in prefix form, with the first item acting as the
lists "tag". Which tag is present determines which procedure will be called and how the other items in the expression will be dealt with. The following commands are able to be processed:
``` racket
(exit) -> exits game
(move (# #) [direction]) -> moves piece from coordinate in the desired direction)
(default-board) -> reverts board to default state
(score) -> displays # of pieces remaining for each player
(save) -> saves to checkers.xlsx
(save [filename]) -> saves to a file with the desired name
(load) -> loads from checkers.xlsx
(load [filename]) -> loads from specified file
```

The evaluator itself takes both the given command and player making the action, given as their piece color. This is used for validation of certain actions as well as determining
whose turn it was when the game is saved. The evaluator itself is defined as:

``` racket
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
```

Evaluation of movement commands is passed to a different procedure, defined as:

``` racket
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
```
For any procedure accessed by the evaluator (and the evaluator itself) the output is a message that dictates how the outer loop will respond. 
```'failed``` indicates that the player's turn must be repeated, and they will be prompted for another command.
```'reset``` indicates that the game has been reset, and that player 1's turn should be next.
```'p1``` or ```'p2``` indicates that the next turn should be player 1's or player 2's respectively; these messages are passed primarily when loading a game
```'exit``` indicates that either a player has won the game or they have manually exited; this causes the loop to terminate.
Any other message indicates that the turns should be swapped. If a move results in a capture, then that is processed immediately after the move command to determine if a player has won.

## 2. Filtering Tiles

The following procedures make use of filter to produce a list of the tile objects on the board belonging to one player or the other

``` racket
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
```

The filter procedure passes the ```'get-piece``` message to each tile, which is accessed as a dispatch procedure and determines if it is equal to the appropriate color; if so
it is added to the list. Append is used to make the list in forward order (top-left to bottom-right). The function goes through each row in a recursive manner, as each list is built and attached
to the existing list row by row.

## 3. Saving to an XML file

The following procedures allow the game to be saved as an XML file, using the procedures of the ```simple-xlsx``` libary.

``` racket
(define (generate-export-grid color) 
  (define (loop-through row column lst)
    (cond ((= row max-row) (if (equal? color 'black)
                               (append lst (list '("p1" "" "" "" "" "" "" "")))
                               (append lst (list'("p2" "" "" "" "" "" "" "")))))
          ((= column max-column) (cons  lst (loop-through (+ row 1) 0 '())))
          (else (begin
                  (cond ((equal? ((get-square (+ row 1) (+ column 1) board) 'get-piece) 'none)
                         (loop-through row (+ 1 column) (append lst (list "E"))))
                        ((equal? ((get-square (+ row 1) (+ column 1) board) 'get-piece) 'black)
                         (if ((get-square (+ row 1) (+ column 1) board) 'king?)
                             (loop-through row (+ 1 column) (append lst (list "B k")))
                             (loop-through row (+ 1 column) (append lst (list "B n")))))
                        ((equal? ((get-square (+ row 1) (+ column 1) board) 'get-piece) 'red)
                         (if ((get-square (+ row 1) (+ column 1) board) 'king?)
                             (loop-through row (+ 1 column) (append lst (list "R k")))
                             (loop-through row (+ 1 column) (append lst (list "R n"))))))))))
  (loop-through 0 0 '()))
```

The procedure functions by going through the board, and determining which piece is present on the tile using the get-square function in tandem with the appropriate query messages. After
The identity of a piece has been determined, a string that represents that piece is produced. "E" represents an empty tile, "B n" and "B k" represent black normal and king pieces
respectively, and "R n" and "R k" represent red normal and red king pieces respectively. Every time it does this, it increases the column number by one and appends the result to the
existing list. After the column number reaches the maximum value (8), it is reset to 0 and the row is incremented. At this time, A new list is created for the new row.
The procedure is complete when both row number exceeds the maximum by one, and the result is a list containing the list of strings for every row. After going through entire board,
an additional row is appended to the grid, that contains the identity of the player that initiated the save, such that when the game is loaded it will return the turn to them. The extra
empty strings within that list are to allow for export to the XML file, as the ```simple-xlsx``` libary only allows for the export of data where row and column lengths are equal.
This procedure used for export is:

``` racket
(define (save-data color [file file-name])
  (let ((save (new xlsx%)))
    (send save add-data-sheet
          #:sheet_name sheet-name
          #:sheet_data (generate-export-grid color))
    (write-xlsx-file save file)))
```

Which saves the game to checker.xlsx by default, or the specified file-name. The sheet-name is always a default name, which is "game".

## 4. Loading from an XML file
The following procedures allow the game to be loaded from an XML file, using the procedures of the ```simple-xlsx``` library.

``` racket
(define (get-board-from-xlsx file sheet)
  (define tiles '())
  (with-input-from-xlsx-file file
    (lambda (board-data)
      (load-sheet sheet board-data)
      (define (loop-through-board row column lst)
        (cond ((> (- ( char->integer row) 48) 9) lst)
              ((equal? lst '()) (loop-through-board (incrementCharacter row) column (list (get-sheet-row row column board-data))))
              (else (loop-through-board (incrementCharacter row) column (append lst (list (get-sheet-row row column board-data)))))))
      (set! tiles (loop-through-board #\1 #\A '()))))
      
  tiles)

(define (get-sheet-row row column board-data)
  (cond ((> (- (char->integer column) 64) 8) '())
        (else (cons (get-tile row column board-data) (get-sheet-row row (incrementCharacter column) board-data)))))

(define (get-tile row column board-data)
  (define data (get-cell-value (string column row) board-data))
  (cond ((equal? data "E") (list 'none))
        ((equal? data "B n")(cons 'black 'normal))
        ((equal? data "R n") (cons 'red 'normal))
        ((equal? data "B k") (cons 'black 'king))
        ((equal? data "R k") (cons 'red 'king))
        ((and (equal? data "p1") (equal? row #\9)) (list 'p1))
        ((and (equal? data "p2") (equal? row #\9)) (list 'p2))
        ((equal? data "") '())
        (else (error "invalid data -- get-tile"))))
```

The ```get-board-from-xlsx```procedure first generates an empty list called tiles, which is modified within the ```with-input-from-xlsx-file``` procedure. This is done as ```with-input-from-xlsx-file``` does
not allow a value to be returned by the lambda, so an external definition and set! must be used to recover the grid. As accessing cells of the XML file is done through characters, some conversion between
characters an integers had to be performed, though those specific functions are not shown here. The procedure then calls the ```get-sheet-row``` function, which is responsible for building a list
for a single row. The ```get-sheet-row``` function, in turn, calls the ```get-tile``` function, which reads the string contained in a particular cell and outputs a cons cell with the appropriate
information. As each row is built and stored in the master list built by the main ```get-board-from-xlsx``` function, a grid detailing which piece is present at each space is produced.
A function is then responsible for updating the state of the board:

``` racket
(define (update-board-from-grid grid)
  (define (loop-through-grid row column grid)
    (cond ((= row max-row) (car (get-element 0 (get-row 8 grid))))
          ((= column max-column) (loop-through-grid (+ row 1) 0 grid))
          (else (begin
                  (update-tile (get-element column (get-row row board))
                               (get-element column (get-row row grid)))
                  (loop-through-grid row (+ 1 column) grid)))))
  (loop-through-grid 0 0 grid))      
```
The ```update-board-from-grid`` function reads through each element in the grid row by row and updates each tile based on the information contained within the cons cell. In this manner,
the state of the board is restored to that of the XML file. The data in the singleton list that details the turn is the output of the function, as the update-tile procedure itself
operates through state-modification using the existing message-passing system, which is interpreted by the command evaluator as detailed above, allowing the game to resume.
