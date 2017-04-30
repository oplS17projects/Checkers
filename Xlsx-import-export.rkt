#lang racket

(provide (all-defined-out))

(require racket/include)
(require "board.rkt")
(require simple-xlsx)

;; Default names
(define file-name "checkers.xlsx")
(define sheet-name "game")

;; Can be used to change default file name
(define (change-file-name string)
  (set! file-name string))

;; Can be used to change default sheet name
(define (change-sheet-name string)
  (set! file-name string))

;; Updates the board from a given file, or the default file if none is given
(define (update-from-file [file file-name])
  (if (file-exists? file) (update-board-from-grid (get-board-from-xlsx file sheet-name))
      (begin (displayln "Invalid filename!")
             'failed)))
;; Reverts board to default state based on default board
(define (revert-to-default) (update-board-from-grid default-board))

;; Default board state
(define default-board
  '(((none) (black normal) (none) (black normal) (none) (black normal) (none) (black normal))
    ((black normal) (none) (black normal) (none) (black normal) (none) (black normal) (none))
    ((none) (black normal) (none) (black normal) (none) (black normal) (none) (black normal))
    ((none) (none) (none) (none) (none) (none) (none) (none))
    ((none) (none) (none) (none) (none) (none) (none) (none))
    ((red normal) (none) (red normal) (none) (red normal) (none) (red normal) (none))
    ((none) (red normal) (none) (red normal) (none) (red normal) (none) (red normal))
    ((red normal) (none) (red normal) (none) (red normal) (none) (red normal) (none))
    ((p1) () () () () () () ())))


;; Increments a character, used for looping through rows and columns of the xml file.
(define (incrementCharacter character)
  (integer->char (add1 (char->integer character))))


;; Creates a list/cons cell based on the data in the given xml cell
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
                                

;; Converts a character and an integer to a coordinate
(define (CharAndInt->coordinate row column)
  (cons (- (char->integer column) 64)(- (char->integer row) 48)))

;; Gets a given row of the sheet
(define (get-sheet-row row column board-data)
  (cond ((> (- (char->integer column) 64) 8) '())
        (else (cons (get-tile row column board-data) (get-sheet-row row (incrementCharacter column) board-data)))))

;; Gets a row of a grid/list-of-lists
(define (get-row row grid)
  (cond ((eq? '() grid) grid)
        ((= row 0) (car grid))
        (else (get-row (- row 1) (cdr grid)))))

;; Gets a specific element of a grid/list-of-lists
(define (get-element column row-data)
  (cond ((eq? '() row-data) row-data)
        ((= column 0) (car row-data))
        (else (get-element (- column 1) (cdr row-data)))))

(define (get-board-from-xlsx file sheet) ;; gets data from an xml file and outputs it in grid form
  (define tiles '());; used because "with-input-from-xlsx-file" cannot return a value
  (with-input-from-xlsx-file file
    (lambda (board-data)
      (load-sheet sheet board-data)
      (define (loop-through-board row column lst)
        (cond ((> (- ( char->integer row) 48) 9) lst)
              ((equal? lst '()) (loop-through-board (incrementCharacter row) column (list (get-sheet-row row column board-data))))
              (else (loop-through-board (incrementCharacter row) column (append lst (list (get-sheet-row row column board-data)))))))
      (set! tiles (loop-through-board #\1 #\A '()))))
      
  tiles)

;;updates tile based on data contained in the given cons cell/list gathered from the get-board procedure
(define (update-tile square cell)
  (cond ((equal? (car cell) 'red) (begin
                                    ((square 'set-piece) 'red)
                                    (if (equal? (cdr cell) 'king)
                                        ((square 'set-king) #t)
                                        ((square 'set-king) #f))))
        ((equal? (car cell) 'black) (begin
                                      ((square 'set-piece)'black)
                                      (if (equal? (cdr cell) 'king)
                                          ((square 'set-king) #t)
                                          ((square 'set-king) #f))))
        ((equal? (car cell) 'none) (begin
                                     ((square 'set-piece) 'none)
                                     ((square 'set-king) #f)))
        (else (error "invalid data in source -- update-tile"))))

;; Updates the board based on a grid/list-of-lists.
(define (update-board-from-grid grid)
  (define (loop-through-grid row column grid)
    (cond ((= row max-row) (car (get-element 0 (get-row 8 grid))))
          ((= column max-column) (loop-through-grid (+ row 1) 0 grid))
          (else (begin
                  (update-tile (get-element column (get-row row board))
                               (get-element column (get-row row grid)))
                  (loop-through-grid row (+ 1 column) grid)))))
  (loop-through-grid 0 0 grid))                        

;; Outputs a list containing the data of a given column
(define (get-column column grid) 
  (define (nth-element n)
    (define (nth-element-iter count proc)
      (if (= count 0) proc
          (nth-element-iter (- count 1) (lambda (n) (proc (cdr n))))))
    (nth-element-iter n car))
  (map (nth-element column) (filter (lambda (n) (> (length n) column)) grid)))

;; Used to convert board information into a grid of strings for export.
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

;; Saves the state of the board to a file with the desired name or the default name
(define (save-data color [file file-name])
  (let ((save (new xlsx%)))
    (send save add-data-sheet
          #:sheet_name sheet-name
          #:sheet_data (generate-export-grid color))
    (write-xlsx-file save file)))
                        