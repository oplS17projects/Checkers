#lang racket
(require racket/include)
(require "board.rkt")

(require simple-xlsx)
(define file-name "checkers.xlsx")
(define sheet-name "game")

(define (change-file-name string)
  (set! file-name string))

(define (change-sheet-name string)
  (set! file-name string))

(define (update-from-file) (update-board-from-grid (get-board-from-xlsx file-name sheet-name)))

(define (revert-to-default) (update-board-from-grid default-board))

(define default-board
  '(((black) (none) (black) (none) (black) (none) (black) (none))
  ((none) (black) (none) (black) (none) (black) (none) (black))
  ((black) (none) (black) (none) (black) (none) (black) (none))
  ((none) (none) (none) (none) (none) (none) (none) (none))
  ((none) (none) (none) (none) (none) (none) (none) (none))
  ((none) (red) (none) (red) (none) (red) (none) (red))
  ((red) (none) (red) (none) (red) (none) (red) (none))
  ((none) (red) (none) (red) (none) (red) (none) (red))))


(define (incrementCharacter character)
  (integer->char (add1 (char->integer character))))

(define (get-tile row column board-data)
  (define data (get-cell-value (string column row) board-data))
  (cond ((equal? data "E") (list 'none))
        ((equal? data "B n")(list 'black))
        ((equal? data "R n") (list 'red))
        (else (error "invalid data -- get-tile"))))
                                

(define (CharAndInt->coordinate row column)
  (cons (- (char->integer column) 64)(- (char->integer row) 48)))

(define (get-sheet-row row column board-data)
  (cond ((> (- (char->integer column) 64) 8) '())
        (else (cons (get-tile row column board-data) (get-sheet-row row (incrementCharacter column) board-data)))))


(define (get-row row grid)
  (cond ((eq? '() grid) grid)
        ((= row 0) (car grid))
        (else (get-row (- row 1) (cdr grid)))))

(define (get-element column row-data)
  (cond ((eq? '() row-data) row-data)
        ((= column 0) (car row-data))
        (else (get-element (- column 1) (cdr row-data)))))

(define (get-board-from-xlsx file sheet) ;; gets data in grid form
  (define tiles '())
  (with-input-from-xlsx-file file
    (lambda (board-data)
      (load-sheet sheet board-data)
      (define (loop-through-board row column lst)
        (cond ((> (- ( char->integer row) 48) 8) lst)
              ((equal? lst '()) (loop-through-board (incrementCharacter row) column (list (get-sheet-row row column board-data))))
              (else (loop-through-board (incrementCharacter row) column (append lst (list (get-sheet-row row column board-data)))))))
              (set! tiles (loop-through-board #\1 #\A '()))))
  tiles)

(define (update-tile square cell)
  (cond ((equal? (car cell) 'red) ((square 'set-piece) 'red))
        ((equal? (car cell) 'black) ((square 'set-piece)'black))
        ((equal? (car cell) 'none) ((square 'set-piece) 'none))
        (else (error "invalid data in source -- update-tile"))))


(define (update-board-from-grid grid)
  (define (loop-through-grid row column grid)
    (cond ((= row max-row) "data_updated")
          ((= column max-column) (loop-through-grid (+ row 1) 0 grid))
          (else (begin
                  (update-tile (get-element column (get-row row board))
                               (get-element column (get-row row grid)))
                  (loop-through-grid row (+ 1 column) grid)))))
  (loop-through-grid 0 0 grid))                        

(define (get-column column grid)
  (define (nth-element n)
    (define (nth-element-iter count proc)
     (if (= count 0) proc
         (nth-element-iter (- count 1) (lambda (n) (proc (cdr n))))))
    (nth-element-iter n car))
  (map (nth-element column) (filter (lambda (n) (> (length n) column)) grid)))



;(define (get-board-from-xlsx file sheet) ;; gets data in list form.
;  (with-input-from-xlsx-file file
;    (lambda (board-data)
;      (load-sheet sheet board-data)
;      (define (loop-through-board row column)
;        (cond ((> (- ( char->integer row) 48) 8) '())
;              ((> (- (char->integer column) 64) 8) (loop-through-board (incrementCharacter row) #\A))
;              (else (cons (get-tile row column board-data)
;                          (loop-through-board row (incrementCharacter column))))))
;              (set! tiles (loop-through-board #\1 #\A)))))
