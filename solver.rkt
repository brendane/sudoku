#!/usr/bin/env racket
#lang racket

;; Sudoku solver in racket.
;;
;; Note that the sets used here are immutable,:which is good, because
;; racket does not seem to have a deep copy function.

(define *initial-cell* (set 1 2 3 4 5 6 7 8 9))


;; integer division
(define (idiv x y) (floor (/ x y)))

(define (read-board input-port)
  (let ([board (make-vector 81 *initial-cell*)])
    (for ([(line r) (in-indexed (in-lines input-port))])
      (let ([cells (string-split line "\t" #:trim? #f)])
        (for ([(cell c) (in-indexed cells)])
             (if (equal? cell "")
               (void)
               (vector-set! board (+ (* r 9) c) (set (string->number cell)))))))
    board))


(define (print-board board)
  (for ([r (in-range 9)])
    (display (if (= 0 (remainder r 3))
                 "||=====================================||\n" ""))
    (for ([c (in-range 9)])
      (display (if (= 0 (remainder c 3)) "|" ""))
      (let ([cell (vector-ref board (+ c (* 9 r)))])
        (if (= (set-count cell) 1)
            (printf "| ~s " (set-first cell))
            (display "| ? "))))
    (display "||\n"))
  (display "||=====================================||\n"))


(define (fixed? cell) (= 1 (set-count cell)))


(define (error? cell) (= 0 (set-count cell)))


(define (filter-fixed cells) (filter fixed? cells))


(define (solved-board? board) (andmap fixed? (vector->list board)))


(define (n-solved-board board)
  (foldl + 0 (map (lambda (x) (if (fixed? x) 1 0)) (vector->list board))))


(define (row board r c)
  (let ([i (* 9 r)])
    (for/list ([ci (in-range 9)]
               #:when (not (= ci c)))
              (vector-ref board (+ ci i)))))


(define (col board r c)
  (for/list ([ri (in-range 9)]
             #:when (not (= ri r)))
            (vector-ref board (+ c (* 9 ri)))))


;; Change to vector
(define (sq board r c)
  (let ([sr (* 3 (idiv r 3))]
        [sc (* 3 (idiv c 3))])
    (for*/list ([i (in-range 3)]
                [j (in-range 3)]
                #:when (or (not (= r (+ sr i)))
                           (not (= c (+ sc j)))))
               (vector-ref board (+ sc j (* 9 (+ sr i)))))))


;; For a list of sets (or list of list of sets, etc), take
;; the set-union.
(define (fold-union ls) (foldl set-union (set) ls))
(define (flatten-set-union ls)
  (cond [(empty? ls) (set)]
        [(set? (first ls)) (fold-union ls)]
        [else (flatten-set-union (map flatten-set-union ls))]))


;; Is this cell the only one in the row, column or square that has
;; a particular value? If not, just return the initial value
(define (get-uniq cell others)
  (let* ([uniqs (map (lambda (ls) (set-subtract cell (flatten-set-union ls))) others)]
         [uniq (filter-fixed uniqs)])
    (if (empty? uniq)
        cell
        ; Only need the first entry, if there are multiple, and need a set, not a list
        (first uniq))))


;; 1) Get the values of the fixed cells in the row, column, and square,
;;    and remove those values from the target cell.
;; 2) Figure out if the target cell has a value that is unique in the
;;    row, column, or square. 
(define (eliminate board r c)
  (let* ([start (vector-ref board (+ c (* 9 r)))]
         [others (list (row board r c) (col board r c) (sq board r c))]
         [cant-be (flatten-set-union
                    (map filter-fixed others))]
         [can-be (set-subtract start cant-be)])
    (if (error? can-be)
      (error "no solution")
      (if (fixed? can-be)
        can-be
        (get-uniq can-be others)))))


(define (elim-solve! board)
  (for ([r (in-range 9)])
    (for ([c (in-range 9)]
          #:unless (fixed? (vector-ref board (+ c (* 9 r)))))
         (vector-set! board (+ c (* 9 r)) (eliminate board r c)))))


(define (do-elim-solve! board [limit 1000] [n 1] [prev-n-solved 0])
  (elim-solve! board)
  (let ([n-solved (n-solved-board board)])
    (if (or (= 81 n-solved) (> n limit) (= n-solved prev-n-solved))
      board
      (do-elim-solve! board limit (+ n 1) n-solved))))


(define (pick-cell-to-guess board prev)
  (for*/last ([n (in-range 2 10)]
              [r (in-range 9)]
              [c (in-range 9)]
              #:final (and (not (set-member? prev (list r c)))
                           (= n (set-count (vector-ref board (+ c (* 9 r)))))
                           (not (fixed? (vector-ref board (+ c (* 9 r)))))))
    (+ c (* 9 r))))

;; This is a convoluted approach to dealing with errors, but it's the only
;; thing I could come up with.  I'm sure if I understood continuations, this
;; would be a place to use them
(define (inner-guess board rc choices prev)
  (let ([choice (set (set-first choices))]
        [b (vector-copy board)])
    (vector-set! b rc choice)
    (let 
      ;; First, take a guess, and see if it is wrong. If it is wrong,
      ;; there will be an error, and we try a different value.
      ([bb (with-handlers*
                ([exn:fail? (lambda (e) (inner-guess board
                                                     rc
                                                     (set-subtract choices choice)
                                                     prev))])
                (do-elim-solve! b))])
      (if (solved-board? bb)
        ;; The board was solved by the above guess, return it.
        bb
        ;; Not solved by guessing at this cell, but also not known to be
        ;; wrong. Try a guess at another cell using guess-solve. Then, if
        ;; that raises an error, change the guess at this cell and try again.
        (with-handlers* ([exn:fail? (lambda (e) (inner-guess board
                                                             rc
                                                             (set-subtract choices choice)
                                                             prev))])
                        (guess-solve bb (set-add prev rc)))))))


;; Pick a cell to guess at, then run inner-guess
(define (guess-solve board [prev (set)])
  (if (solved-board? board)
    board
    (let* ([rc (pick-cell-to-guess board prev)]
           [cell (vector-ref board rc)]
           [b (vector-copy board)])
      (inner-guess b rc cell (set-add prev rc)))))


;; Loop over files, solve, and print
(define (main infiles)
  (for ([fname infiles])
       (let ([board (call-with-input-file fname
                                          (lambda (input) (read-board input)))])
         (printf "\nSTART ~s\n" fname)
         (print-board board)
         (do-elim-solve! board 100)
         (define final (guess-solve board))
         (printf "\nEND ~s\n" fname)
         (printf "Solved?: ~a\n" (solved-board? final))
         (print-board final))))

(main (current-command-line-arguments))
