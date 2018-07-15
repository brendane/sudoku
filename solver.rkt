#!/usr/bin/env racket
#lang racket

;; Sudoku solver in racket. Might make sense to use a plain vector
;; instead of a hash-table (lists are immutable).
;;
;; Also, there may be further opportunities to eliminate unnecessary
;; computations.
;;
;; Note that the sets used here are immutable. The hash-copy function seems
;; to make a shallow copy, but that doesn't matter if the sets inside of
;; it are immutable.

(define *initial-cell* (list 1 2 3 4 5 6 7 8 9))


;; integer division
(define (idiv x y) (floor (/ x y)))

(define (read-board input-port)
  (let ([board (make-hash)])
    (for ([(line r) (in-indexed (in-lines input-port))])
      (let ([cells (string-split line "\t" #:trim? #f)])
        (for ([(cell c) (in-indexed cells)])
          (hash-set! board (list r c)
                     (if (equal? cell "")
                         (list->set *initial-cell*)
                         (set (string->number cell)))))))
    board))


(define (print-board board)
  (for ([r (in-range 9)])
    (display (if (= 0 (remainder r 3))
                 "||=====================================||\n" ""))
    (for ([c (in-range 9)])
      (display (if (= 0 (remainder c 3)) "|" ""))
      (let ([cell (hash-ref board (list r c))])
        (if (= (set-count cell) 1)
            (printf "| ~s " (set-first cell))
            (display "| ? "))))
    (display "||\n"))
  (display "||=====================================||\n"))


(define (fixed? cell) (= 1 (set-count cell)))


(define (fixed-or-error? cell) (>= 1 (set-count cell)))


(define (filter-fixed cells) (filter fixed? cells))


(define (solved-board? board)
  (andmap fixed? (hash-values board)))


(define (n-solved-board board)
  (foldl + 0 (map (lambda (x) (if (fixed? x) 1 0)) (hash-values board))))


(define (row board r c)
  (for/list ([(k v) (in-hash board)]
             #:when (and (= r (first k))
                         (not (= c (second k)))))
    v))


(define (col board r c)
  (for/list ([(k v) (in-hash board)]
             #:when (and (= c (second k))
                         (not (= r (first k)))))
    v))


(define (sq board r c)
  (let ([sr (idiv r 3)]
        [sc (idiv c 3)])
    (for/list ([(k v) (in-hash board)]
               #:when (and (= sr (idiv (first k) 3))
                           (= sc (idiv (second k) 3))
                           (or
                            (not (= r (first k)))
                            (not (= c (second k))))))
      v)))


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
  (let* ([start (hash-ref board (list r c))]
         [others (list (row board r c) (col board r c) (sq board r c))]
         [cant-be (flatten-set-union
                   (map filter-fixed others))]
         [can-be (set-subtract start cant-be)])
    (if (fixed-or-error? can-be)
        can-be
        (get-uniq can-be others))))


(define (elim-solve! board)
  (for ([r (in-range 9)])
    (for ([c (in-range 9)]
          #:unless (fixed? (hash-ref board (list r c))))
      (let ([newc (eliminate board r c)])
        (if (= 0 (set-count newc))
            (void)
            (hash-set! board (list r c) newc))))))


(define (do-elim-solve! board [limit 1000] [n 1] [prev-n-solved 0])
  (elim-solve! board)
  (let ([n-solved (n-solved-board board)])
    (if (or (= 81 n-solved) (> n limit) (= n-solved prev-n-solved))
      board
      (do-elim-solve! board limit (+ n 1) n-solved))))


(define (pick-cell-to-guess board prev)
  (for*/last ([n (in-range 2 9)]
              [r (in-range 9)]
              [c (in-range 9)]
              #:final (and (not (set-member? prev (list r c)))
                           (= n (set-count (hash-ref board (list r c))))
                           (not (fixed? (hash-ref board (list r c))))))
    (list r c)))


(define (guess-solve board [prev (set)])
  (if (solved-board? board)
      board
      (let* ([rc (pick-cell-to-guess board prev)]
             [cell (hash-ref board rc)])
        (for/last ([i (in-set cell)])
          (let ([b (hash-copy board)])
            (hash-set! b rc (set i))
            (do-elim-solve! b)
            (if (solved-board? b)
                b
                (guess-solve b (set-add prev rc))))))))


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
