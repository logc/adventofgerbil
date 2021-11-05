#lang :gerbil/polydactyl

(import :std/iter)
(export count-1d
        count-1d-until)

(define (par->val chr)
  (cond [(eq? chr #\( )  1]
        [(eq? chr #\) ) -1]
        [else 0]))

(define (count-1d chars)
  (for/fold (cnt 0) [(c chars)]
    (+ cnt (par->val c))))

(define (count-1d-until chars pred)
  (let loop ([acc 0] [pos 0] [chrs (string->list chars)])
    (cond [(pred acc) pos]
          [(= 0 (length chrs)) pos]
          [else
           (let ([c (car chrs)])
             (loop (+ acc (par->val c)) (+ 1 pos) (cdr chrs)))])))
