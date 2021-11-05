#lang :gerbil/polydactyl

(import :std/iter)
(export #t)

(define (sum ns)
  (apply + ns))

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

(define (parse-present-dimensions line)
  (map string->number (string-split line #\x)))

(define (wrap-paper dimensions)
  (let ([l (list-ref dimensions 0)]
        [w (list-ref dimensions 1)]
        [h (list-ref dimensions 2)])
    (let ([a (* l w)]
          [b (* w h)]
          [c (* h l)])
      (+ (* 2 (+ a b c))
         (min a b c)))))

(define (wrap-ribbon dimensions)
  (let ([l (list-ref dimensions 0)]
        [w (list-ref dimensions 1)]
        [h (list-ref dimensions 2)])
    (let ([a (+ l l w w)]
          [b (+ w w h h)]
          [c (+ h h l l)]
          [d (* l w h)])
      (+ (min a b c) d))))
