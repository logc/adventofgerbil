(import :std/iter
        :std/misc/list
        :std/crypto
        :std/text/hex)
(export #t)

(define (sum ns)
  (apply + ns))

(define (par->val chr)
  (cond ((eq? chr #\( )  1)
        ((eq? chr #\) ) -1)
        (else 0)))

(define (count-1d chars)
  (for/fold (cnt 0) ((c chars))
    (+ cnt (par->val c))))

(define (count-1d-until chars pred)
  (let loop ((acc 0) (pos 0) (chrs (string->list chars)))
    (cond ((pred acc) pos)
          ((= 0 (length chrs)) pos)
          (else
           (let ((c (car chrs)))
             (loop (+ acc (par->val c)) (+ 1 pos) (cdr chrs)))))))

(define (parse-present-dimensions line)
  (map string->number (string-split line #\x)))

(define (wrap-paper dimensions)
  (let ((l (list-ref dimensions 0))
        (w (list-ref dimensions 1))
        (h (list-ref dimensions 2)))
    (let ((a (* l w))
          (b (* w h))
          (c (* h l)))
      (+ (* 2 (+ a b c))
         (min a b c)))))

(define (wrap-ribbon dimensions)
  (let ((l (list-ref dimensions 0))
        (w (list-ref dimensions 1))
        (h (list-ref dimensions 2)))
    (let ((a (+ l l w w))
          (b (+ w w h h))
          (c (+ h h l l))
          (d (* l w h)))
      (+ (min a b c) d))))

;; NOTE: needs to be transparent, otherwise `unique` does not work!
(defstruct pos (x y)
  transparent: t)

(define (deliver-presents moves-contents)
  (let ((start (make-pos 0 0))
        (moves (string->list moves-contents)))
    (let ((past-positions (list start)))
      (let ((all-positions (move start moves past-positions)))
        (length (unique all-positions))))))

(define (move a-pos moves past-moves)
  (cond ((null? moves) past-moves)
        (else (let ((b-pos (update-pos a-pos (car moves))))
                (move b-pos (cdr moves) (cons b-pos past-moves))))))

(define (update-pos a-pos move)
  (cond ((eq? move #\>) (make-pos (1+ (pos-x a-pos)) (pos-y a-pos)))
        ((eq? move #\<) (make-pos (1- (pos-x a-pos)) (pos-y a-pos)))
        ((eq? move #\^) (make-pos (pos-x a-pos) (1+ (pos-y a-pos))))
        ((eq? move #\v) (make-pos (pos-x a-pos) (1- (pos-y a-pos))))
        (else a-pos)))

(define (deliver-presents-with-robo moves-contents)
  (let ((divided (divide (string->list moves-contents))))
    (let ((start (make-pos 0 0))
          (santa-moves (car divided))
          (robo-moves (cadr divided)))
      (let ((past-positions (list start)))
        (let ((all-santa-positions (move start santa-moves past-positions))
              (all-robo-positions (move start robo-moves past-positions)))
          (length (unique (append all-santa-positions all-robo-positions))))))))

(define (enumerate lst)
  (lambda ()
    (let loop ((idx 0))
      (when (< idx (length lst))
        (yield (cons idx (list-ref lst idx)))
        (loop (1+ idx))))))

(defrules append! ()
  ((_ lst val) (set! lst (append1 lst val))))

(define (divide lst)
  (define evens '())
  (define odds  '())
  (for ([idx . val] (enumerate lst))
    (cond ((even? idx) (append! evens val))
          ((odd? idx)  (append! odds val))))
  (list evens odds))

(define (mine-adventcoins key)
  (let loop ((num 0))
    (if (has-five-leading-zeroes? (md5-hex key num))
      num
      (loop (1+ num)))))

(define (mine-more-adventcoins key)
  (let loop ((num 0))
    (if (has-six-leading-zeroes? (md5-hex key num))
      num
      (loop (1+ num)))))

(define (md5-hex key num)
  (hex-encode (md5 (string-append key (number->string num)))))

(define (has-five-leading-zeroes? a-string)
  (has-many-leading-zeroes? a-string 5))

(define (has-six-leading-zeroes? a-string)
  (has-many-leading-zeroes? a-string 6))

(define (has-many-leading-zeroes? a-string how-many)
  (let ((first-five-chars (slice (string->list a-string) 0 how-many)))
    (andmap (lambda (x) (eqv? x #\0)) first-five-chars)))
