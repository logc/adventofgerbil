#!/usr/bin/env gxi-script

(import
  :std/getopt
  :std/format
  :std/misc/ports)

(import "advent")

(define (parse-args args)
  (def year-opt (option 'year "-y" "--year" help:"which year of solutions"))
  (def day-opt  (option 'day  "-d" "--day"  help:"which day to solve"))
  (def parser (getopt year-opt day-opt))
  (getopt-parse parser args))

(define (solve year day)
  (match [day year]
    ([1 2015]
     (let ((puzzle-input (read-file-string "data/2015/day01.txt")))
       (format "Puzzle 1: ~a~nPuzzle 2: ~a"
               (count-1d puzzle-input)
               (count-1d-until puzzle-input negative?))))
    (else (format "Sorry, no solution yet for day ~a of ~a" day year))))

(define (main . args)
  (let ((cli-args (parse-args args)))
    (let ((year (string->number (hash-get cli-args 'year)))
          (day (string->number (hash-get cli-args 'day))))
      (displayln (solve year day)))))
