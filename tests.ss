#!/usr/bin/env gxi-script
(import :std/test)

(import "advent")

(define 2015-advent-tests
  (test-suite "--- Day 1"
    (test-case "Not Quite Lisp"
      ;; Santa is trying to deliver presents in a large apartment building, but he can't
      ;; find the right floor - the directions he got are a little confusing. He starts
      ;; on the ground floor (floor 0) and then follows the instructions one character at
      ;; a time.

      ;; An opening parenthesis, (, means he should go up one floor, and a closing
      ;; parenthesis, ), means he should go down one floor.

      ;; The apartment building is very tall, and the basement is very deep; he will
      ;; never find the top or bottom floors.
      (check (count-1d "(())") => 0)
      (check (count-1d "()()") => 0)
      (check (count-1d "(((") => 3)
      (check (count-1d "(()(()(") => 3)
      (check (count-1d "))(((((") => 3)
      (check (count-1d "())") => -1)
      (check (count-1d "))(") => -1)
      (check (count-1d ")))") => -3)
      (check (count-1d ")())())") => -3))
    (test-case "Part Two"
      ;; Now, given the same instructions, find the position of the first
      ;; character that causes him to enter the basement (floor -1). The first character
      ;; in the instructions has position 1, the second character has position 2, and so
      ;; on.
      (check (count-1d-until "(" negative?) => 1)
      (check (count-1d-until "()())" negative?) => 5))))

(define (main . args)
  (let ((tests [2015-advent-tests]))
    (apply run-tests! tests)
    (test-report-summary!)
    (exit (case (test-result) ((OK) 0) (else 1)))))

(main)
