#!/usr/bin/env gxi-script
(import :std/test)

(import "advent")

(define 2015/01-tests
  (test-suite "2015 day 1"
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
      (check (count-1d-until "()())" negative?) => 5)))

  )

(define 2015/02-tests
  (test-suite "2015 day 2"
    (test-case "I Was Told There Would Be No Math"
      ;; The elves are running low on wrapping paper, and so they need to submit an order
      ;; for more. They have a list of the dimensions (length l, width w, and height h)
      ;; of each present, and only want to order exactly as much as they need.

      ;; Fortunately, every present is a box (a perfect right rectangular prism), which
      ;; makes calculating the required wrapping paper for each gift a little easier:
      ;; find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also
      ;; need a little extra paper for each present: the area of the smallest side.
      (check (wrap-paper '(2 3 4)) => 58)
      (check (wrap-paper '(1 1 10)) => 43))
    (test-case "Part Two"

      ;; The elves are also running low on ribbon. Ribbon is all the same width, so they
      ;; only have to worry about the length they need to order, which they would again
      ;; like to be exact.

      ;; The ribbon required to wrap a present is the shortest distance around its side
      ;; s, or the smallest perimeter of any one face. Each present also requires a bow made
      ;; out of ribbon as well\; the feet of ribbon required for the perfect bow is equal to
      ;; the cubic feet of volume of the present. Don't ask how they tie the bow, though\;
      ;; they'll never tell.
      (check (wrap-ribbon '(2 3 4)) => 34)
      (check (wrap-ribbon '(1 1 10)) => 14)

      )))

(define (main . args)
  (let ((tests [2015/01-tests 2015/02-tests]))
    (apply run-tests! tests)
    (test-report-summary!)
    (exit (case (test-result) ((OK) 0) (else 1)))))

(main)
