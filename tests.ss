#!/usr/bin/env gxi-script
(import :std/test)

(import "advent")

(define 2015-01
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

(define 2015-02
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
      (check (wrap-ribbon '(1 1 10)) => 14))))

(define 2015-03
  (test-suite "2015 day 3"
    (test-case "Perfectly Spherical Houses in a Vacuum"
      ;; Santa is delivering presents to an infinite two-dimensional grid of houses.

      ;; He begins by delivering a present to the house at his starting location, and
      ;; then an elf at the North Pole calls him via radio and tells him where to move
      ;; next. Moves are always exactly one house to the north (^), south (v), east (>),
      ;; or west (<). After each move, he delivers another present to the house at his
      ;; new location.

      ;; However, the elf back at the north pole has had a little too much eggnog, and so
      ;; his directions are a little off, and Santa ends up visiting some houses more
      ;; than once. How many houses receive at least one present?
      (check (deliver-presents ">") => 2)
      (check (deliver-presents "^>v<") => 4)
      (check (deliver-presents "^v^v^v^v^v") => 2)
      (check (deliver-presents ">>>>>^>>><") => 10)
      )
    (test-case "Part Two"
      ;; The next year, to speed up the process, Santa creates a robot version of
      ;; himself, Robo-Santa, to deliver presents with him.

      ;; Santa and Robo-Santa start at the same location (delivering two presents to the
      ;; same starting house), then take turns moving based on instructions from the elf,
      ;; who is eggnoggedly reading from the same script as the previous year.

      ;; This year, how many houses receive at least one present?
      (check (deliver-presents-with-robo "^v") => 3)
      (check (deliver-presents-with-robo "^>v<") => 3)
      (check (deliver-presents-with-robo "^v^v^v^v^v") => 11))))

(define 2015-04
  (test-suite "2015 day 4"
    (test-case "The Ideal Stocking Stuffer"
      ;; Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
      ;; gifts for all the economically forward-thinking little girls and boys.

      ;; To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
      ;; least five zeroes. The input to the MD5 hash is some secret key (your puzzle
      ;; input, given below) followed by a number in decimal. To mine AdventCoins, you
      ;; must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
      ;; that produces such a hash.
      (check (md5-hex "abcdef" 609043) => "000001dbbfa3a5c83a2d506429c7b00e")
      (check (has-five-leading-zeroes? "000001dbbfa3a5c83a2d506429c7b00e") => #t)
      (check (has-five-leading-zeroes? "000021dbbfa3a5c83a2d506429c7b00e") => #f)
      (check (mine-adventcoins "abcdef") => 609043)
      (check (mine-adventcoins "pqrstuv") => 1048970))))

(define 2015-05
  (test-suite "2015 day 5"
    (test-case "Doesn't He Have Intern-Elves For This?"
      ;; Santa needs help figuring out which strings in his text file are naughty
      ;; or nice.

      ;; A nice string is one with all of the following properties:
      ;; - It contains at least three vowels (aeiou only), like aei, xazegov, or
      ;;   aeiouaeiouaeiou.
      ;; - It contains at least one letter that appears twice in a row, like xx,
      ;;   abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
      ;; - It does not contain the strings ab, cd, pq, or xy, even if they are
      ;;   part of one of the other requirements.

      ;; ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...),
      ;; a double letter (...dd...), and none of the disallowed substrings.
      (check (is-nice? "ugknbfddgicrmopn") => #t)
      ;; aaa is nice because it has at least three vowels and a double letter,
      ;; even though the letters used by different rules overlap.
      (check (is-nice? "aaa") => #t)
      ;; jchzalrnumimnmhp is naughty because it has no double letter.
      (check (is-nice? "jchzalrnumimnmhp") => #f)
      ;; haegwjzuvuyypxyu is naughty because it contains the string xy.
      (check (is-nice? "haegwjzuvuyypxyu") => #f)
      ;; dvszwmarrgswjxmb is naughty because it contains only one vowel.
      (check (is-nice? "dvszwmarrgswjxmb") => #f))
    (test-case "Part Two"

      ;; Realizing the error of his ways, Santa has switched to a better model of
      ;; determining whether a string is naughty or nice. None of the old rules apply, as
      ;; they are all clearly ridiculous.

      ;; Now, a nice string is one with all of the following properties:

      ;; - It contains a pair of any two letters that appears at least twice in the string
      ;;   without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa
      ;;   (aa, but it overlaps).

      ;; - It contains at least one letter which repeats with exactly one letter between
      ;;   them, like xyx, abcdefeghi (efe), or even aaa.

      ;; qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj)
      ;; and a letter that repeats with exactly one letter between them (zxz).
      (check (is-nice-now? "qjhvhtzxzqqjkmpb") => #t)

      ;; xxyxx is nice because it has a pair that appears twice and a letter that
      ;; repeats with one between, even though the letters used by each rule overlap.
      (check (is-nice-now? "xxyxx") => #t)

      ;; uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat
      ;; with a single letter between them.
      (check (is-nice-now? "uurcxstgmygtbstg") => #f)

      ;; ieodomkazucvgmuy is naughty because it has a repeating letter with one
      ;; between (odo), but no pair that appears twice.
      (check (is-nice-now? "ieodomkazucvgmuy") => #f)

      )))

(define (main . args)
  (let ((tests [2015-01
                2015-02
                2015-03
                2015-04
                2015-05
                ]))
    (apply run-tests! tests)
    (test-report-summary!)
    (exit (case (test-result) ((OK) 0) (else 1)))))

(main)
