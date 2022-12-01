(require "bignum-operators.rkt")

;; Data Definitions

;; bignum

;;   empty

;;   (list x y) where a is a digit and b is a bignum

;;              EXCEPT: a can't be 0 when b is empty

;; nothing else is a bignum

;;

;; bignum's number representation:

;;   each new list element indicates a power of ten, starting

;;   with the lowest power and moving to the highest. so, each

;;   digit represents digit*10^n position, starting with n=0

;;   and, 0 is represented separately as empty

;;

;; examples of bignum:

;; empty represents 0

;; (list 4 2 6 5) represents 5624

;; (list 0 1) represents 10



;; bignum+: bignum * bignum -> bignum

;; input: 2 bignums, bignum1 and bignum2, to be added.

;; output: a bignum, the sum of bignum1 and bignum2

;;

;; Recursion Diagrams

;; OI: (list 1 2 3) (list 0 2)

;;  RI: (list 2 3) (list 2)

;;  RO: (list 4 3)

;;   I: cons addition of first of both lists in OI to RO?

;; OO: (list  1 4 3)

;; OI: (list 0 3) (list 1)

;;  RI: (list 3) empty

;;  RO: (list 3)

;;   I: cons addition of first of both lists in OI to RO?

;; OO: (list 1 3)

;; OI: (list 0 3) empty

;;  RI: NA

;;  RO: NA

;;   I: base cases are when either list is empty?

;; OO: (list 0 3)

(define (bignum+ bignum1 bignum2)

  (cond

    [(empty? bignum1) bignum2]

    [(empty? bignum2) bignum1]

    [(and (cons? bignum1) (cons? bignum2))

     (cons (digit-rem (digit-add (first bignum1) (first bignum2)) 10)

           (bignum+ (carry (rest (if (empty? (rest bignum1)) bignum1 bignum2))

                           (digit-quo (digit-add (first bignum1)

                                                 (first bignum2)) 10))

                    (rest (if (empty? (rest bignum1)) bignum2 bignum1))))]))

;; Testing for bignum+:

;; 1 + 9 = 10

(check-expect (bignum+ (list 1) (list 9)) (list 0 1))

;; 0 + 0 = 0

(check-expect (bignum+ empty empty) empty)

;; 0 + 1 = 1

(check-expect (bignum+ empty (list 1)) (list 1))

;; 1 + 0 = 1

(check-expect (bignum+ (list 1) empty) (list 1))

;; 1 + 1 = 2

(check-expect (bignum+ (list 1) (list 1)) (list 2))

;; 0 + 99 = 99

(check-expect (bignum+ empty (list 9 9)) (list 9 9))

;; 0 + 98 = 98

(check-expect (bignum+ empty (list 8 9)) (list 8 9))

;; 1 + 99 = 100

(check-expect (bignum+ (list 1) (list 9 9)) (list 0 0 1))

;; 1 + 98 = 99

(check-expect (bignum+ (list 1) (list 8 9)) (list 9 9))

;; 99 + 0 = 99

(check-expect (bignum+ (list 9 9) empty) (list 9 9))

;; 98 + 0 = 98

(check-expect (bignum+ (list 8 9) (list 0)) (list 8 9))

;; 99 + 1 = 100

(check-expect (bignum+ (list 9 9) (list 1)) (list 0 0 1))

;; 98 + 1 = 99

(check-expect (bignum+ (list 8 9) (list 1)) (list 9 9))

;; 99 + 99 = 198

(check-expect (bignum+ (list 9 9) (list 9 9)) (list 8 9 1))

;; 98 + 98 = 196

(check-expect (bignum+ (list 8 9) (list 8 9)) (list 6 9 1))

;; 1 + 2 = 3

(check-expect (bignum+ (list 1) (list 2)) (list 3))

;; 5 + 6 = 11

(check-expect (bignum+ (list 5) (list 6)) (list 1 1))

;; 1 + 19 = 20

(check-expect (bignum+ (list 1) (list 9 1)) (list 0 2))

;; 19 + 25 = 44

(check-expect (bignum+ (list 9 1) (list 5 2)) (list 4 4))

;; 91 + 25 = 116

(check-expect (bignum+ (list 1 9) (list 5 2)) (list 6 1 1))

;; 55 + 55 = 110

(check-expect (bignum+ (list 5 5) (list 5 5)) (list 0 1 1))

;; 10000 + 10 = 10010

(check-expect (bignum+ (list 0 0 0 0 1) (list 0 1)) (list 0 1 0 0 1))

;; 9019 + 49145 = 58164

(check-expect (bignum+ (list 9 1 0 9) (list 5 4 1 9 4)) (list 4 6 1 8 5))

;; 55554 + 555566 = 611120

(check-expect (bignum+ (list 4 5 5 5 5) (list 6 6 5 5 5 5)) (list 0 2 1 1 1 6))

;; 1 + 999999 = 1000000

(check-expect (bignum+ (list 1) (list 9 9 9 9 9 9)) (list 0 0 0 0 0 0 1))

;; 999999 + 1 = 1000000

(check-expect (bignum+ (list 9 9 9 9 9 9) (list 1)) (list 0 0 0 0 0 0 1))

;; 123456789 + 987654321 = 1111111110

(check-expect (bignum+ (list 9 8 7 6 5 4 3 2 1) (list 1 2 3 4 5 6 7 8 9))

              (list 0 1 1 1 1 1 1 1 1 1))



;; carry: bignum * num -> bignum

;; input: a bignum, bignum, and a digit, num

;; output: a bignum, the sum of the inputed bignum and num

(define (carry bignum num)

  (cond

    [(empty? bignum) (if (zero? num) empty (cons num empty))]

    [(cons? bignum)

     (cons (digit-rem (digit-add num (first bignum)) 10)

           (if (zero? (digit-quo (digit-add num (first bignum)) 10))

               (rest bignum)

               (if (cons? (rest bignum))

                   (cons (digit-add (digit-quo (digit-add (first bignum)

                                                          num) 10)

                                    (first (rest bignum)))

                         (rest (rest bignum)))

                   (cons (digit-quo (digit-add num

                                               (first bignum))

                                    10) empty))))]))

;; Testing for carry:

(check-expect (carry (list 1 1 1)  1) (list 2 1 1))

(check-expect (carry (list 5 6) 3) (list 8 6))

(check-expect (carry (list 5) 5) (list 0 1))

(check-expect (carry (list 5 5) 5) (list 0 6))

(check-expect (carry (list 1 2) 0) (list 1 2))

(check-expect (carry empty 2) (list 2))



;; bignum*: bignum * bignum -> bignum

;; input: 2 bignums, bignum1 and bignum2

;; output: a bignum, the product of bignum1 and bignum2

;;

;; Recursion Diagrams

;; OI: (list 1 2 3) (list 3 2 1)

;;  RI: (list 2 3) (list 3 2 1)

;;   RI: (list 3) (list 3 2 1)

;;   RO: (list 0 0 9 6 3)

;;  RO: (list 0 6 3 9 3)

;;   I: add 1 * (list 3 2 1) to RO?

;; OO: (list 3 8 4 9 3)

;; OI: empty (list 1)

;;  RI: empty (list 1)

;;  RO: empty

;;   I: base case is empty?

;; OO: empty

;; OI: (list 1 1) (list 4)

;;  RI: (list 1) (list 4)

;;  RO: (list 0 4)

;;   I: add first of first bignum * second bignum to RO?

;; OO: (list 4 4)

(define (bignum* bignum1 bignum2)

  (cond

    [(or (empty? bignum1) (empty? bignum2)) empty]

    [(cons? bignum1)

     (bignum+ (partial-product (first bignum1) bignum2)

              (cons 0 (bignum* (rest bignum1) bignum2)))]))

;; Testing for bignum*:

;; 1 * 9 = 9

(check-expect (bignum* (list 1) (list 9)) (list 9))

;; 0 * 0 = 0

(check-expect (bignum* empty empty) empty)

;; 0 * 1 = 0

(check-expect (bignum* empty (list 1)) empty)

;; 1 * 0 = 0

(check-expect (bignum* (list 1) empty) empty)

;; 1 * 1 = 1

(check-expect (bignum* (list 1) (list 1)) (list 1))

;; 0 * 99 = 0

(check-expect (bignum* empty (list 9 9)) empty)

;; 0 * 98 = 0

(check-expect (bignum* empty (list 8 9)) empty)

;; 1 * 99 = 99

(check-expect (bignum* (list 1) (list 9 9)) (list 9 9))

;; 1 * 98 = 98

(check-expect (bignum* (list 1) (list 8 9)) (list 8 9))

;; 99 * 0 = 0

(check-expect (bignum* (list 9 9) empty) empty)

;; 98 * 0 = 0

(check-expect (bignum* (list 8 9) empty) empty)

;; 99 * 1 = 99

(check-expect (bignum* (list 9 9) (list 1)) (list 9 9))

;; 98 * 1 = 98

(check-expect (bignum* (list 8 9) (list 1)) (list 8 9))

;; 99 * 99 = 9801

(check-expect (bignum* (list 9 9) (list 9 9)) (list 1 0 8 9))

;; 98 * 98 = 9604

(check-expect (bignum* (list 8 9) (list 8 9)) (list 4 0 6 9))

;; 1 * 2 = 2

(check-expect (bignum* (list 1) (list 2)) (list 2))

;; 5 * 6 = 30

(check-expect (bignum* (list 5) (list 6)) (list 0 3))

;; 1 * 19 = 19

(check-expect (bignum* (list 1) (list 9 1)) (list 9 1))

;; 19 * 25 = 475

(check-expect (bignum* (list 9 1) (list 5 2)) (list 5 7 4))

;; 91 * 25 = 2275

(check-expect (bignum* (list 1 9) (list 5 2)) (list 5 7 2 2))

;; 55 * 55 = 3025

(check-expect (bignum* (list 5 5) (list 5 5)) (list 5 2 0 3))

;; 10000 * 10 = 100000

(check-expect (bignum* (list 0 0 0 0 1) (list 0 1)) (list 0 0 0 0 0 1))

;; 9019 * 49145 = 443238755

(check-expect (bignum* (list 9 1 0 9) (list 5 4 1 9 4))

              (list 5 5 7 8 3 2 3 4 4))

;; 55554 * 555566 = 30863913564

(check-expect (bignum* (list 4 5 5 5 5) (list 6 6 5 5 5 5))

              (list 4 6 5 3 1 9 3 6 8 0 3))

;; 1 * 999999 = 999999

(check-expect (bignum* (list 1) (list 9 9 9 9 9 9)) (list 9 9 9 9 9 9))

;; 999999 * 1 = 999999

(check-expect (bignum* (list 9 9 9 9 9 9) (list 1)) (list 9 9 9 9 9 9))



;; partial-product: num * bignum -> bignum

;; input: a positive number, multiplier, and a bignum, bignum

;; output: another bignum, the result of multiplying each digit

;;         of the inputed bignum by a single digit.

;;         EXEPT 0: if multiplier is 0, the output is empty.

;;       

Instructor
| 10/27 at 3:57 am
Grading comment:

good job!

;; Recursion Diagrams

;; OI: 2 (list 1 2 3)

;;  RI: 2 (list 2 3)

;;  RO: (list 4 6)

;;   I: cons 1 * 2 onto RO?

;; OO: (list  2 4 6)

;; OI: 2 empty

;;  RI: NA

;;  RO: NA

;;   I: empty would be a base case

;; OO: empty

(define (partial-product multiplier bignum)

  (if (zero? multiplier) empty

      (cond

        [(empty? bignum) empty]

        [(cons? bignum) (cons (digit-mul multiplier (first bignum))

                              (partial-product multiplier (rest bignum)))])))

;; Testing for partial-product:

(check-expect (partial-product 1 (list 1 2 3 4)) (list 1 2 3 4))

(check-expect (partial-product 2 (list 1 2 3 4)) (list 2 4 6 8))

(check-expect (partial-product 0 (list 1 2 3 4)) empty)

(check-expect (partial-product 0 empty) empty)

(check-expect (partial-product 3 (list 3)) (list 9))
