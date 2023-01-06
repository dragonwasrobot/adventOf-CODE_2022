#lang racket

(require json)

(define sample-input "
  [1,1,3,1,1]
  [1,1,5,1,1]

  [[1],[2,3,4]]
  [[1],4]

  [9]
  [[8,7,6]]

  [[4,4],4,4]
  [[4,4],4,4,4]

  [7,7,7,7]
  [7,7,7]

  []
  [3]

  [[[]]]
  [[]]

  [1,[2,[3,[4,[5,6,7]]]],8,9]
  [1,[2,[3,[4,[5,6,0]]]],8,9]
  ")

(define (read-input use-file)
  (if use-file
      (port->string (open-input-file "day13-input.txt") #:close? #t)
      sample-input))

(define (parse-input use-file)
  (let* ((raw-input (string-trim (read-input use-file)))
         (pairs (map string-trim (string-split raw-input "\n\n"))))
    (map (lambda (pair)
           (map (lambda (str) (string->jsexpr str))
                (string-split pair "\n")))
         pairs)))

(define (ordered? left right) (equal? (compare left right) 'lt))

(define (compare left right)
  (cond
    [(and (integer? left) (integer? right) (< left right)) 'lt]
    [(and (integer? left) (integer? right) (> left right)) 'gt]
    [(and (integer? left) (integer? right) (= left right)) 'eq]
    [(and (list? left) (list? right) (empty? left) (not (empty? right))) 'lt]
    [(and (list? left) (list? right) (not (empty? left)) (empty? right)) 'gt]
    [(and (list? left) (list? right) (empty? left) (empty? right)) 'eq]
    [(and (list? left) (integer? right)) (compare left (list right))]
    [(and (integer? left) (list? right)) (compare (list left) right)]
    [(and (list? left) (list? right))
     (let* ((x (car left))
            (xs (cdr left))
            (y (car right))
            (ys (cdr right))
            (compared (compare x y)))
       (cond
         [(equal? compared 'lt) 'lt]
         [(equal? compared 'gt) 'gt]
         [(equal? compared 'eq) (compare xs ys)]))]))

(define (compute-part1 pairs)
  (let* ((ordered-results (map (lambda (pair) (ordered? (first pair) (second pair)))
                               pairs))
         (sum-of-indices (foldl
                          (lambda (b acc)
                            (if b
                                (cons (+ (cdr acc) (car acc)) (+ 1 (cdr acc)))
                                (cons (car acc) (+ 1 (cdr acc)))
                                ))
                          (cons 0 1)
                          ordered-results)))
    (car sum-of-indices)))

(define (compute-part2 pairs)
  (let* ((divider1 '((2)))
         (divider2 '((6)))
         (all-signals (foldl
                           (lambda (pair acc) (cons (first pair) (cons (second pair) acc)))
                           (list divider1 divider2)
                           pairs))
         (sorted-signals (sort all-signals ordered?))
         (divider1-idx (+ 1 (index-of sorted-signals divider1)))
         (divider2-idx (+ 1 (index-of sorted-signals divider2))))
    (* divider1-idx divider2-idx)))

(define (run use-file)
  (let* ((pairs (parse-input use-file))
         (part1 (compute-part1 pairs))
         (part2 (compute-part2 pairs)))

    (displayln "Answer 1:")
    (displayln part1)

    (displayln "Answer 1:")
    (displayln part2)))

(run true)
;; Sample input part 1: 13
;; Sample input part 2: 140
;; File input part 1: 5938
;; File input part 2: 29095
