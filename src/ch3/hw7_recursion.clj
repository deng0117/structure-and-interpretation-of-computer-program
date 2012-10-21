(ns ch3.hw7-recursion
  (:use clojure.test))

"Homework 7: Recursion"

"1) The number of partitions of a positive integer n is the number of ways in
which n can be expressed as the sum of positive integers in increasing order.
For example, the number 5 has 7 partitions:

    5 = 5
    5 = 1 + 4
    5 = 2 + 3
    5 = 1 + 1 + 3
    5 = 1 + 2 + 2
    5 = 1 + 1 + 1 + 2
    5 = 1 + 1 + 1 + 1 + 1

Write a tree-recursive function part(n) that returns the number of partitions
of n. 

Hint: Introduce a locally defined function that computes partitions of n using only
a subset of the integers less than or equal to n.  Once you have done so, you
can use very similar logic to the count_change function from lecture."

(defn count-change [amount kinds]
  (cond (= amount 0) 1
        (< amount 0) 0
        (= (count kinds) 0) 0
        :else (+ (count-change amount 
                               (rest kinds))
                 (count-change (- amount (first kinds)) 
                               kinds))))

(defn part 
  "Return the number of partitions of positive integer n
  (part 5) = 7"
  [n]
  (let [amount n
        kinds (take n
                    (iterate inc
                             1))]
    (count-change amount 
                  kinds)))


"2) A mathematical function g is defined by two cases:
   
   g(n) = n,                                       if n < 4
   g(n) = g(n - 1) + 2 * g(n - 2) + 3 * g(n - 3),  if n > 3 
   
Write a recursive function that computes g. Then, write an iterative function
that computes g."

(defn g-recursive [n]
  (let [f (fn [n]
            (+ (* 1 (g-recursive (- n 1)))
               (* 2 (g-recursive (- n 2)))
               (* 3 (g-recursive (- n 3)))))]
    (if (< n 4)
      n
      (f n))))


(defn f-iterative [[x y z]]
  [y z (+ z
          (* 2 y)
          (* 3 x))])

(defn g-iterative [n]
  (nth (map first
            (iterate f-iterative
                     [1 2 3]))
       (dec n)))  ;nth function start from zero


"3) A perfect number is defined as a positive integer equal to the sum of all
its factors less than itself. For example, the first perfect number is 6,
because its factors are 1, 2, 3, and 6, and 1+2+3=6. The second perfect number
is 28, because 1+2+4+7+14=28.

Write a function next_perfect(n) that tests numbers starting with n and
continuing with n+1, n+2, etc. until a perfect number is found. You will need
to implement sum_of_factors as well."

(defn sum-of-factors 
  "Return the sum of the factors of n less than n. A factor of a positive
  integer n is a positive integer that divides n evenly."
  [n]
  (reduce +
          (filter #(= 0 (mod n %)) 
                  (take (dec n)
                        (iterate inc
                                 1)))))

(defn next-perfect 
  "Return the smallest perfect number greater than or equal to n."
  [n]
  (first (filter #(= % (sum-of-factors %))
                 (iterate inc
                          n))))


"4.5) Write a data-directed apply function that computes the area or
perimeter of either a square or a rectangle."

(defmulti data-directed-apply :obj-type)  ;dispatch on :obj-type

(defmethod data-directed-apply ::square [m]
  (let [side (get m :side)
        perimeter (* 4 side)]
    perimeter))

(defmethod data-directed-apply ::rect [m]
  (let [width (get m :width)
        height (get m :height)
        area (* width height)]
    area))

(defn square [side]
  {:obj-type ::square, 
   :side side})

(defn rect [width height]
  {:obj-type ::rect, 
   :width width,
   :height height})


;#####################
;# EXTRA FOR EXPERTS #
;#####################

"5) The Y-combinator

The recursive factorial function can be written as a single expression by using
a conditional expression (Python syntax not introduced in the notes).
http://docs.python.org/py3k/reference/expressions.html#conditional-expressions

>>> fact = lambda n: 1 if n == 1 else mul(n, fact(sub(n, 1)))
>>> fact(5)
120"

(defn fact [n]
  (if (= 1 n)
    1
    (* n
       (fact (dec n)))))

"To write a recursive function previously, we have always given it a name using
a def or assignment statement so that we can refer to the function within its
own body.  In this question, your job is to define fact without giving it a
name!

Write an expression that computes n factorial using only call expressions,
conditional expressions, and lambda expressions (no assignment or def
statements).  The sub and mul functons from the operator module are the only
built-in function required to solve this problem. 

Write your solution as a doctest by filling in the expression below.

>>> (lambda n: YOUR_CODE_HERE)(5)
120

Hint: Googling Y-combinator will tell you the solution, so don't do that until
you've tried to solve the problem yourself!"

; Y Combinator definition:
; Y = λf.(λx.f (x x)) (λx.f (x x))
;
; it is used to find fixed point of higher order function,
; which is also a function
;
; g is higher order function, Y(g) is fixed point of g, then:
; g(Y(g)) = Y(g)

(defn y-combinator [f]
  (let [lambda-x (fn [g]
                   (f (fn [x] 
                        ((g g) x))))]
    (lambda-x lambda-x)))

;http://www.fatvat.co.uk/2009/04/understanding-y-combinator.html
(defn y-combinator-from-web [f]
  ((fn [g] (g g)) (fn [g]
                    (f (fn [x] 
                         ((g g) x))))))

; F = λf. λx. (ISZERO x) 1 (MULT x (f (PRED x)))
; Intuitively, F is constructed from the structure of factorial function, 
; so that factorial is the fixed point of F,
; therefor y-combinator can find it out:
;
; Y(F) = fact
;
; proof, denote Y by fix:
; fix(F)(n) = F(fix(F))(n) = λn. (ISZERO n) 1 (MULT n (fix(F) (PRED n)))
;
; let fix(F) = fact
; fact = λn. (ISZERO n) 1 (MULT n (fact (PRED n)))
;
; f in y-combinator is a place-holder argument 
; for the factorial function itself.
(defn lambda-fact [f]
  (fn [n]
    (if (= 1 n)
      1
      (* n 
         (f (dec n))))))

(defn lambda-sum [f]
  (fn [n]
    (if (= 1 n)
      1
      (+ n
         (f (dec n))))))

(defn sum [n]
 (reduce + 
         (take n 
               (iterate inc
                        1))))

(defn lambda-sum-seq [f]
  (fn [s]
    (if (empty? s)
      0
      (+ (first s)
         (f (rest s))))))

(defn sum-seq [s]
  (reduce +
          s))


"6) Lists with loops.

The Rlist class (copied below) can represent lists with cycles.  That is, a
list may contain itself as a sublist.

>>> s = Rlist(1, Rlist(2, Rlist(3)))
>>> s.rest.rest.rest = s
>>> s[20]
3

This question has two parts: 
  A) Write a function has_cycle that returns True if and only if its argument,
     an Rlist instance, contains a cycle.
  B) Write a function has_cycle_constant that has the same behavior as
     has_cycle but requires only a constant amount of space.

Hint: The solution to B is short (~10 lines of code), but requires a clever
idea. Try to discover the solution yourself before asking around."


;#####################
;# CODE FROM LECTURE #
;#####################

"class Rlist(object):
    #A recursive list consisting of a first element and the rest.
    class EmptyList(object):
        def __len__(self):
            return 0

    empty = EmptyList()

    def __init__(self, first, rest=empty):
        self.first = first
        self.rest = rest

    def __repr__(self):
        args = repr(self.first)
        if self.rest is not Rlist.empty:
            args += ', {0}'.format(repr(self.rest))
        return 'Rlist({0})'.format(args)

    def __len__(self):
        return 1 + len(self.rest)

    def __getitem__(self, i):
        if i == 0:
            return self.first
        return self.rest[i-1]
"




"use run-test in REPL"
(deftest test-hw7
  
  "recursion"
  (is (= 7 
         (part 5)))
  (is (= 10 
         (g-recursive 4)))
  (is (= 10 
         (g-iterative 4)))
  (is (= (g-recursive 10) 
         (g-iterative 10)))
  
  "perfect number"
  (is (= 6 
         (next-perfect 5)))
  (is (= 496 
         (next-perfect 29)))
  
  "polymorphism"
  (is (= 40 
         (data-directed-apply (square 10))))
  (is (= 500 
         (data-directed-apply (rect 5 100))))
  
  "y-combinator"
  (is (= (fact 10) 
         ((y-combinator lambda-fact) 10)))
  (is (= (sum 10) 
         ((y-combinator lambda-sum) 10)))
  (is (= (sum-seq [2 4 6 8 10]) 
         ((y-combinator lambda-sum-seq) [2 4 6 8 10]))))







