(ns ch1.hw2-higher-order-function
  (:use clojure.test))


"Q1. The summation function from lecture is only the simplest 
of a vast number of similar abstractions that can be captured 
as higher-order functions.

def summation(n, term, next=lambda x: x+1):
    total, k = 0, 1
    while k <= n:
        total, k = total + term(k), next(k)
    return total
"
(def identity-1 (fn [x] x))

(def default-term identity-1)
(def default-next inc)

(defn summation 
  [n & 
   {:keys [term next] 
    :or {term default-term
         next default-next}}]
  (reduce +
          (map term
               (take n
                     (iterate next 
                              1)))))

"Write a similar product function that returns the product 
of the values of a function for n natural number arguments. 
Show how to define the factorial function in terms of product"

(defn product
  "Return the product of the first n terms in a sequence.  
  term -- a function that takes one argument"
  [n & 
   {:keys [term next] 
    :or {term default-term
         next default-next}}]
  (reduce *
          (map term
               (take n
                     (iterate next 
                              1)))))

(defn factorial
  "Return n factorial by calling product"
  [n]
  (product n))


"Q2. Show that both summation and product are instances of a 
more general function, called accumulate

Accumulate takes as arguments the same arguments term and n as summation 
and product, together with a combiner function (of two arguments) that 
specifies how the current term is to be combined with the accumulation 
of the preceding terms and a start value that specifies what base value 
to use to start the accumulation. Implement accumulate and show how summation 
and product can both be defined as simple calls to accumulate."

(defn accumulate 
  "Return the result of combining the first n terms in a sequence"
  [combiner n & 
   {:keys [start term next] 
    :or {start 1
         term default-term
         next default-next}}]
  (reduce combiner
          (map term
               (take n
                     (iterate next 
                              start)))))

(defn summation-with-accumulate
  [n & 
   {:keys [term next] 
    :or {term default-term
         next default-next}}]
  (accumulate + n :start 1 :term term :next next))

(defn product-with-accumulate
  [n & 
   {:keys [term next] 
    :or {term default-term
         next default-next}}]
  (accumulate * n :start 1 :term term :next next))


"Q3. Define a function double that takes a function of one argument 
as an argument and returns a function that applies the original 
function twice. For example, if successor is a function that 
returns 1 more than its argument, then double(inc) should be a 
function that returns two more:"

(defn compose-1 
  "Return a function h, such that h(x) = f(g(x))."
  [f g]
  #(f (g %)))

(defn my-double 
  "Return a function that applies f twice."
  [f]
  (compose-1 f f))


"Q4. If f is a numerical function and n is a positive integer, 
then we can form the nth repeated application of f, which is defined 
to be the function whose value at x is f(f(...(f(x))...)). 
For example, if f adds 1 to its argument, then the nth repeated 
application of f adds n. Write a function that takes as inputs a 
function f and a positive integer n and returns the function that 
computes the nth repeated application of f:"

(defn repeated 
  "Return the function that computes the nth application of f"
  [f initial-n]
  (loop [repeated-n identity-1
         n initial-n]
    (if (= n 0)
      repeated-n
      (recur (compose-1 f repeated-n) 
             (dec n)))))


"Extra for Experts: Q5. The logician Alonzo Church invented a system 
of representing non-negative integers entirely using functions. 
Here are the definitions of 0, and a function that returns 1 more 
than its argument:

def zero(f):
    return lambda x: x

def successor(n):
    return lambda f: lambda x: f(n(f)(x))
    
This representation is known as Church numerals. 
Define one and two directly, which are also functions. 
To get started, apply successor to zero. Then, give a direct definition 
of the add function (not in terms of repeated application of successor) 
over Church numerals. Finally, implement a function church_to_int that 
converts a church numeral argument to a regular Python int.

It is easy to find answers to this question on the Internet. 
Try to solve it on your own before looking it up!

Note: 'Extra for Experts' problems are completely optional. 
You are encouraged to try these questions, but certainly don't be 
discouraged if you don't solve them."

(defn zero 
  "Church numeral 0."
  [f]
  identity-1)

(defn successor [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(defn one 
  "Church numeral 1."
  [f]
  (fn [x]
    (f ((zero f) x))))

(def two (successor one))

(defn add-church 
  "Return the Church numeral for m + n, for Church numerals m and n.
  give a direct definition of the add function 
  (not in terms of repeated application of successor)"
  [m n]
  (fn [f]
    (fn [x]
      ((m f) ((n f) x)))))

(defn church-to-int 
  "Convert the Church numeral n to a integer"
  [n]
  ((n inc) 0))




"use run-test in REPL"
(deftest test-hw2
  (testing 
    "higher order function"
    (is (= (product 5) (product-with-accumulate 5)))
    (is (= (summation 10) (summation-with-accumulate 10))))
  
  (testing 
    "church numeral"
    (is (= (church-to-int one) 1))
    (is (= (church-to-int two) 2))
    (is (= (church-to-int (add-church one two)) 3))
    (is (= (church-to-int (add-church two (add-church two two))) 6))))



