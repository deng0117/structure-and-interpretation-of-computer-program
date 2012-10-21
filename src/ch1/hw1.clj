(ns ch1.hw1
  (:require [clojure.contrib.generic.math-functions :as math]))


"Q1. Recall that we can assign new names to existing functions. 
Fill in the following function definition for adding a to 
the absolute value of b, without calling abs:"

(defn a-plus-abs-b [a b]
  (let [op (if (< b 0) - +)]
    (op a b)))


"Q2. Write a function that takes three positive numbers and 
returns the sum of the squares of the two larger numbers. 
Use only a single expression for the body of the function."

(defn q2 [x y z]
  (let [arg (list x y z)
        arg (sort arg)]
    (reduce + 
            (map math/sqrt 
                 (take-last 2 arg)))))


"Q4. Douglas Hofstadter's Pulitzer-prize-winning book,
Godel, Escher, Bach, poses the following mathematical puzzle.

    Pick a positive number n
    If n is even, divide it by 2.
    If n is odd, multipy it by 3 and add 1.
    Continue this process until n is 1.

The number n will travel up and down but eventually end at 1 
(at least for all numbers that have ever been tried -- 
nobody has ever proved that the sequence will terminate).

The sequence of values of n is often called a Hailstone sequence, 
because hailstones also travel up and down in the atmosphere before 
falling to earth. Write a function that takes a single argument 
with formal parameter name n, prints out the hailstone sequence 
starting at n, and returns the number of steps in the sequence.

Hailstone sequences can get quite long! Try 27. What's the 
longest you can find?"

(defn hofstadter-puzzle [initial-n]
  (loop [n initial-n count 0]
    (println "n:" n " count:" count)
    (if (= n 1)
      count
      (if (= (mod n 2) 0)
        (recur (/ n 2) 
               (inc count))
        (recur (+ (* 3 n) 1) 
               (inc count))))))



