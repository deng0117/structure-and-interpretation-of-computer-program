(ns ch2.hw5)



(defn make-withdraw [balance]
  (let [b (atom balance)]
    (fn [amount]
      (if (<= amount (deref b))
        (swap! b 
               (fn [x]
                 (- x amount)))
        "Insufficient funds"))))

(defn accumulator []
  (let [x (atom 0)]
    (fn []
      (swap! x inc))))

