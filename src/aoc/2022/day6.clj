(ns aoc.2022.day6)

(defn start-of-packet? [n input i]
  (let [marker (take n (subs input i))]
    (= n (count (set marker)))))

(defn start-of-something [n input]
  (+ n (first (filter (partial start-of-packet? n input) (range (count input))))))

(defn start-of-packet [input] (start-of-something 4 input))

(defn start-of-message [input] (start-of-something 14 input))
