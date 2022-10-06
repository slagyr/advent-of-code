(ns aoc.2015.day1)

(defn translate [char] (if (= \( char) 1 -1))

(defn navigate-floors [input] (apply + (map translate input)))

(defn basement-index [input]
  (loop [floor 0 index 1 moves (map translate input)]
    (if (seq moves)
      (let [floor (+ floor (first moves))]
        (if (neg? floor)
          index
          (recur floor (inc index) (rest moves))))
      (throw (Exception. "never went to basement")))))
