(ns aoc.2022.day5
  (:require
    [aoc.core :as core]
    [clojure.string :as str]))

(defn read-move [line]
  (let [parts (str/split line #" ")]
    [(core/->int (nth parts 1))
     (dec (core/->int (nth parts 3)))
     (dec (core/->int (nth parts 5)))]))

(defn read-stack-line [stacks line]
  (reduce
    (fn [stacks n]
      (let [crate-index (* n 4)
            crate       (when (< crate-index (count line)) (subs line crate-index (+ crate-index 3)))]
        (if (str/blank? crate)
          stacks
          (update stacks n conj (second crate)))))
    stacks (range (count stacks))))

(defn read-stacks-and-moves [[stacks-lines moves-lines]]
  (let [stack-count  (Math/round (/ (float (count (last stacks-lines))) 4.0))
        empty-stacks (vec (take stack-count (repeat ())))]
    [(reduce read-stack-line empty-stacks (reverse (butlast stacks-lines)))
     (mapv read-move (rest moves-lines))]))

(defn read-input [input]
  (->> (str/split-lines input)
       (split-with (complement str/blank?))
       read-stacks-and-moves))

(defn move-crates [multi? stacks [n from to]]
  (let [crates (take n (nth stacks from))
        crates (if multi? crates (reverse crates))]
    (-> stacks
        (update from #(drop n %))
        (update to #(concat crates %)))))

(defn resulting-tops [multi? input]
  (let [[stacks moves] (read-input input)
        stacks (reduce (partial move-crates multi?) stacks moves)]
    (apply str (map first stacks))))
