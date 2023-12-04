(ns aoc.2023.day4
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn str->numbers [part]
  (map #(Integer/parseInt %) (str/split (str/trim part) #" +")))

(defn line->card [line]
  (let [parts (str/split line #"[:|]")]
    {:card    (Integer/parseInt (str/trim (subs (first parts) 4)))
     :winning (str->numbers (second parts))
     :have    (str->numbers (last parts))}))

(defn read-input [input]
  (->> (str/split-lines input)
       (map line->card)))

(defn card-points [{:keys [winning have]}]
  (let [matches (set/intersection (set winning) (set have))]
    (if (seq matches)
      (int (Math/pow 2 (dec (count matches))))
      0)))

(defn solution1 [input]
  (->> (read-input input)
       (map card-points)
       (apply +)))

(defn add-copies [counts {:keys [card winning have]}]
  (let [matches (set/intersection (set winning) (set have))
        n       (get counts card)
        copies  (range (inc card) (+ (inc card) (count matches)))]
    (reduce (fn [counts card-copy] (update counts card-copy + n)) counts copies)))

(defn solution2 [input]
  (let [cards           (read-input input)
        starting-counts (reduce #(assoc %1 (:card %2) 1) {} cards)
        counts          (reduce add-copies starting-counts cards)]
    (apply + (vals counts))))
