(ns aoc.2022.day1
  (:require [clojure.string :as str]))

(defn read-snack-group [snack-strings]
  (let [snacks (map #(Integer/parseInt %) snack-strings)]
    {:snacks snacks :total (apply + snacks)}))

(defn read-snacks [input]
  (->> (str/split-lines input)
       (partition-by str/blank?)
       (remove #(= % [""]))
       (map read-snack-group)))

(defn max-calories [input]
  (let [snacks (read-snacks input)]
    (apply max (map :total snacks))))

(defn top-3-calories [input]
  (->> (read-snacks input)
       (sort-by :total)
       reverse
       (take 3)
       (map :total)
       (apply +)))
