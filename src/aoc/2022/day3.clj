(ns aoc.2022.day3
  (:require [aoc.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn read-rucksacks [input]
  (->> (str/split-lines input)
       (map #(split-at (/ (count %) 2) %))))

(defn common-item [[sack1 sack2]]
  (first (set/intersection (set sack1) (set sack2))))

(defn priority [item]
  (let [v (int item)]
    (if (< v 97)
      (- v 38)
      (- v 96))))

(defn priority-total [input]
  (->> (read-rucksacks input)
       (map common-item)
       (map priority)
       (apply +)))

(defn badge-type [group]
  (->> group
       (map #(apply concat %))
       (map set)
       (apply set/intersection)
       first))

(defn group-priority-total [input]
  (->> (read-rucksacks input)
       (partition 3)
       (map badge-type)
       (map priority)
       (apply +)))
