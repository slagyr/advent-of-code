(ns aoc.2023.day9
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map #(map core/->long %))))

(defn differences [values]
  (->> (partition 2 1 values)
       (map (fn [[a b]] (- b a)))))

(defn history [values]
  (loop [result [] history (iterate differences values)]
    (let [row (first history)]
      (if (every? zero? row)
        (conj result row)
        (recur (conj result row) (rest history))))))

(defn extrapolate [history]
  (loop [result [0] values (rest (reverse (map last history)))]
    (if (seq values)
      (let [n (+ (last result) (first values))]
        (recur (conj result n) (rest values)))
      (reverse result))))

(defn solution1 [input]
  (->> (read-input input)
       (map history)
       (map extrapolate)
       (map first)
       (apply +)))

(defn extrapolate-first [history]
  (loop [result [0] values (rest (reverse (map first history)))]
    (if (seq values)
      (let [n (- (first values) (last result))]
        (recur (conj result n) (rest values)))
      (reverse result))))

(defn solution2 [input]
  (->> (read-input input)
       (map history)
       (map extrapolate-first)
       (map first)
       (apply +)))
