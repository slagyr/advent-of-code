(ns aoc.2022.day15
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-point [point-str]
  (let [[left right] (str/split point-str #", ")]
    [(core/->int (subs left 2))
     (core/->int (subs right 2))]))

(defn ->sensor [line]
  (let [[left right] (str/split line #":")]
    {:sensor (read-point (subs left 10))
     :beacon (read-point (subs right 22))}))

(defn read-input [input]
  (->> (str/split-lines input)
       (map ->sensor)))

(defn manhattan-distance [[x1 y1] [x2 y2]] (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn sensor-dead-range [{:keys [sensor beacon]} row]
  (let [dist       (manhattan-distance sensor beacon)
        [sx sy] sensor
        max-x-dist (- dist (abs (- sy row)))]
    (if (neg? max-x-dist)
      nil
      [(- sx max-x-dist) (+ sx max-x-dist)])))

(defn dead-ranges [sensors row]
  (->> (map #(sensor-dead-range % row) sensors)
       (remove nil?)))

(defn in-range? [[a b] x] (<= a x b))
(defn in-ranges? [ranges x] (some #(in-range? % x) ranges))

(defn beacon-cols [sensors row]
  (->> (map :beacon sensors)
       (filter #(= row (second %)))
       (map first)
       set
       count))

(defn solution1 [input row]
  (let [sensors (read-input input)
        ranges  (dead-ranges sensors row)
        min-x   (apply min (map first ranges))
        max-x   (apply max (map second ranges))]
    (- (->> (range min-x (inc max-x))
            (filter (partial in-ranges? ranges))
            count)
       (beacon-cols sensors row))))

(defn range- [[a1 a2] [b1 b2]]
  (cond (>= b1 a2) [[a1 a2]]
        (<= b2 a1) [[a1 a2]]
        (and (<= b1 a1) (>= b2 a2)) nil
        (and (>= b1 a1) (>= b2 a2)) [[a1 (dec b1)]]
        (and (<= b1 a1) (<= b2 a2)) [[(inc b2) a2]]
        (and (>= b1 a1) (<= b2 a2)) [[a1 (dec b1)][(inc b2) a2]]))

(defn subtract-range [ranges range]
  (remove nil? (mapcat #(range- % range) ranges)))

(defn find-beacon [sensors max-xy]
  (loop [row 0]
    ;(when (= 0 (rem row 10000)) (prn "row: " row))
    (if (> row max-xy)
      (throw (Exception. "Search exhausted"))
      (let [ranges (dead-ranges sensors row)]
        (if-let [result (seq (reduce subtract-range [[0 max-xy]] ranges))]
          [(first (first result)) row]
          (recur (inc row)))))))


(defn solution2 [input max-xy]
  (let [sensors (read-input input)
        [x y] (find-beacon sensors max-xy)]
    (+ (* x 4000000) y)))

