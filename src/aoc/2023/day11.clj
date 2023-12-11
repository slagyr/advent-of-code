(ns aoc.2023.day11
  (:require [aoc.core :as core]
            [clojure.math.combinatorics :as math]
            [clojure.string :as str]))

(defn read-input [input]
  (let [data      (mapv vec (str/split-lines input))
        rows      (count data)
        cols      (count (first data))
        locations (for [row (range rows) col (range cols)] [row col])]
    {:rows     rows
     :cols     cols
     :galaxies (->> locations
                    (map #(when (= \# (get-in data %)) {:location %}))
                    (remove nil?)
                    (map-indexed (fn [i g] (assoc g :id (inc i)))))}))

(defn empty-cols [{:keys [cols galaxies] :as universe}]
  (filter
    (fn [n] (not (some #(= n (second (:location %))) galaxies)))
    (range cols)))

(defn empty-rows [{:keys [rows galaxies] :as universe}]
  (filter
    (fn [n] (not (some #(= n (first (:location %))) galaxies)))
    (range rows)))

(defn expand-galaxy [x rows cols galaxy]
  (let [[row col] (:location galaxy)
        dr (count (filter #(< % row) rows))
        dc (count (filter #(< % col) cols))]
    (assoc galaxy :location [(+ row (- dr) (* x dr)) (+ col (- dc) (* x dc))])))

(defn expand [x universe]
  (let [ex-cols (empty-cols universe)
        ex-rows (empty-rows universe)]
    {:rows     (+ (:rows universe) (- (count ex-rows)) (* x (count ex-rows)))
     :cols     (+ (:cols universe) (- (count ex-cols)) (* x (count ex-cols)))
     :galaxies (map (partial expand-galaxy x ex-rows ex-cols) (:galaxies universe))}))

(defn galaxy-pairs [{:keys [galaxies]}]
  (math/combinations galaxies 2))

(defn distance [[galaxy1 galaxy2]]
  (let [[r1 c1] (:location galaxy1)
        [r2 c2] (:location galaxy2)
        dr (abs (- r2 r1))
        dc (abs (- c2 c1))]
    (+ dr dc)))

(defn solution1 [input]
  (->> (read-input input)
       (expand 2)
       galaxy-pairs
       (map distance)
       (apply +)))

(defn solution2 [x input]
  (->> (read-input input)
       (expand x)
       galaxy-pairs
       (map distance)
       (apply +)))
