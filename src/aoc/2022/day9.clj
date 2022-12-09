(ns aoc.2022.day9
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-input [input]
  (->> (str/split-lines input)
       (map #(vector (first %) (core/->int (apply str (drop 2 %)))))))

(defn step-head [[x y] dir]
  (case dir
    \R [(inc x) y]
    \U [x (inc y)]
    \L [(dec x) y]
    \D [x (dec y)]))

(defn step-tail [[hx hy] [tx ty]]
  (let [dx (- hx tx)
        dy (- hy ty)
        [mx my] (case [dx dy]
                  [0 2] [0 1]
                  [1 2] [1 1]
                  [2 2] [1 1]
                  [2 1] [1 1]
                  [2 0] [1 0]
                  [2 -1] [1 -1]
                  [2 -2] [1 -1]
                  [1 -2] [1 -1]
                  [0 -2] [0 -1]
                  [-1 -2] [-1 -1]
                  [-2 -2] [-1 -1]
                  [-2 -1] [-1 -1]
                  [-2 0] [-1 0]
                  [-2 1] [-1 1]
                  [-2 2] [-1 1]
                  [-1 2] [-1 1]
                  [0 0])]
    [(+ tx mx) (+ ty my)]))

(defn step-one [[head & tails] dir]
  (loop [new-knots [(step-head head dir)] tails tails]
    (if (empty? tails)
      new-knots
      (let [new-knots (conj new-knots (step-tail (last new-knots) (first tails)))]
        (recur new-knots (rest tails))))))

(defn step-many [knots [dir n]]
  (rest (reductions step-one knots (take n (repeat dir)))))

(defn apply-moves [start moves]
  (loop [start start moves moves history []]
    (if (not (seq moves))
      history
      (let [steps (step-many start (first moves))]
        (recur (last steps) (rest moves) (concat history steps))))))

(defn run [knots input]
  (let [moves   (read-input input)
        history (apply-moves knots moves)]
    ;(doseq [h history] (prn h))
    (count (set (map last history)))))

(defn solution1 [input] (run [[0 0][0 0]] input))

(defn solution2 [input] (run (take 10 (repeat [0 0])) input))
