(ns aoc.2022.day10
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn ->cmd [line]
  (if (str/starts-with? line "addx")
    ["addx" (core/->int (subs line 5))]
    ["noop"]))

(defn read-input [input]
  (->> (str/split-lines input)
       (map ->cmd)))

(defmulti process-cmd (fn [register [cmd & _]] cmd))
(defmethod process-cmd "noop" [register _]
  [register])

(defmethod process-cmd "addx" [register [_ n]]
  [register (+ register n)])

(defn cycles [register commands]
  (loop [commands commands cycles [register]]
    (if (empty? commands)
      cycles
      (let [new-cycles (process-cmd (last cycles) (first commands))]
        (recur (rest commands) (concat cycles new-cycles))))))

(defn during-cycle [cycles n]
  (* n (nth cycles (dec n))))

(defn solution1 [input]
  (let [commands (read-input input)
        results  (cycles 1 commands)
        steps    (rest (range -20 221 40))
        values   (map (partial during-cycle results) steps)]
    (apply + values)))

(defn ->row [signals]
  (apply str
         (map-indexed
           (fn [n signal]
             (if (#{-1 0 1} (- n signal))
               "#"
               "."))
           signals)))

(defn crt [input]
  (let [cycles (cycles 1 (read-input input))
        rows   (mapv ->row (partition 40 cycles))]
    (str/join "\n" rows)))
