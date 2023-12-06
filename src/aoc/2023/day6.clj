(ns aoc.2023.day6
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-input [input]
  (let [[time-line dist-line] (str/split-lines input)
        times (->> (str/split time-line #"\s+")
                   rest
                   (map core/->int))
        dists (->> (str/split dist-line #"\s+")
                   rest
                   (map core/->int))]
    (->> (interleave times dists)
         (partition 2)
         (map (fn [[time dist]] {:time time :distance dist})))))

(defn hold-for [time hold]
  (* hold (- time hold)))

(defn win? [{:keys [time distance]} hold]
  (> (hold-for time hold) distance))

(defn wins [race]
  (filter #(win? race %) (range 0 (inc (:time race)))))

(defn solution1 [input]
  (->> (read-input input)
       (map wins)
       (map count)
       (apply *)))

(defn ->one-race [races]
  (let [times (map :time races)
        dists (map :distance races)]
    {:time (core/->long (apply str times))
     :distance (core/->long (apply str dists))}))

(defn solution2 [input]
  (let [race (->one-race (read-input input))
        low (first (wins race))
        high (first (filter #(win? race %) (range (:time race) low -1)))]
    (inc (- high low))))
