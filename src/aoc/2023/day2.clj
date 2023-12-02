(ns aoc.2023.day2
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn ->draw-seq [draw]
  (let [draw (str/trim draw)
        n-colors (str/split draw #",")
        n-colors-pairs (map #(str/split (str/trim %) #" ") n-colors)]
    (reduce (fn [result [n color]] (assoc result (keyword color) (Integer/parseInt n))) {} n-colors-pairs)))

(defn read-game [line]
  (let [[id-part draw-part] (str/split line #":")
        id (Integer/parseInt (subs id-part 5))
        draws (str/split draw-part #";")
        draws-seq (map ->draw-seq draws)]
    {:id id :draws draws-seq}))

(defn valid-draw? [draw]
  (and (let [red (:red draw)] (or (nil? red) (<= red 12)))
       (let [green (:green draw)] (or (nil? green) (<= green 13)))
       (let [blue (:blue draw)] (or (nil? blue) (<= blue 14)))))

(defn valid-game? [game]
  (every? valid-draw? (:draws game)))

(defn read-input [input]
  (->> (str/split-lines input)
       (map read-game)))

(defn solution1 [input]
  (->> (read-input input)
       (filter valid-game?)
       (map :id)
       (apply +)))

(defn power [game]
  (let [draws (map #(merge {:red 0 :green 0 :blue 0} %) (:draws game))
        red-m (apply max (map :red draws))
        green-m (apply max (map :green draws))
        blue-m (apply max (map :blue draws))]
    (apply * (remove zero? [red-m green-m blue-m]))))

(defn solution2 [input]
  (->> (read-input input)
       (map power)
       (apply +)))
