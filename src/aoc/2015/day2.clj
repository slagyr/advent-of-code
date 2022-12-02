(ns aoc.2015.day2
  (:require [clojure.string :as str]))

(defn ->dimensions [line]
  (->> (str/split line #"x")
       (map #(Integer/parseInt %))))

(defn read-sizes [input]
  (->> (str/split-lines input)
       (map ->dimensions)))

(defn paper-needed [[x y z]]
  (let [sides [(* x y) (* y z) (* x z)]
        slack (apply min sides)]
    (+ (* 2 (apply + sides))
       slack)))

(defn total-paper [input]
  (->> (read-sizes input)
       (map paper-needed)
       (apply +)))

(defn ribbon-needed [[x y z]]
  (+ (min (+ x x y y)
          (+ y y z z)
          (+ z z x x))
     (* x y z)))

(defn total-ribbon [input]
  (->> (read-sizes input)
       (map ribbon-needed)
       (apply +)))
