(ns aoc.2022.day4
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn ->assignment-pair [line]
  (let [sections (->> (str/split line #",|\-")
                      (map core/->int))]
    [(take 2 sections)(drop 2 sections)]))

(defn read-assignments [input]
  (->> (str/split-lines input)
       (map ->assignment-pair)))

(defn fully-contains? [[a b] [c d]]
  (and (<= a c) (>= b d)))

(defn redundant? [[assignment1 assignment2]]
  (or (fully-contains? assignment1 assignment2)
      (fully-contains? assignment2 assignment1)))

(defn count-redundant [input]
  (->> (read-assignments input)
       (filter redundant?)
       count))

(defn overlap? [[[a b] [c d]]]
  (or (<= c a d)
      (<= a c b)))

(defn count-overlaps [input]
  (->> (read-assignments input)
       (filter overlap?)
       count))
