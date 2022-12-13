(ns aoc.2022.day13
  (:require
    [aoc.core :as core]
    [clojure.string :as str])
  (:import (java.util Comparator)))

(defn read-input [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map read-string)
       (partition 2)))

(defn ->type [x]
  (cond (int? x) :int
        (vector? x) :vec
        :else :bad))

(defmulti in-order? (fn [[left right]] [(->type left) (->type right)]))

(defmethod in-order? [:int :int] [[left right]]
  (cond (< left right) true
        (> left right) false
        :else nil))

(defmethod in-order? [:vec :vec] [[left right]]
  (loop [left left right right]
    (let [l (first left) r (first right)]
      (cond (and (nil? l) (some? r)) true
            (and (some? l) (nil? r)) false
            (and (nil? l) (nil? r)) nil
            :else (let [result (in-order? [l r])]
                    (if (nil? result)
                      (recur (rest left) (rest right))
                      result))))))

(defmethod in-order? [:vec :int] [[left right]] (in-order? [left [right]]))
(defmethod in-order? [:int :vec] [[left right]] (in-order? [[left] right]))

(deftype PacketComparer []
  Comparator
  (compare [_ left right]
    (case (in-order? [left right])
      true -1
      false 1
      0)))

(defn solution1 [input]
  (->> (read-input input)
       (map in-order?)
       (map-indexed (fn [i o?] (when o? (inc i))))
       (remove nil?)
       (apply +)))

(def dividers #{[[2]] [[6]]})

(defn sort-packets [packets]
  (sort-by identity (PacketComparer.) (concat packets [[[2]] [[6]]])))

(defn solution2 [input]
  (->> (read-input input)
       (apply concat)
       (sort-packets)
       (map-indexed (fn [i p] (when (dividers p) (inc i))))
       (remove nil?)
       (apply *)))
