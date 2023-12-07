(ns aoc.2023.day7
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map (fn [[h b]] {:hand (seq h) :bid (core/->long b)}))))

(def jokers-wild? false)

(def card-values {\2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9
                  \T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14})

(defn card-value [card]
  (if (and jokers-wild? (= \J card))
    1
    (get card-values card)))

(defn hand-type [hand]
  (let [freqs  (frequencies hand)
        jokers (if jokers-wild? (get freqs \J 0) 0)
        freqs (if jokers-wild? (dissoc freqs \J) freqs)
        counts (sort (vals freqs))
        m      (+ (if (seq counts) (apply max counts) 0) jokers)]
    (cond (= 5 m) :five-of-a-kind
          (= 4 m) :four-of-a-kind
          (or (= [2 3] counts) (and jokers-wild? (= 3 m) (= [2 2] counts))) :full-house
          (= 3 m) :three-of-a-kind
          (= [1 2 2] counts) :two-pair
          (= 2 m) :one-pair
          :else :high-card)))

(def type-value {:high-card       0
                 :one-pair        1
                 :two-pair        3
                 :three-of-a-kind 4
                 :full-house      5
                 :four-of-a-kind  6
                 :five-of-a-kind  7})

(defn compare-hands [h1 h2]
  (let [type1  (hand-type h1)
        type2  (hand-type h2)
        result (compare (get type-value type1) (get type-value type2))]
    (if (= 0 result)
      (or (->> (partition 2 (interleave h1 h2))
               (map (fn [[c1 c2]] (compare (card-value c1) (card-value c2))))
               (remove zero?)
               first)
          0)
      result)))

(defn compare-hand-bids [hb1 hb2] (compare-hands (:hand hb1) (:hand hb2)))

(defn solution1 [input]
  (->> (read-input input)
       (sort compare-hand-bids)
       (map-indexed (fn [i hb] (assoc hb :rank (inc i))))
       (map (fn [{:keys [bid rank]}] (* bid rank)))
       (apply +)))

(defn solution2 [input]
  (with-redefs [jokers-wild? true]
    (solution1 input)))
