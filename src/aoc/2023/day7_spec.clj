(ns aoc.2023.day7-spec
  (:require [aoc.2023.day7 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str "32T3K 765\n"
       "T55J5 684\n"
       "KK677 28\n"
       "KTJJT 220\n"
       "QQQJA 483"))

(describe "2023 Day 7: Camel Cards"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:hand [\3 \2 \T \3 \K] :bid 765} (first result))
      (should= {:hand [\T \5 \5 \J \5] :bid 684} (second result))))

  (it "compare cards"
    (should= [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]
             (sort-by sut/card-value (shuffle [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A])))
    (with-redefs [sut/jokers-wild? true]
      (should= [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A]
               (sort-by sut/card-value (shuffle [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A])))))

  (it "hand-type"
    (should= :high-card (sut/hand-type [\2 \3 \4 \5 \6]))
    (should= :one-pair (sut/hand-type [\2 \2 \4 \5 \6]))
    (should= :one-pair (sut/hand-type [\2 \3 \4 \5 \5]))
    (should= :two-pair (sut/hand-type [\2 \2 \4 \5 \5]))
    (should= :three-of-a-kind (sut/hand-type [\2 \2 \2 \5 \6]))
    (should= :full-house (sut/hand-type [\2 \2 \2 \5 \5]))
    (should= :four-of-a-kind (sut/hand-type [\2 \2 \2 \2 \5]))
    (should= :five-of-a-kind (sut/hand-type [\2 \2 \2 \2 \2])))

  (it "hand-type jokers-wild"
    (with-redefs [sut/jokers-wild? true]
      (should= :high-card (sut/hand-type [\2 \3 \4 \5 \6]))
      (should= :one-pair (sut/hand-type [\2 \3 \4 \5 \J]))
      (should= :three-of-a-kind (sut/hand-type [\2 \3 \4 \J \J]))
      (should= :full-house (sut/hand-type [\2 \2 \4 \4 \J]))
      (should= :full-house (sut/hand-type [\2 \2 \4 \4 \4]))
      (should= :four-of-a-kind (sut/hand-type [\2 \3 \J \J \J]))
      (should= :five-of-a-kind (sut/hand-type [\2 \2 \J \J \J]))
      (should= :five-of-a-kind (sut/hand-type [\2 \J \J \J \J]))
      ))

  (it "hand sorting"
    (let [hands  (map :hand (sut/read-input sample))
          result (sort sut/compare-hands hands)]
      (should= [[\3 \2 \T \3 \K]
                [\K \T \J \J \T]
                [\K \K \6 \7 \7]
                [\T \5 \5 \J \5]
                [\Q \Q \Q \J \A]] result)))

  (it "solution 1"
    (should= 6440 (sut/solution1 sample))
    (should= 250474325 (sut/solution1 (core/input 2023 7))))

  (it "solution 2"
    (should= 5905 (sut/solution2 sample))
    (should= 248909434 (sut/solution2 (core/input 2023 7))))

  )
