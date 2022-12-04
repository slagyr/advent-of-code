(ns aoc.2022.day4-spec
  (:require [aoc.2022.day4 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(describe "2022 Day 4: Title"

  (it "read assignments"
    (let [result (sut/read-assignments sample)]
      (should= [[2 4][6 8]] (first result))
      (should= [[2 3][4 5]] (second result))))

  (it "redundant?"
    (let [result (sut/read-assignments sample)]
      (should= false (sut/redundant? (nth result 0)))
      (should= false (sut/redundant? (nth result 1)))
      (should= false (sut/redundant? (nth result 2)))
      (should= true (sut/redundant? (nth result 3)))
      (should= true (sut/redundant? (nth result 4)))
      (should= false (sut/redundant? (nth result 5)))))

  (it "count-redundant"
    (should= 2 (sut/count-redundant sample))
    (should= 433 (sut/count-redundant (core/input 2022 4))))

  (it "overlap?"
    (let [result (sut/read-assignments sample)]
      (should= false (sut/overlap? (nth result 0)))
      (should= false (sut/overlap? (nth result 1)))
      (should= true (sut/overlap? (nth result 2)))
      (should= true (sut/overlap? (nth result 3)))
      (should= true (sut/overlap? (nth result 4)))
      (should= true (sut/overlap? (nth result 5)))))

  (it "count-overlaps"
    (should= 4 (sut/count-overlaps sample))
    (should= 852 (sut/count-overlaps (core/input 2022 4))))

)
