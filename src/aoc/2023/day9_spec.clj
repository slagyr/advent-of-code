(ns aoc.2023.day9-spec
  (:require [aoc.2023.day9 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str
    "0 3 6 9 12 15\n"
    "1 3 6 10 15 21\n"
    "10 13 16 21 30 45"))

(describe "2023 Day 9: Mirage Maintenance"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= [0 3 6 9 12 15] (first result))
      (should= [1 3 6 10 15 21] (second result))))

  (it "differences"
    (should= [3 3 3 3 3] (sut/differences [0 3 6 9 12 15])))

  (it "history"
    (should= [[0 3 6 9 12 15]
              [3 3 3 3 3]
              [0 0 0 0]] (sut/history [0 3 6 9 12 15])))

  (it "extrapolate"
    (should= [18 3 0] (sut/extrapolate (sut/history [0 3 6 9 12 15])))
    (should= [28 7 1 0] (sut/extrapolate (sut/history [1 3 6 10 15 21]))))

  (it "solution 1"
    (should= 114 (sut/solution1 sample))
    (should= 1731106378 (sut/solution1 (core/input 2023 9))))

  (it "extrapolate-first"
    (should= [-3 3 0] (sut/extrapolate-first (sut/history [0 3 6 9 12 15])))
    (should= [0 1 1 0] (sut/extrapolate-first (sut/history [1 3 6 10 15 21])))
    (should= [5 5 -2 2 0] (sut/extrapolate-first (sut/history [10 13 16 21 30 45]))))

  (it "solution 2"
      (should= 2 (sut/solution2 sample))
      (should= 1087 (sut/solution2 (core/input 2023 9))))

  )
