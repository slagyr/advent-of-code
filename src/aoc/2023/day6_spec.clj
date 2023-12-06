(ns aoc.2023.day6-spec
  (:require [aoc.2023.day6 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str
    "Time:      7  15   30\n"
    "Distance:  9  40  200"))

(describe "2023 Day 6: Wait For It"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:time 7 :distance 9} (first result))
      (should= {:time 15 :distance 40} (second result))
      (should= {:time 30 :distance 200} (last result))))

  (it "wins"
    (should= [2 3 4 5] (sut/wins {:time 7 :distance 9}))
    (should= [4 5 6 7 8 9 10 11] (sut/wins {:time 15 :distance 40}))
    (should= (range 11 20) (sut/wins {:time 30 :distance 200})))

  (it "solution 1"
    (should= 288 (sut/solution1 sample))
    (should= 74698 (sut/solution1 (core/input 2023 6))))

  (it "->one-race"
    (should= {:time 71530 :distance 940200} (sut/->one-race (sut/read-input sample))))

  (it "solution 2"
    (should= 71503 (sut/solution2 sample))
    #_(should= 27563421 (sut/solution2 (core/input 2023 6))))

  )
