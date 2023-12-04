(ns aoc.2023.day4-spec
  (:require [aoc.2023.day4 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(describe "2023 Day 4: Scratchcards"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:card 1 :winning [41 48 83 86 17] :have [83 86 6 31 17 9 48 53]} (first result))))

  (it "card-points"
    (let [cards (sut/read-input sample)]
      (should= 8 (sut/card-points (first cards)))
      (should= 2 (sut/card-points (nth cards 1)))
      (should= 2 (sut/card-points (nth cards 2)))
      (should= 1 (sut/card-points (nth cards 3)))
      (should= 0 (sut/card-points (nth cards 4)))
      (should= 0 (sut/card-points (nth cards 5)))))

  (it "solution 1"
    (should= 13 (sut/solution1 sample))
    (should= 18653 (sut/solution1 (core/input 2023 4))))

  (it "solution 2"
    (should= 30 (sut/solution2 sample))
    (should= 5921508 (sut/solution2 (core/input 2023 4))))

  )
