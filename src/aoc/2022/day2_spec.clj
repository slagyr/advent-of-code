(ns aoc.2022.day2-spec
  (:require [aoc.2022.day2 :as sut]
            [speclj.core :refer :all]))

(def sample "A Y\nB X\nC Z")

(describe "Day 2: Rock Paper Scissors"

  (it "read guide"
    (let [result (sut/read-guide sample)]
      (should= [:rock :paper] (nth result 0))
      (should= [:paper :rock] (nth result 1))
      (should= [:scissors :scissors] (nth result 2))))

  (it "score round"
    (should= 8 (sut/score-round [:rock :paper]))
    (should= 1 (sut/score-round [:paper :rock]))
    (should= 6 (sut/score-round [:scissors :scissors])))

  (it "score total"
    (should= 15 (sut/score-rounds sample))
    (should= 10624 (sut/score-rounds (slurp "src/aoc/2022/day2-input.txt"))))

  (it "read guide correctly"
    (let [result (sut/read-guide-correctly sample)]
      (should= [:rock :tie] (nth result 0))
      (should= [:paper :loss] (nth result 1))
      (should= [:scissors :win] (nth result 2))))

  (it "score round correctly"
    (should= 4 (sut/score-round-correctly [:rock :tie]))
    (should= 1 (sut/score-round-correctly [:paper :loss]))
    (should= 7 (sut/score-round-correctly [:scissors :win])))

  (it "correct score total"
    (should= 12 (sut/score-rounds-correctly sample))
    (should= 14060 (sut/score-rounds-correctly (slurp "src/aoc/2022/day2-input.txt"))))

  )
