(ns aoc.2022.day6-spec
  (:require [aoc.2022.day6 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(describe "2022 Day 6: Tuning Trouble"

  (it "start-of-packet"
    (should= 7 (sut/start-of-packet "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
    (should= 5 (sut/start-of-packet "bvwbjplbgvbhsrlpgdmjqwftvncz"))
    (should= 1210 (sut/start-of-packet (core/input 2022 6))))

  (it "start-of-message"
    (should= 19 (sut/start-of-message "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
    (should= 23 (sut/start-of-message "bvwbjplbgvbhsrlpgdmjqwftvncz"))
    (should= 3476 (sut/start-of-message (core/input 2022 6))))

)
