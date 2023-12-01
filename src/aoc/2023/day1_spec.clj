(ns aoc.2023.day1-spec
  (:require [aoc.2023.day1 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str "1abc2\n"
       "pqr3stu8vwx\n"
       "a1b2c3d4e5f\n"
       "treb7uchet"))

(def sample2
  (str "two1nine\n"
       "eightwothree\n"
       "abcone2threexyz\n"
       "xtwone3four\n"
       "4nineeightseven2\n"
       "zoneight234\n"
       "7pqrstsixteen"))

(describe "2023 Day 1: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= "1abc2" (first result))))

  (it "solution 1"
    (should= 142 (sut/solution1 sample))
    (should= 54239 (sut/solution1 (core/input 2023 1))))

  (it "solution 2"
    (should= 281 (sut/solution2 sample2))
    (should= 55343 (sut/solution2 (core/input 2023 1))))

  )
