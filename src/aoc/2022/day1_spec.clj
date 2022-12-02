(ns aoc.2022.day1-spec
  (:require [aoc.2022.day1 :as sut]
            [speclj.core :refer :all]))

(def sample "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(describe "Day 1: Calorie Counting"

  (it "read snacks"
    (let [result (sut/read-snacks sample)]
      (should= {:snacks [1000 2000 3000] :total 6000} (nth result 0))
      (should= {:snacks [4000] :total 4000} (nth result 1))
      (should= {:snacks [5000 6000] :total 11000} (nth result 2))
      (should= {:snacks [7000 8000 9000] :total 24000} (nth result 3))
      (should= {:snacks [10000] :total 10000} (nth result 4))))

  (it "max calories"
    (should= 24000 (sut/max-calories sample))
    (should= 72511 (sut/max-calories (slurp "src/aoc/2022/day1-input.txt"))))

  (it "top 3 calories"
    (should= 45000 (sut/top-3-calories sample))
    (should= 212117 (sut/top-3-calories (slurp "src/aoc/2022/day1-input.txt"))))

  )
