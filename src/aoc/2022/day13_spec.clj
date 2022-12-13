(ns aoc.2022.day13-spec
  (:require [aoc.2022.day13 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(describe "2022 Day 13: Distress Signal"

  (it "read input"
    (let [result (sut/read-input sample)]
      (should= [[1 1 3 1 1] [1 1 5 1 1]] (first result))
      (should= [[[1] [2 3 4]] [[1] 4]] (second result))
      (should= 8 (count result))))

  (it "in-order?"
    (let [result (sut/read-input sample)]
      (should= true (sut/in-order? (nth result 0)))
      (should= true (sut/in-order? (nth result 1)))
      (should= false (sut/in-order? (nth result 2)))
      (should= true (sut/in-order? (nth result 3)))
      (should= false (sut/in-order? (nth result 4)))
      (should= true (sut/in-order? (nth result 5)))
      (should= false (sut/in-order? (nth result 6)))
      (should= false (sut/in-order? (nth result 7)))))

  (it "solution1"
    (should= 13 (sut/solution1 sample))
    (should= 5905 (sut/solution1 (core/input 2022 13))))

  (it "sorted"
    (should= [[]
              [[]]
              [[[]]]
              [1, 1, 3, 1, 1]
              [1, 1, 5, 1, 1]
              [[1], [2, 3, 4]]
              [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
              [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
              [[1], 4]
              [[2]]
              [3]
              [[4, 4], 4, 4]
              [[4, 4], 4, 4, 4]
              [[6]]
              [7, 7, 7]
              [7, 7, 7, 7]
              [[8, 7, 6]]
              [9]] (sut/sort-packets (apply concat (sut/read-input sample)))))

  (it "solution2"
    (should= 140 (sut/solution2 sample))
    (should= 21691 (sut/solution2 (core/input 2022 13))))

  )
