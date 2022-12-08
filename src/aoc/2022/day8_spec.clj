(ns aoc.2022.day8-spec
  (:require [aoc.2022.day8 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "30373
25512
65332
33549
35390")

(describe "2022 Day 8: Treetop Tree House"

  (it "read input"
    (let [result (sut/read-input sample)]
      (should= {:trees [[3 0 3 7 3]
                        [2 5 5 1 2]
                        [6 5 3 3 2]
                        [3 3 5 4 9]
                        [3 5 3 9 0]]
                :rows 5
                :cols 5}
                result)))

  (it "visible?"
    (let [grid (sut/read-input sample)]
      (should= true (sut/visible? grid 0 0))
      (should= true (sut/visible? grid 0 1))
      (should= true (sut/visible? grid 1 0))
      (should= true (sut/visible? grid 4 4))
      (should= true (sut/visible? grid 4 3))
      (should= true (sut/visible? grid 3 4))
      (should= false (sut/visible? grid 3 3))
      (should= false (sut/visible? grid 2 2))
      (should= false (sut/visible? grid 3 1))))

  (it "solution1"
    (should= 21 (sut/solution1 sample))
    #_(should= 1798 (sut/solution1 (core/input 2022 8))))

  (it "scenic-score"
    (let [grid (sut/read-input sample)]
      (should= 4 (sut/scenic-score grid 1 2))
      (should= 8 (sut/scenic-score grid 3 2))))

  (it "solution2"
    (should= 8 (sut/solution2 sample))
    #_(should= 259308 (sut/solution2 (core/input 2022 8))))
  )
