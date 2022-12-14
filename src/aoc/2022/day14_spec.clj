(ns aoc.2022.day14-spec
  (:require
    [aoc.2022.day14 :as sut]
    [aoc.core :as core]
    [speclj.core :refer :all]))

(def sample
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(describe "2022 Day 14: Regolith Reservoir"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= [[498 4] [498 6] [496 6]] (first result))
      (should= [[503 4] [502 4] [502 9] [494 9]] (second result))))

  (it "->wall"
    (let [result (sut/read-input sample)]
      (should= #{[498 4] [498 5] [498 6] [497 6] [496 6]} (sut/points->wall (first result)))
      (should= #{[503 4] [502 4] [502 5] [502 6] [502 7]
                 [502 8] [502 9] [501 9] [500 9] [499 9]
                 [498 9] [497 9] [496 9] [495 9] [494 9]} (sut/points->wall (second result)))))

  (it "build cave"
    (let [cave (sut/build-cave sample)]
      (should= 20 (count (:walls cave)))
      (should= #{} (:sand cave))
      (should= 11 (:floor cave))))

  (it "drop sand"
    (let [cave (sut/build-cave sample)
          results (iterate sut/drop-sand cave)]
      (should= #{[500 8]} (:sand (nth results 1)))
      (should= #{[500 8][499 8]} (:sand (nth results 2)))
      (should= #{[500 8][499 8][501 8]} (:sand (nth results 3)))
      (should= #{[500 8][499 8][501 8][500 7]} (:sand (nth results 4)))
      (should= #{[500 8][499 8][501 8][500 7][498 8]} (:sand (nth results 5)))))

  (it "solution1"
    (should= 24 (sut/solution1 sample))
    #_(should= 1133 (sut/solution1 (core/input 2022 14))))

  (it "solution2"
    (should= 93 (sut/solution2 sample))
    #_(should= 27566 (sut/solution2 (core/input 2022 14))))

  )
