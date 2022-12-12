(ns aoc.2022.day12-spec
  (:require [aoc.2022.day12 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(describe "2022 Day 12: Hill Climbing Algorithm"

  (it "read-input"
    (let [{:keys [width height elevations start end]} (sut/read-input sample)]
      (should= [0 1 2 17 16 15 14 13] (first elevations))
      (should= [1 3 3 19 26 27 24 11]  (nth elevations 2))
      (should= 5 (count elevations))
      (should= 8 width)
      (should= 5 height)
      (should= [0 0] start)
      (should= [2 5] end)))

  (it "neighbors"
    (let [card (sut/read-input sample)]
      (should= [[0 1] [1 0]] (sut/neighbors card [0 0]))
      (should= [[0 6] [1 7]] (sut/neighbors card [0 7]))
      (should= [[0 1] [1 0] [1 2] [2 1]] (sut/neighbors card [1 1]))
      (should= [[0 2] [1 1] [2 2]] (sut/neighbors card [1 2]))))

  (it "distance"
    (should= 0.0 (sut/distance [0 0] [0 0]) 0.01)
    (should= 5.0 (sut/distance [0 0] [0 5]) 0.01)
    (should= 5.0 (sut/distance [0 0] [5 0]) 0.01)
    (should= 7.07 (sut/distance [0 0] [5 5]) 0.01))

  (it "shortest-path"
    (let [card (sut/read-input sample)]
      (should= [[2 4][2 5]] (sut/shortest-path (assoc card :start [2 4])))
      (should= [[1 4] [2 4][2 5]] (sut/shortest-path (assoc card :start [1 4])))))

  (it "solution1"
    (should= 31 (sut/solution1 sample))
    (should= 506 (sut/solution1 (core/input 2022 12))))

)
