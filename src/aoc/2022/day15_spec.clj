(ns aoc.2022.day15-spec
  (:require [aoc.2022.day15 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(describe "2022 Day 15: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:sensor [2 18] :beacon [-2 15]} (nth result 0))
      (should= {:sensor [9 16] :beacon [10 16]} (nth result 1))))

  (it "manhattan-distance"
    (should= 0 (sut/manhattan-distance [0 0] [0 0]))
    (should= 0 (sut/manhattan-distance [99 123] [99 123]))
    (should= 1 (sut/manhattan-distance [0 0] [0 1]))
    (should= 1 (sut/manhattan-distance [0 0] [1 0]))
    (should= 2 (sut/manhattan-distance [0 0] [1 1]))
    (should= 198 (sut/manhattan-distance [0 0] [99 99])))

  (it "range-"
    (should= nil (sut/range- [3 10] [3 13]))
    (should= nil (sut/range- [0 4] [-5 5]))
    (should= [[6 10]] (sut/range- [6 10] [-5 5]))
    (should= [[6 10]] (sut/range- [0 10] [0 5])))

  (it "subtract-range"
    (should= [[0 10]] (sut/subtract-range [[0 10]] [99 100]))
    (should= [[0 10]] (sut/subtract-range [[0 10]] [-5 0]))
    (should= [[0 4]] (sut/subtract-range [[0 10]] [5 10]))
    (should= [[6 10]] (sut/subtract-range [[0 10]] [0 5]))
    (should= [[0 1] [9 10]] (sut/subtract-range [[0 10]] [2 8]))
    (should= [[0 2] [8 10]] (sut/subtract-range [[0 4] [6 10]] [3 7]))
    (should= [[6 10]] (sut/subtract-range [[0 4] [6 10]] [-5 5]))
    (should= [[0 1] [3 20]] (sut/subtract-range [[0 20]] [2 2]))
    (should= [[0 1] [3 10] [14 20]] (sut/subtract-range [[0 1] [3 20]] [11 13]))
    (should= [[0 1] [14 20]] (sut/subtract-range [[0 1] [3 10] [14 20]] [3 13])))

  (it "solution1"
    (should= 26 (sut/solution1 sample 10))
    #_(should= 5607466 (sut/solution1 (core/input 2022 15) 2000000)))

  (it "solution2"
    (should= 56000011 (sut/solution2 sample 20))
    #_(should= 12543202766584 (sut/solution2 (core/input 2022 15) 4000000)))

  )
