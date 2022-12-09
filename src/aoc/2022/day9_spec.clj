(ns aoc.2022.day9-spec
  (:require [aoc.2022.day9 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def sample2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(describe "2022 Day 9: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= [\R 4] (first result))
      (should= [\U 4] (second result))))

  (it "head-step"
    (should= [1 0] (sut/step-head [0 0] \R))
    (should= [-1 0] (sut/step-head [0 0] \L))
    (should= [0 1] (sut/step-head [0 0] \U))
    (should= [0 -1] (sut/step-head [0 0] \D)))

  (it "step-tail"
    (should= [0 0] (sut/step-tail [1 0] [0 0]))
    (should= [1 0] (sut/step-tail [2 0] [0 0]))
    (should= [1 1] (sut/step-tail [2 1] [0 0]))
    (should= [1 1] (sut/step-tail [2 2] [0 0])))

  (it "step one"
    (should= [[1 0] [0 0]] (sut/step-one [[0 0] [0 0]] \R))
    (should= [[1 0] [0 0] [0 0]] (sut/step-one [[0 0] [0 0] [0 0]] \R)))

  (it "step many"
    (should= [[[1 0] [0 0]]
              [[2 0] [1 0]]
              [[3 0] [2 0]]
              [[4 0] [3 0]]]
             (sut/step-many [[0 0] [0 0]] [\R 4]))
    (should= [[[1 0] [0 0] [0 0]]
              [[2 0] [1 0] [0 0]]
              [[3 0] [2 0] [1 0]]
              [[4 0] [3 0] [2 0]]]
             (sut/step-many [[0 0] [0 0] [0 0]] [\R 4])))

  (it "solution 1"
    (should= 13 (sut/solution1 sample))
    #_(should= 6209 (sut/solution1 (core/input 2022 9))))

  (it "solution2"
    (should= 1 (sut/solution2 sample))
    (should= 36 (sut/solution2 sample2))
    #_(should= 2460 (sut/solution2 (core/input 2022 9))))

  )
