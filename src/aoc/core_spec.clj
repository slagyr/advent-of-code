(ns aoc.core-spec
  (:require [aoc.core :as sut]
            [speclj.core :refer :all]))

(describe "core"

  (it "*neighbors-coords"
    (should= [[0 0] [0 1] [0 2]
              [1 0] [1 2]
              [2 0] [2 1] [2 2]] (sut/*neighbor-coords [1 1])))

  (it "+neighbors-coords"
    (should= [[0 1]
              [1 0] [1 2]
              [2 1]] (sut/+neighbor-coords [1 1])))

  )
