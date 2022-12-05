(ns aoc.2022.day5-spec
  (:require [aoc.2022.day5 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(describe "2022 Day 5: Title"

  (it "read input"
    (let [[stacks moves] (sut/read-input sample)]
      (should= [\N \Z] (first stacks))
      (should= [\D \C \M] (second stacks))
      (should= [\P] (nth stacks 2))
      (should= [1 1 0] (nth moves 0))
      (should= [3 0 2] (nth moves 1))
      (should= [2 1 0] (nth moves 2))
      (should= [1 0 1] (nth moves 3))))

  (it "move crates"
    (let [[stacks moves] (sut/read-input sample)
          results (doall (reductions (partial sut/move-crates false) stacks moves))]
      (should= [[\D \N \Z][\C \M][\P]] (nth results 1))
      (should= [[][\C \M][\Z \N \D \P]] (nth results 2))
      (should= [[\M \C][][\Z \N \D \P]] (nth results 3))
      (should= [[\C][\M][\Z \N \D \P]] (nth results 4))))

  (it "move multiple crates"
    (let [[stacks moves] (sut/read-input sample)
          results (doall (reductions (partial sut/move-crates true) stacks moves))]
      (should= [[\D \N \Z][\C \M][\P]] (nth results 1))
      (should= [[][\C \M][\D \N \Z \P]] (nth results 2))
      (should= [[\C \M][][\D \N \Z \P]] (nth results 3))
      (should= [[\M][\C][\D \N \Z \P]] (nth results 4))))

  (it "resulting-tops"
    (should= "CMZ" (sut/resulting-tops false sample))
    (should= "QNHWJVJZW" (sut/resulting-tops false (core/input 2022 5))))

  (it "resulting-tops multi"
    (should= "MCD" (sut/resulting-tops true sample))
    (should= "BPCZJLFJW" (sut/resulting-tops true (core/input 2022 5))))
  )
