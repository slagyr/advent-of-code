(ns aoc.2023.day3-spec
  (:require [aoc.2023.day3 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str
    "467..114..\n"
    "...*......\n"
    "..35..633.\n"
    "......#...\n"
    "617*......\n"
    ".....+.58.\n"
    "..592.....\n"
    "......755.\n"
    "...$.*....\n"
    ".664.598.."))

(describe "2023 Day 3: Gear Ratios"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:kind :part-number :number 467 :coordinates [[0 0] [0 1] [0 2]]} (first result))
      (should= {:kind :part-number :number 114 :coordinates [[0 5] [0 6] [0 7]]} (second result))
      (should= {:kind :symbol :char \* :coordinates [[1 3]]} (nth result 2))
      (should= #{:part-number :symbol} (set (map :kind result)))))

  (it "adjacent?"
    (let [parts (sut/read-input-optimized sample)
          [p1 p2 p3] parts]
      (should= true (sut/adjacent? p1 p3))
      (should= false (sut/adjacent? p2 p3))))

  (it "neighbors-coords"
    (should= [[0 0] [0 1] [0 2]
              [1 0] [1 2]
              [2 0] [2 1] [2 2]] (sut/neighbor-coords [1 1])))

  (it "solution 1"
    (should= 4361 (sut/solution1 sample))
    (should= 525119 (sut/solution1 (core/input 2023 3))))

  (it "gear-numbers"
    (let [parts (sut/read-input-optimized sample)]
      (should= [467 35] (sut/gear-numbers parts (nth parts 2)))
      (should= nil (sut/gear-numbers parts (nth parts 5)))
      (should= [617] (sut/gear-numbers parts (nth parts 7)))))

  (it "solution 2"
      (should= 467835 (sut/solution2 sample))
      (should= 76504829 (sut/solution2 (core/input 2023 3))))

  )
