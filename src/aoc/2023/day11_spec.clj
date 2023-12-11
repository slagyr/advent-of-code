(ns aoc.2023.day11-spec
  (:require [aoc.2023.day11 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str
    "...#......\n"
    ".......#..\n"
    "#.........\n"
    "..........\n"
    "......#...\n"
    ".#........\n"
    ".........#\n"
    "..........\n"
    ".......#..\n"
    "#...#....."))

(describe "2023 Day 11: Cosmic Expansion"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= 10 (:rows result))
      (should= 10 (:cols result))
      (should= {:id 1 :location [0 3]} (first (:galaxies result)))
      (should= {:id 2 :location [1 7]} (second (:galaxies result)))))

  (it "empty cols"
    (let [universe (sut/read-input sample)]
      (should= [2 5 8] (sut/empty-cols universe))))

  (it "empty rows"
    (let [universe (sut/read-input sample)]
      (should= [3 7] (sut/empty-rows universe))))

  (it "expand"
    (let [universe (sut/expand 2 (sut/read-input sample))
          galaxies (:galaxies universe)]
      (should= 13 (:cols universe))
      (should= 12 (:rows universe))
      (should= [0 4] (:location (first galaxies)))
      (should= [1 9] (:location (second galaxies)))
      (should= [2 0] (:location (nth galaxies 2)))
      (should= [5 8] (:location (nth galaxies 3)))))

  (it "expand 10"
    (let [universe (sut/expand 10 (sut/read-input sample))
          galaxies (:galaxies universe)]
      (should= 37 (:cols universe))
      (should= 28 (:rows universe))
      (should= [0 12] (:location (first galaxies)))
      (should= [1 25] (:location (second galaxies)))
      (should= [2 0] (:location (nth galaxies 2)))
      (should= [13 24] (:location (nth galaxies 3)))))

  (it "pairs"
    (let [universe (sut/expand 2 (sut/read-input sample))
          galaxies (:galaxies universe)
          pairs    (sut/galaxy-pairs universe)]
      (should= 36 (count pairs))
      (should-contain [(first galaxies) (second galaxies)] pairs)))

  (it "distance"
    (let [universe (sut/expand 2 (sut/read-input sample))
          galaxies (:galaxies universe)]
      (should= 6 (sut/distance [(nth galaxies 0) (nth galaxies 1)]))
      (should= 9 (sut/distance [(nth galaxies 4) (nth galaxies 8)]))))

  (it "solution 1"
    (should= 374 (sut/solution1 sample))
    (should= 9536038 (sut/solution1 (core/input 2023 11))))

  (it "solution 2"
    (should= 374 (sut/solution2 2 sample))
    (should= 1030 (sut/solution2 10 sample))
    (should= 8410 (sut/solution2 100 sample))
    (should= 447744640566 (sut/solution2 1000000 (core/input 2023 11))))

  )
