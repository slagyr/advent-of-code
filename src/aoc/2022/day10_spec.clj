(ns aoc.2022.day10-spec
  (:require [aoc.2022.day10 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(describe "2022 Day 10: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= ["addx" 15] (first result))
      (should= ["addx" -11] (second result))
      (should= ["noop"] (nth result 9))))

  (it "cycles"
    (let [result (sut/cycles 1 (sut/read-input sample))]
      (should= [1 1 16 16 5 5 11 11 8 8 13 13 12 12 4 4 17 17 21 21 21 20 20]
               (take 23 result))
      (should= 21 (nth result 19))
      (should= 19 (nth result 59))
      (should= 18 (nth result 99))
      (should= 21 (nth result 139))
      (should= 16 (nth result 179))
      (should= 18 (nth result 219))))

  (it "solution 1"
    (should= 13140 (sut/solution1 sample))
    (should= 14780 (sut/solution1 (core/input 2022 10))))

  (it "crt"
    (should=
      "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
      (sut/crt sample))
    ;; ELPLZGZL
    (should=
      "####.#....###..#....####..##..####.#....
#....#....#..#.#.......#.#..#....#.#....
###..#....#..#.#......#..#......#..#....
#....#....###..#.....#...#.##..#...#....
#....#....#....#....#....#..#.#....#....
####.####.#....####.####..###.####.####."
      (sut/crt (core/input 2022 10))))

  )
