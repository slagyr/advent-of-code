(ns aoc.2023.day10-spec
  (:require [aoc.2023.day10 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample1 (str ".....\n"
                  ".S-7.\n"
                  ".|.|.\n"
                  ".L-J.\n"
                  "....."))

(def sample2 (str "7-F7-\n"
                  ".FJ|7\n"
                  "SJLL7\n"
                  "|F--J\n"
                  "LJ.LJ"))

(def sample3 (str "...........\n"
                  ".S-------7.\n"
                  ".|F-----7|.\n"
                  ".||.....||.\n"
                  ".||.....||.\n"
                  ".|L-7.F-J|.\n"
                  ".|..|.|..|.\n"
                  ".L--J.L--J.\n"
                  "..........."))

(def sample4 (str "..........\n"
                  ".S------7.\n"
                  ".|F----7|.\n"
                  ".||....||.\n"
                  ".||....||.\n"
                  ".|L-7F-J|.\n"
                  ".|..||..|.\n"
                  ".L--JL--J.\n"
                  ".........."))

(def sample5 (str "FF7FSF7F7F7F7F7F---7\n"
                  "L|LJ||||||||||||F--J\n"
                  "FL-7LJLJ||||||LJL-77\n"
                  "F--JF--7||LJLJIF7FJ-\n"
                  "L---JF-JLJIIIIFJLJJ7\n"
                  "|F|F-JF---7IIIL7L|7|\n"
                  "|FFJF7L7F-JF7IIL---7\n"
                  "7-L-JL7||F7|L7F-7F7|\n"
                  "L.L7LFJ|||||FJL7||LJ\n"
                  "L7JLJL-JLJLJL--JLJ.L"))

(describe "2023 Day 10: Pipe Maze"

  (it "read-input"
    (let [result (sut/read-input sample1)]
      (should= [nil nil nil nil nil] (first result))
      (should= [nil \S \- \7 nil] (second result))
      (should= [nil \| nil \| nil] (nth result 2))))

  (it "start"
    (should= [1 1] (sut/start-position (sut/read-input sample1)))
    (should= [2 0] (sut/start-position (sut/read-input sample2))))

  (it "ends"
    (let [pipes1 (sut/read-input sample1)
          pipes2 (sut/read-input sample2)]
      (should= [[1 1] [1 3]] (sut/ends pipes1 [1 2]))       ;; -
      (should= [[1 2] [2 3]] (sut/ends pipes1 [1 3]))       ;; 7
      (should= [[1 3] [3 3]] (sut/ends pipes1 [2 3]))       ;; |
      (should= [[2 3] [3 2]] (sut/ends pipes1 [3 3]))       ;; J
      (should= [[3 1] [3 3]] (sut/ends pipes1 [3 2]))       ;; -
      (should= [[2 1] [3 2]] (sut/ends pipes1 [3 1]))       ;; L
      (should= [[1 1] [3 1]] (sut/ends pipes1 [2 1]))       ;; |
      (should= [[1 2] [2 1]] (sut/ends pipes2 [1 1]))       ;; F
      (should= [[0 1] [1 2] [2 1] [1 0]] (sut/ends pipes1 [1 1])) ;; S
      ))

  (it "connections"
    (let [pipes1 (sut/read-input sample1)]
      (should= [[1 2] [2 1]] (sut/connections pipes1 [1 1]))))

  (it "loop"
    (should= [[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1] [1 1]] (sut/find-loop (sut/read-input sample1)))
    (should= [[2 0] [2 1] [1 1] [1 2] [0 2] [0 3] [1 3] [2 3] [2 4] [3 4] [3 3] [3 2] [3 1] [4 1] [4 0] [3 0] [2 0]]
             (sut/find-loop (sut/read-input sample2)))
    #_(prn (sut/find-loop (sut/read-input sample5))))

  (it "solution 1"
    (should= 4 (sut/solution1 sample1))
    (should= 8 (sut/solution1 sample2))
    (should= 6856 (sut/solution1 (core/input 2023 10))))

  (it "frames"
    (should= [#{[0 0] [0 1] [0 2] [0 3] [0 4] [1 4] [2 4] [3 4] [4 4] [4 3] [4 2] [4 1] [4 0] [3 0] [2 0] [1 0]}
              #{[1 1] [1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1]}
              #{[2 2]}] (sut/frames (sut/read-input sample1)))
    ;; 11 x 9, 9 x 7, 7 x 5, 5 x 3, 3, 1
    (should= [36 28 20 12 3] (map count (sut/frames (sut/read-input sample3)))))

  (it "inside-tile"
    (let [pipes (sut/read-input sample1)]
      (should= [{:dir :right :position [1 3] :inside [2 2] :outside [[1 4] [0 3] [0 4]]}
                {:dir :right :position [3 3] :inside [2 2] :outside [[3 4] [4 3] [4 4]]}
                {:dir :right :position [3 1] :inside [2 2] :outside [[3 0] [4 1] [4 0]]}]
               (sut/turns pipes (sut/find-loop pipes))))
    #_(let [pipes (sut/read-input sample3)]
      (should= :looky (sut/turns pipes (sut/find-loop pipes))))
    )

  (it "solution 2"
    (should= 1 (sut/solution2 sample1))
    (should= 1 (sut/solution2 sample2))
    (should= 4 (sut/solution2 sample3))
    (should= 4 (sut/solution2 sample4))
    (should= 10 (sut/solution2 sample5))
    (should= 501 (sut/solution2 (core/input 2023 10))))

  )
