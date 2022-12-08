(ns aoc.2022.day7-spec
  (:require [aoc.2022.day7 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(describe "2022 Day 7: No Space Left On Device"

  (it "read input"
    (should= {"/" {"a"     {"e"     {"i" 584}
                            "f"     29116
                            "g"     2557
                            "h.lst" 62596}
                   "b.txt" 14848514
                   "c.dat" 8504156
                   "d"     {"j"     4060174
                            "d.log" 8033020
                            "d.ext" 5626152
                            "k"     7214296}}}
             (sut/read-input sample)))

  (it "directory sizes"
    (let [result (sut/directory-sizes (sut/read-input sample))]
      (should= 584 (get result ["/" "a" "e"]))
      (should= 94853 (get result ["/" "a"]))
      (should= 24933642 (get result ["/" "d"]))
      (should= 48381165 (get result ["/"]))))

  (it "solution 1"
    (should= 95437 (sut/solution1 sample))
    (should= 1423358 (sut/solution1 (core/input 2022 7))))

  (it "solution 2"
    (should= 24933642 (sut/solution2 sample))
    (should= 545729 (sut/solution2 (core/input 2022 7))))

  )
