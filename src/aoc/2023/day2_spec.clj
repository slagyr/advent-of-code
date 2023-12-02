(ns aoc.2023.day2-spec
  (:require [aoc.2023.day2 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n"
       "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n"
       "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n"
       "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n"
       "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(describe "2023 Day 2: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:id 1 :draws [{:blue 3 :red 4}
                              {:red 1 :green 2 :blue 6}
                              {:green 2}]}
               (first result))))

  (it "valid?"
    (should= true (sut/valid-draw? {:red 12 :green 13 :blue 14}))
    (should= true (sut/valid-draw? {:red 0 :green 0 :blue 0}))
    (should= false (sut/valid-draw? {:red 13 :green 13 :blue 14}))
    (should= false (sut/valid-draw? {:red 12 :green 14 :blue 14}))
    (should= false (sut/valid-draw? {:red 12 :green 13 :blue 15})))


  (it "solution 1"
    (should= 8 (sut/solution1 sample))
    (should= 2256 (sut/solution1 (core/input 2023 2))))

  (it "powers"
    (should= [48 12 1560 630 36] (map sut/power (sut/read-input sample))))

  (it "solution 2"
    (should= 2286 (sut/solution2 sample))
    (should= 74229 (sut/solution2 (core/input 2023 2))))

  )
