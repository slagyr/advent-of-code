(ns aoc.2023.day8-spec
  (:require [aoc.2023.day8 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample1
  (str
    "RL\n"
    "\n"
    "AAA = (BBB, CCC)\n"
    "BBB = (DDD, EEE)\n"
    "CCC = (ZZZ, GGG)\n"
    "DDD = (DDD, DDD)\n"
    "EEE = (EEE, EEE)\n"
    "GGG = (GGG, GGG)\n"
    "ZZZ = (ZZZ, ZZZ)\n"))

(def sample2
  (str
    "LLR\n"
    "\n"
    "AAA = (BBB, BBB)\n"
    "BBB = (AAA, ZZZ)\n"
    "ZZZ = (ZZZ, ZZZ)"))

(def sample3
  (str "LR\n"
       "\n"
       "11A = (11B, XXX)\n"
       "11B = (XXX, 11Z)\n"
       "11Z = (11B, XXX)\n"
       "22A = (22B, XXX)\n"
       "22B = (22C, 22C)\n"
       "22C = (22Z, 22Z)\n"
       "22Z = (22B, 22B)\n"
       "XXX = (XXX, XXX)"))

(describe "2023 Day 8: Haunted Wasteland"

  (it "read-input"
    (let [result (sut/read-input sample1)]
      (should= [\R \L] (:turns result))
      (should= {\L "BBB" \R "CCC"} (get result "AAA"))
      (should= {\L "DDD" \R "EEE"} (get result "BBB"))))

  (it "step"
    (let [doc (sut/start (sut/read-input sample1))
          step-r (sut/step doc \R)
          step-l (sut/step doc \L)]
      (should= "CCC" (:location step-r))
      (should= 1 (:steps step-r))
      (should= "BBB" (:location step-l))
      (should= 1 (:steps step-l))))

  (it "solution 1"
    (should= 2 (sut/solution1 sample1))
    (should= 6 (sut/solution1 sample2))
    (should= 12643 (sut/solution1 (core/input 2023 8))))

  (it "multi-start"
    (let [doc (sut/multi-start (sut/read-input sample3))]
      (should= ["11A" "22A"] (:locations doc))
      (should= 0 (:steps doc))))

  (it "multi-step"
    (let [doc (sut/multi-start (sut/read-input sample3))
          step1 (sut/multi-step doc \L)
          step2 (sut/multi-step step1 \R)
          step3 (sut/multi-step step2 \L)
          step4 (sut/multi-step step3 \R)
          step5 (sut/multi-step step4 \L)
          step6 (sut/multi-step step5 \R)]
      (should= false (sut/multi-end? doc))
      (should= ["11B" "22B"] (:locations step1))
      (should= 1 (:steps step1))
      (should= false (sut/multi-end? step1))
      (should= ["11Z" "22C"] (:locations step2))
      (should= ["11Z" "22Z"] (:locations step6))
      (should= true (sut/multi-end? step6))))

  (it "solution 2"
    (should= 6 (sut/solution2 sample3))
    (should= [12643 15871 19099 11567 19637 21251] (sut/solution2-parts (core/input 2023 8)))
    ;; LCM 0f those numbers is 13,133,452,426,987
    )

)
