(ns aoc.2022.day3-spec
  (:require [aoc.2022.day3 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsS
PmmdzqPrVvPwwTWBw
wMqvLMZHhHMvwLHjbvcjnnSBnvTQF
ttgJtRGJQctTZtZ
CrZsJsPPZsGzwwsLwLmpwMDw")

(describe "2022 Day 3: Rucksack Reorganization"

  (it "read rucksacks"
    (let [result (sut/read-rucksacks sample)]
      (should= [(seq "vJrwpWtwJgWr") (seq "hcsFMMfFFhFp")] (nth result 0))
      (should= [(seq "jqHRNqRjqzjGDLGL") (seq "rsFMfFZSrLrFZsS")] (nth result 1))
      (should= [(seq "PmmdzqPrV") (seq "vPwwTWBw")] (nth result 2))))

  (it "common-item"
    (let [rucksacks (sut/read-rucksacks sample)]
      (should= \p (sut/common-item (nth rucksacks 0)))
      (should= \L (sut/common-item (nth rucksacks 1)))
      (should= \P (sut/common-item (nth rucksacks 2)))
      (should= \v (sut/common-item (nth rucksacks 3)))
      (should= \t (sut/common-item (nth rucksacks 4)))
      (should= \s (sut/common-item (nth rucksacks 5)))))

  (it "priority"
    (should= 1 (sut/priority \a))
    (should= 26 (sut/priority \z))
    (should= 27 (sut/priority \A))
    (should= 52 (sut/priority \Z)))

  (it "priority-total"
    (should= 157 (sut/priority-total sample))
    (should= 7793 (sut/priority-total (core/input 2022 3))))

  (it "badge type"
    (let [result (sut/read-rucksacks sample)]
      (should= \r (sut/badge-type (take 3 result)))
      (should= \Z (sut/badge-type (drop 3 result)))))

  (it "groups priority total"
    (should= 70 (sut/group-priority-total sample))
    (should= 2499 (sut/group-priority-total (core/input 2022 3))))

  )
