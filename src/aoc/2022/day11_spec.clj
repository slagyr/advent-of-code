(ns aoc.2022.day11-spec
  (:require [aoc.2022.day11 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(describe "2022 Day 11: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= 4 (count result))
      (should= {:items       [79 98]
                :operation   [* :old 19]
                :test        23
                :pass        2
                :fail        3
                :inspections 0} (first result))
      (should= {:items       [54 65 75 74]
                :operation   [+ :old 6]
                :test        19
                :pass        2
                :fail        0
                :inspections 0} (second result))))


  (it "monkey-round 0"
    (let [monkeys (sut/read-input sample)
          monkeys (sut/monkey-round true monkeys 0)]
      (should= [] (get-in monkeys [0 :items]))
      (should= 2 (get-in monkeys [0 :inspections]))
      (should= [74 500 620] (get-in monkeys [3 :items]))))

  (it "monkey-round 1"
    (let [monkeys (sut/read-input sample)
          monkeys (sut/monkey-round true monkeys 0)
          monkeys (sut/monkey-round true monkeys 1)]
      (should= [] (get-in monkeys [1 :items]))
      (should= [20 23 27 26] (get-in monkeys [0 :items]))
      (should= 4 (get-in monkeys [1 :inspections]))
      (should= [74 500 620] (get-in monkeys [3 :items]))))

  (it "monkey-round 2"
    (let [monkeys (sut/read-input sample)
          monkeys (sut/monkey-round true monkeys 0)
          monkeys (sut/monkey-round true monkeys 1)
          monkeys (sut/monkey-round true monkeys 2)]
      (should= [] (get-in monkeys [2 :items]))
      (should= 3 (get-in monkeys [2 :inspections]))
      (should= [20 23 27 26] (get-in monkeys [0 :items]))
      (should= [2080] (get-in monkeys [1 :items]))
      (should= [74 500 620 1200 3136] (get-in monkeys [3 :items]))))

  (it "round"
    (let [monkeys (sut/round true (sut/read-input sample))]
      (should= [20 23 27 26] (get-in monkeys [0 :items]))
      (should= [2080 25 167 207 401 1046] (get-in monkeys [1 :items]))
      (should= [] (get-in monkeys [2 :items]))
      (should= [] (get-in monkeys [3 :items]))))

  (it "monkey-business"
    (should= 10605 (sut/monkey-business true 20 sample))
    (should= 56350 (sut/monkey-business true 20 (core/input 2022 11))))

  (it "monkey-business 2"
    (should= 2713310158 (sut/monkey-business false 10000 sample))
    (should= 13954061248 (sut/monkey-business false 10000 (core/input 2022 11))))

  )
