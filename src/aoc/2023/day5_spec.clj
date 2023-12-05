(ns aoc.2023.day5-spec
  (:require [aoc.2023.day5 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  (str
    "seeds: 79 14 55 13\n"
    "\n"
    "seed-to-soil map:\n"
    "50 98 2\n"
    "52 50 48\n"
    "\n"
    "soil-to-fertilizer map:\n"
    "0 15 37\n"
    "37 52 2\n"
    "39 0 15\n"
    "\n"
    "fertilizer-to-water map:\n"
    "49 53 8\n"
    "0 11 42\n"
    "42 0 7\n"
    "57 7 4\n"
    "\n"
    "water-to-light map:\n"
    "88 18 7\n"
    "18 25 70\n"
    "\n"
    "light-to-temperature map:\n"
    "45 77 23\n"
    "81 45 19\n"
    "68 64 13\n"
    "\n"
    "temperature-to-humidity map:\n"
    "0 69 1\n"
    "1 0 69\n"
    "\n"
    "humidity-to-location map:\n"
    "60 56 37\n"
    "56 93 4"))

(describe "2023 Day 5: If You Give A Seed A Fertilizer"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= [79 14 55 13] (:seeds result))
      (should= [{:source 98 :destination 50 :range 2}
                {:source 50 :destination 52 :range 48}] (:seed-to-soil result))
      (should-contain :soil-to-fertilizer result)
      (should-contain :fertilizer-to-water result)
      (should-contain :water-to-light result)
      (should-contain :light-to-temperature result)
      (should-contain :temperature-to-humidity result)
      (should-contain :humidity-to-location result)))

  (it "almanac-get"
    (let [almanac      (sut/read-input sample)
          seed-to-soil (:seed-to-soil almanac)]
      (should= 0 (sut/almanac-get seed-to-soil 0))
      (should= 6 (sut/almanac-get seed-to-soil 6))
      (should= 47 (sut/almanac-get seed-to-soil 47))
      (should= 52 (sut/almanac-get seed-to-soil 50))
      (should= 99 (sut/almanac-get seed-to-soil 97))
      (should= 50 (sut/almanac-get seed-to-soil 98))
      (should= 51 (sut/almanac-get seed-to-soil 99))))

  (it "seed->location"
    (let [almanac (sut/read-input sample)]
      (should= 82 (sut/seed->location almanac 79))
      (should= 43 (sut/seed->location almanac 14))
      (should= 86 (sut/seed->location almanac 55))
      (should= 35 (sut/seed->location almanac 13))))

  (it "solution 1"
    (should= 35 (sut/solution1 sample))
    (should= 836040384 (sut/solution1 (core/input 2023 5))))

  (it "has-seed?"
    (let [almanac (sut/read-input sample)]
      (should= false (sut/has-seed? almanac 0))
      (should= false (sut/has-seed? almanac 54))
      (should= true (sut/has-seed? almanac 55))
      (should= true (sut/has-seed? almanac 67))
      (should= true (sut/has-seed? almanac 79))
      (should= true (sut/has-seed? almanac 92))
      (should= false (sut/has-seed? almanac 93))))

  (it "location->seed"
    (let [almanac (sut/read-input sample)]
      (should= 79 (sut/location->seed almanac 82))
      (should= 14 (sut/location->seed almanac 43))
      (should= 55 (sut/location->seed almanac 86))
      (should= 13 (sut/location->seed almanac 35))))

  (it "solution 2"
    (should= 46 (sut/solution2 sample))
    #_(should= 10834440 (sut/solution2 (core/input 2023 5))))

  )
