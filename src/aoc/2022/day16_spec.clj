(ns aoc.2022.day16-spec
  (:require [aoc.2022.day16 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def sample
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(describe "2022 Day 16: Title"

  (it "read-input"
    (let [result (sut/read-input sample)]
      (should= {:rate 0 :tunnels ["DD" "II" "BB"]} (get result "AA"))
      (should= {:rate 13 :tunnels ["CC" "AA"]} (get result "BB"))
      (should= {:rate 21 :tunnels ["II"]} (get result "JJ"))
      (should= 10 (count result))))

  (it "sim options - not opened"
    (let [options (sut/sim-options (sut/read-input sample) sut/start-sim-single)]
      (should= 4 (count options))
      (should-contain {:location "AA" :opened #{"AA"} :relief 0 :actions ["open AA"]} options)
      (should-contain {:location "DD" :opened #{} :relief 0 :actions ["move to DD"]} options)
      (should-contain {:location "II" :opened #{} :relief 0 :actions ["move to II"]} options)
      (should-contain {:location "BB" :opened #{} :relief 0 :actions ["move to BB"]} options)))

  (it "sim options - opened"
    (let [options (sut/sim-options (sut/read-input sample) (update sut/start-sim-single :opened conj "AA"))]
      (should= 3 (count options))
      (should-contain {:location "DD" :opened #{"AA"} :relief 0 :actions ["move to DD"]} options)
      (should-contain {:location "II" :opened #{"AA"} :relief 0 :actions ["move to II"]} options)
      (should-contain {:location "BB" :opened #{"AA"} :relief 0 :actions ["move to BB"]} options)))

  (it "sim options - doubled"
    (let [options (sut/sim-options (sut/read-input sample) sut/start-sim-double)]
      (should= 15 (count options))
      (should-contain {:location1 "AA" :location2 "DD" :opened #{"AA"} :relief 0 :actions ["1 open AA" "2 move to DD"]} options)
      (should-contain {:location1 "AA" :location2 "II" :opened #{"AA"} :relief 0 :actions ["1 open AA" "2 move to II"]} options)
      (should-contain {:location1 "AA" :location2 "BB" :opened #{"AA"} :relief 0 :actions ["1 open AA" "2 move to BB"]} options)
      (should-contain {:location1 "DD" :location2 "AA" :opened #{"AA"} :relief 0 :actions ["1 move to DD" "2 open AA"]} options)
      (should-contain {:location1 "DD" :location2 "DD" :opened #{} :relief 0 :actions ["1 move to DD" "2 move to DD"]} options)
      (should-contain {:location1 "DD" :location2 "II" :opened #{} :relief 0 :actions ["1 move to DD" "2 move to II"]} options)
      (should-contain {:location1 "DD" :location2 "BB" :opened #{} :relief 0 :actions ["1 move to DD" "2 move to BB"]} options)
      (should-contain {:location1 "II" :location2 "AA" :opened #{"AA"} :relief 0 :actions ["1 move to II" "2 open AA"]} options)
      (should-contain {:location1 "II" :location2 "DD" :opened #{} :relief 0 :actions ["1 move to II" "2 move to DD"]} options)
      (should-contain {:location1 "II" :location2 "II" :opened #{} :relief 0 :actions ["1 move to II" "2 move to II"]} options)
      (should-contain {:location1 "II" :location2 "BB" :opened #{} :relief 0 :actions ["1 move to II" "2 move to BB"]} options)
      (should-contain {:location1 "BB" :location2 "AA" :opened #{"AA"} :relief 0 :actions ["1 move to BB" "2 open AA"]} options)
      (should-contain {:location1 "BB" :location2 "DD" :opened #{} :relief 0 :actions ["1 move to BB" "2 move to DD"]} options)
      (should-contain {:location1 "BB" :location2 "II" :opened #{} :relief 0 :actions ["1 move to BB" "2 move to II"]} options)
      (should-contain {:location1 "BB" :location2 "BB" :opened #{} :relief 0 :actions ["1 move to BB" "2 move to BB"]} options)))

  (it "update-relief"
    (let [valves        (sut/read-input sample)
          update-relief (partial sut/update-relief valves)]
      (should= 0 (:relief (update-relief sut/start-sim-single)))
      (should= 0 (:relief (-> (sut/open-valve sut/start-sim-single) update-relief)))
      (should= 13 (:relief (-> (sut/move-to sut/start-sim-single "BB") sut/open-valve update-relief)))
      (should= 26 (:relief (-> (sut/move-to sut/start-sim-single "BB") sut/open-valve update-relief update-relief)))
      (should= 35 (:relief (-> (sut/move-to sut/start-sim-single "BB") sut/open-valve
                               (sut/move-to "HH") sut/open-valve update-relief)))))

  #_(it "solution1"
    (should= 1651 (sut/solution1 sample))
    #_(should= 2183 (sut/solution1 (core/input 2022 16))))

  #_(it "solution2"
    (should= 1707 (sut/solution2 sample))
    (should= 2911 (sut/solution2 (core/input 2022 16))))

  )
