(ns aoc.2022.day17-spec
  (:require [aoc.2022.day17 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(def _ false)
(def t true)

(def sample ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(describe "2022 Day 17: Pyroclastic Flow"

  (it "read-input"
    (let [result (sut/->jets sample)]
      (should= \> (first result))
      (should= \> (nth result 1))
      (should= \> (nth result 2))
      (should= \< (nth result 3))
      (should= \> (nth result 41))
      (should= \> (nth result 41))
      (should= \> (nth result 42))
      (should= \< (nth result 43))))

  (it "shifting"
    (should= [[2 0] [1 1] [2 1] [3 1] [2 2]] (sut/shift-right sut/rock+))
    (should= [[0 0] [-1 1] [0 1] [1 1] [0 2]] (sut/shift-left sut/rock+))
    (should= [[1 -1] [0 0] [1 0] [2 0] [1 1]] (sut/shift-down sut/rock+)))

  (it "moves rock"
    (should= [[6 5] [5 6] [6 6] [7 6] [6 7]] (sut/move-to sut/rock+ [5 5])))

  (it "hit-wall?"
    (should= true (-> sut/rock- sut/shift-left sut/hit-wall?))
    (should= false (sut/hit-wall? sut/rock-))
    (should= false (-> sut/rock- sut/shift-right sut/hit-wall?))
    (should= false (-> sut/rock- sut/shift-right sut/shift-right sut/hit-wall?))
    (should= false (-> sut/rock- sut/shift-right sut/shift-right sut/shift-right sut/hit-wall?))
    (should= true (-> sut/rock- sut/shift-right sut/shift-right sut/shift-right sut/shift-right sut/hit-wall?)))

  (it "start-state"
    (let [state (sut/start-state sample)]
      (should= [\> \> \> \<] (take 4 (:jets state)))
      (should= 0 (:top state))
      (should= 0 (:drops state))
      (should= (sut/move-to sut/rock- [2 3]) (:rock state))
      (should= [] (:pile state))))

  (it "tick 1"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 1)]
      (should= [\> \> \< \<] (take 4 (:jets state)))
      (should= 0 (:top state))
      (should= 0 (:drops state))
      (should= [[3 2] [4 2] [5 2] [6 2]] (:rock state))
      (should= [] (:pile state))))

  (it "tick 2"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 2)]
      (should= [\> \< \< \>] (take 4 (:jets state)))
      (should= 0 (:top state))
      (should= [[3 1] [4 1] [5 1] [6 1]] (:rock state))
      (should= [] (:pile state))))

  (it "tick 3"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 3)]
      (should= 0 (:top state))
      (should= [[3 0] [4 0] [5 0] [6 0]] (:rock state))
      (should= [] (:pile state))))

  (it "tick 4"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 4)]
      ;(sut/print-chamber state)
      (should= 1 (:top state))
      (should= 1 (:drops state))
      (should= [[3 4] [2 5] [3 5] [4 5] [3 6]] (:rock state))
      (should= [[_ _ t t t t _]] (:pile state))))

  (it "tick 8"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 8)]
      (should= 4 (:top state))
      (should= 2 (:drops state))
      (should= (sut/move-to sut/rock> [2 7]) (:rock state))
      (should= [[_ _ _ t _ _ _]
                [_ _ t t t _ _]
                [_ _ _ t _ _ _]
                [_ _ t t t t _]] (reverse (:pile state)))))

  (it "tick 14"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 13)]
      ;(sut/print-chamber state)
      (should= 6 (:top state))
      (should= 3 (:drops state))
      (should= (sut/move-to sut/rock| [2 9]) (:rock state))
      (should= [[_ _ t _ _ _ _]
                [_ _ t _ _ _ _]
                [t t t t _ _ _]
                [_ _ t t t _ _]
                [_ _ _ t _ _ _]
                [_ _ t t t t _]] (reverse (:pile state)))))

  (it "tick 52"
    (let [states (iterate sut/tick (sut/start-state sample))
          state  (nth states 52)]
      ;(sut/print-chamber state)
      (should= 17 (:top state))))

  #_(it "more sim"
      (doall (->> (sut/start-state sample)
                  (iterate sut/tick)
                  (take 100)
                  (map-indexed (fn [i s] (println "  " i) (sut/print-chamber s))))))

  (it "solution 1"
    (should= 3068 (sut/solution1 sample))
    (should= 3219 (sut/solution1 (core/input 2022 17))))

  ;(it "finds a pattern"
  ;  (let [states (->> (sut/start-state sample)
  ;                    (iterate sut/tick))
  ;        state (->> (drop-while #(> 1000 (:top %)) states) first)
  ;        pile  (:pile state)]
  ;    ;(sut/print-chamber state)
  ;    (let [[start end] (sut/find-pattern pile)
  ;          len       (- end start)
  ;          start-state (->> states
  ;                         (filter #(= start (count (:pile %))))
  ;                         first)
  ;          end-state (->> states
  ;                         (filter #(= end (count (:pile %))))
  ;                         first)
  ;          start-drops (:drops start-state)
  ;          end-drops (:drops end-state)
  ;          drops-per-repeat (- end-drops start-drops)
  ;          repeats (int (/ (- 2022 start-drops) drops-per-repeat))
  ;          tail-drops (rem (- 2022 start-drops) drops-per-repeat)
  ;          state-after-tail-drops (->> states
  ;                                    (filter #(= (+ start-drops tail-drops) (:drops %)))
  ;                                    first)
  ;          tail-size (- (count (:pile state-after-tail-drops)) start)
  ;          answer (+ start tail-size (* len repeats))]
  ;      (prn "start-drops: " start-drops)
  ;      (prn "repeats: " repeats)
  ;      (prn "tail-drops: " tail-drops)
  ;      (prn "tail-size: " tail-size)
  ;      (prn "answer: " answer)
  ;      )))

  (it "solution 2"
    #_(should= 3068 (sut/solution2 sample 2022) 1000)
    #_(should= 1514285714288 (sut/solution2 sample 1000000000000 1000))
    #_(should= 3219 (sut/solution2 (core/input 2022 17) 2022 10000))
    #_(should= 1582758620701 (sut/solution2 (core/input 2022 17) 1000000000000 10000)))

  )
