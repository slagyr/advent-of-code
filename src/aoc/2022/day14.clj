(ns aoc.2022.day14
  (:require [aoc.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn ->points [point-strings]
  (map (fn [point-str]
         (let [[a b] (str/split point-str #",")]
           [(core/->int a) (core/->int b)]))
       point-strings))

(defn read-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" -> "))
       (map ->points)))

(defn segment->wall [[[x1 y1] [x2 y2]]]
  (let [dx             (cond (< x1 x2) 1 (< x2 x1) -1 :else 0)
        dy             (cond (< y1 y2) 1 (< y2 y1) -1 :else 0)
        inf-points     (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) [x1 y1])
        segment-points (take-while #(not (= [x2 y2] %)) inf-points)]
    (conj segment-points [x2 y2])))

(defn points->wall [points]
  (let [segments (partition 2 1 points)]
    (set (mapcat segment->wall segments))))

(defn build-cave [input]
  (let [wall-points (read-input input)
        walls       (apply set/union (map points->wall wall-points))
        max-y       (apply max (map second walls))]
    {:walls walls
     :sand  #{}
     :floor (+ max-y 2)}))

(def falling-deltas [[0 1] [-1 1] [1 1]])
(defn falling-options [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) falling-deltas))

(defn hit-floor? [{:keys [floor]} [_ y]] (>= y floor))

(defn resting-position [{:keys [walls sand] :as cave}]
  (loop [grain [500 0]]
    (if-let [step (->> (falling-options grain)
                       (remove walls)
                       (remove sand)
                       (remove (partial hit-floor? cave))
                       first)]
      (recur step)
      grain)))

(defn drop-sand [cave]
  (if-let [grain (resting-position cave)]
    (update cave :sand conj grain)))

(defn sand-touches-floor? [{:keys [sand floor]}] (some #(>= (second %) (dec floor)) sand))
(defn sand-touches-ceiling? [{:keys [sand]}] (some #(<= (second %) 0) sand))

(defn solution1 [input]
  (->> (build-cave input)
       (iterate drop-sand)
       (take-while (complement sand-touches-floor?))
       count
       dec))

(defn solution2 [input]
  (->> (build-cave input)
       (iterate drop-sand)
       (take-while (complement sand-touches-ceiling?))
       count))


