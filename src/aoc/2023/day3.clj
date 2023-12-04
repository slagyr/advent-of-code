(ns aoc.2023.day3
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def digit-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def neighbor-deltas [[-1 -1] [-1 0] [-1 1]
                      [0 -1] [0 1]
                      [1 -1] [1 0] [1 1]])

(defn neighbor-coords [[row col]]
  (for [[dr dc] neighbor-deltas]
    [(+ row dr) (+ col dc)]))

(def new-part {:kind :part-number :coordinates [] :buffer []})

(defn add-part-number [part n coord]
  (-> (update part :buffer conj n)
      (update :coordinates conj coord)))

(defn close-part [part]
  (-> (assoc part :number (Integer/parseInt (apply str (:buffer part))))
      (dissoc :buffer)))

(defn maybe-add-part [result part]
  (if (seq (:buffer part))
    (conj result (close-part part))
    result))

(defn read-input [input]
  (let [lines (mapv vec (str/split-lines input))
        max-r (dec (count lines))
        max-c (dec (count (first lines)))]
    (loop [row 0 col 0 part new-part result []]
      (let [c (get-in lines [row col])]
        (cond (> row max-r) result
              (> col max-c) (recur (inc row) 0 new-part (maybe-add-part result part))
              (contains? digit-chars c) (recur row (inc col) (add-part-number part c [row col]) result)
              :else (let [result (maybe-add-part result part)]
                      (if (not= \. c)
                        (recur row (inc col) new-part (conj result {:kind :symbol :char c :coordinates [[row col]]}))
                        (recur row (inc col) new-part result))))))))

(defn optimize-part [part]
  (-> (update part :coordinates set)
      (assoc :neighbors (set (mapcat neighbor-coords (:coordinates part))))))

(defn read-input-optimized [input]
  (map optimize-part (read-input input)))

(defn adjacent? [part1 part2]
  (-> (set/intersection (:neighbors part1) (:coordinates part2))
      seq
      boolean))

(defn valid-part-number? [non-numbers part]
  (and (= :part-number (:kind part))
       (some #(adjacent? part %) non-numbers)))

(defn solution1 [input]
  (let [parts        (read-input-optimized input)
        non-numbers  (remove #(= :part-number (:kind %)) parts)
        part-numbers (filterv (partial valid-part-number? non-numbers) parts)]
    (->> (mapv :number part-numbers)
         (apply +))))

(defn gear-numbers [parts part]
  (when (= \* (:char part))
    (->> (filter #(and (= :part-number (:kind %)) (adjacent? part %)) parts)
         (map :number))))

(defn solution2 [input]
  (let [parts        (read-input-optimized input)
        gears  (->> (map #(gear-numbers parts %) parts)
                    (remove nil?)
                    (filter #(= 2 (count %))))
        ratios (map (fn [[a b]] (* a b)) gears)]
    (apply + ratios)))
