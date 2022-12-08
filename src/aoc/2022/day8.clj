(ns aoc.2022.day8
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-heights [line]
  (mapv #(core/->int (str %)) line))

(defn read-input [input]
  (let [trees (mapv read-heights (str/split-lines input))]
    {:trees trees
     :rows  (count trees)
     :cols  (count (first trees))}))

(defn hidden-by? [tree cover-trees]
  (let [tallest-cover-tree (apply max cover-trees)]
    (>= tallest-cover-tree tree)))

(defn trees-up [{:keys [trees]} r c] (map #(get-in trees [% c]) (range 0 r)))
(defn trees-down [{:keys [trees rows]} r c] (map #(get-in trees [% c]) (range (inc r) rows)))
(defn trees-left [{:keys [trees]} r c] (map #(get-in trees [r %]) (range 0 c)))
(defn trees-right [{:keys [trees cols]} r c] (map #(get-in trees [r %]) (range (inc c) cols)))

(defn visible? [{:keys [trees rows cols] :as grid} r c]
  (let [tree (get-in trees [r c])]
    (or (= 0 r)
        (= 0 c)
        (= (dec rows) r)
        (= (dec cols) c)
        (not (hidden-by? tree (trees-up grid r c)))
        (not (hidden-by? tree (trees-down grid r c)))
        (not (hidden-by? tree (trees-left grid r c)))
        (not (hidden-by? tree (trees-right grid r c))))))

(defn solution1 [input]
  (let [{:keys [rows cols] :as grid} (read-input input)]
    (->> (for [r (range 0 rows) c (range 0 cols)]
           (visible? grid r c))
         (filter true?)
         count)))

(defn visual-range [tree tree-line]
  (loop [tree-line tree-line n 0]
    (if (not (seq tree-line))
      n
      (if (< (first tree-line) tree)
        (recur (rest tree-line) (inc n))
        (inc n)))))

(defn scenic-score [{:keys [trees] :as grid} r c]
  (let [tree (get-in trees [r c])]
    (* (visual-range tree (reverse (trees-up grid r c)))
       (visual-range tree (trees-down grid r c))
       (visual-range tree (reverse (trees-left grid r c)))
       (visual-range tree (trees-right grid r c)))))

(defn solution2 [input]
  (let [{:keys [rows cols] :as grid} (read-input input)]
    (->> (for [r (range 0 rows) c (range 0 cols)]
           (scenic-score grid r c))
         (apply max))))
