(ns aoc.2023.day8
  (:require [clojure.string :as str]))

(defn add-node [result line]
  (let [[node turns] (str/split line #"=")
        node  (str/trim node)
        left  (subs turns 2 5)
        right (subs turns 7 10)]
    (assoc result node {\L left \R right})))

(defn read-input [input]
  (let [lines (str/split-lines input)
        turns (seq (first lines))]
    (reduce add-node {:turns turns} (drop 2 lines))))

(defn start [doc]
  (assoc doc :location "AAA" :steps 0))

(defn step [{:keys [location] :as doc} turn]
  (let [node (get-in doc [location turn])]
    (-> (assoc doc :location node)
        (update :steps inc))))

(defn solution1 [input]
  (let [doc (start (read-input input))]
    (->> (reductions step doc (cycle (:turns doc)))
         (filter #(= "ZZZ" (:location %)))
         first
         :steps)))

(defn multi-start [doc]
  (let [locations (filter #(and (string? %) (= \A (last %))) (keys doc))]
    (assoc doc :locations (sort locations) :steps 0)))

(defn multi-end? [doc]
  (every? #(= \Z (last %)) (:locations doc)))

(defn multi-step [doc turn]
  (let [locations (mapv #(get-in doc [% turn]) (:locations doc))]
    (-> (assoc doc :locations locations)
        (update :steps inc))))

(defn solution2 [input]
  (let [doc (multi-start (read-input input))]
    (->> (reductions multi-step doc (cycle (:turns doc)))
         (filter multi-end?)
         first
         :steps)))

(defn steps-to-z [doc]
  (->> (reductions step doc (cycle (:turns doc)))
       (filter #(= \Z (last (:location %))))
       first
       :steps))

(defn solution2-parts [input]
  (let [doc    (multi-start (read-input input))
        starts (map #(assoc doc :location %) (:locations doc))]
    (map steps-to-z starts)))
