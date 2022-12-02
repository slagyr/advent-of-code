(ns aoc.2022.day2
  (:require [clojure.string :as str]))

(def weapon-map {\A :rock
                 \B :paper
                 \C :scissors
                 \X :rock
                 \Y :paper
                 \Z :scissors})

(defn ->weapon [letter] (get weapon-map letter))

(defn read-guide [input]
  (->> (str/split-lines input)
       (map (fn [turn] (remove #(= \space %) turn)))
       (map #(map ->weapon %))))

(def result-map {\X :loss
                 \Y :tie
                 \Z :win})

(defn ->round [[weapon result]] [(->weapon weapon) (get result-map result)])

(defn read-guide-correctly [input]
  (->> (str/split-lines input)
       (map (fn [turn] (remove #(= \space %) turn)))
       (map ->round)))

(def weapon-scores {:rock     1
                    :paper    2
                    :scissors 3})

(defn score-for-weapon [round] (get weapon-scores (second round)))

(defn round-result [round]
  (case round
    [:rock :rock] :tie
    [:rock :paper] :win
    [:rock :scissors] :loss
    [:paper :rock] :loss
    [:paper :paper] :tie
    [:paper :scissors] :win
    [:scissors :rock] :win
    [:scissors :paper] :loss
    [:scissors :scissors] :tie))

(def result-scores {:win 6
                    :tie 3
                    :loss 0})

(defn score-for-result [round] (get result-scores (round-result round)))

(defn score-round [round]
  (+ (score-for-weapon round)
     (score-for-result round)))

(def weapon-result-map
  {[:rock :tie]      :rock
   [:rock :win]      :paper
   [:rock :loss]     :scissors
   [:paper :loss]    :rock
   [:paper :tie]     :paper
   [:paper :win]     :scissors
   [:scissors :win]  :rock
   [:scissors :loss] :paper
   [:scissors :tie]  :scissors})

(defn score-round-correctly [round]
  (let [weapon (get weapon-result-map round)]
    (+ (get weapon-scores weapon)
       (get result-scores (second round)))))

(defn score-rounds [input]
  (->> (read-guide input)
       (map score-round)
       (apply +)))

(defn score-rounds-correctly [input]
  (->> (read-guide-correctly input)
       (map score-round-correctly)
       (apply +)))




