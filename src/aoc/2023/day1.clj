(ns aoc.2023.day1
  (:require [clojure.string :as str]))

(defn read-input [input]
  (str/split-lines input))

(def digits #{#_\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn line->2digit-int [line]
  (let [ds      (filter digits line)
        int-str (str (first ds) (last ds))]
    (Integer/parseInt int-str)))

(defn solution1 [input]
  (->> (read-input input)
       (map line->2digit-int)
       (apply +)))

(defn line->digits [line]
  (loop [line line result []]
    (if (str/blank? line)
      result
      (cond (contains? digits (first line)) (recur (subs line 1) (conj result (Integer/parseInt (str (first line)))))
            ;(str/starts-with? line "zero") (recur (subs line 4) (conj result 0))
            (str/starts-with? line "one") (recur (subs line 1) (conj result 1))
            (str/starts-with? line "two") (recur (subs line 1) (conj result 2))
            (str/starts-with? line "three") (recur (subs line 1) (conj result 3))
            (str/starts-with? line "four") (recur (subs line 1) (conj result 4))
            (str/starts-with? line "five") (recur (subs line 1) (conj result 5))
            (str/starts-with? line "six") (recur (subs line 1) (conj result 6))
            (str/starts-with? line "seven") (recur (subs line 1) (conj result 7))
            (str/starts-with? line "eight") (recur (subs line 1) (conj result 8))
            (str/starts-with? line "nine") (recur (subs line 1) (conj result 9))
            :else (recur (subs line 1) result)))))

(defn calibrate [digits]
  (Integer/parseInt (str (first digits) (last digits))))

(defn solution2 [input]
  (let [lines        (read-input input)
        calibrations (->> lines
                          (map line->digits)
                          (map calibrate))]
    (apply + calibrations)))
