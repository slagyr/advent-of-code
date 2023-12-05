(ns aoc.2023.day5
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn read-mapping [line]
  (let [[destination source range] (map core/->long (str/split line #" "))]
    {:source source :destination destination :range range}))

(defn read-map [maps lines]
  (let [k        (-> (first lines)
                     (str/split #" ")
                     first
                     keyword)
        mappings (map read-mapping (rest lines))]
    (assoc maps k mappings)))

(defn read-input [input]
  (let [lines (str/split-lines input)
        seeds (map core/->long (str/split (subs (first lines) 7) #" "))
        maps  (->> (partition-by str/blank? (rest (rest lines)))
                   (remove #(= [""] %)))]
    (reduce read-map {:seeds seeds} maps)))

(defn mapping-matches? [{:keys [source range]} key]
  (and (>= key source) (< key (+ source range))))

(defn matching-mapping [almanac-map source]
  (first (filter #(mapping-matches? % source) almanac-map)))

(defn almanac-get [almanac-map source]
  (if-let [mapping (matching-mapping almanac-map source)]
    (let [d (- source (:source mapping))]
      (+ (:destination mapping) d))
    source))

(defn seed->location [almanac seed]
  (->> (almanac-get (:seed-to-soil almanac) seed)
       (almanac-get (:soil-to-fertilizer almanac))
       (almanac-get (:fertilizer-to-water almanac))
       (almanac-get (:water-to-light almanac))
       (almanac-get (:light-to-temperature almanac))
       (almanac-get (:temperature-to-humidity almanac))
       (almanac-get (:humidity-to-location almanac))))

(defn solution1 [input]
  (let [almanac        (read-input input)
        seed-locations (reduce #(assoc %1 %2 (seed->location almanac %2)) {} (:seeds almanac))]
    (-> (sort-by second seed-locations)
        first
        second)))

(defn has-seed? [almanac seed]
  (boolean (some (fn [[a b]] (and (>= seed a) (< seed (+ a b)))) (partition 2 (:seeds almanac)))))

(defn reverse-mapping-matches? [{:keys [destination range]} key]
  (and (>= key destination) (< key (+ destination range))))

(defn reverse-matching-mapping [almanac-map k]
  (first (filter #(reverse-mapping-matches? % k) almanac-map)))

(defn reverse-almanac-get [almanac-map k]
  (if-let [mapping (reverse-matching-mapping almanac-map k)]
    (let [d (- k (:destination mapping))]
      (+ (:source mapping) d))
    k))

(defn location->seed [almanac location]
  (->> (reverse-almanac-get (:humidity-to-location almanac) location)
       (reverse-almanac-get (:temperature-to-humidity almanac))
       (reverse-almanac-get (:light-to-temperature almanac))
       (reverse-almanac-get (:water-to-light almanac))
       (reverse-almanac-get (:fertilizer-to-water almanac))
       (reverse-almanac-get (:soil-to-fertilizer almanac))
       (reverse-almanac-get (:seed-to-soil almanac))))

(defn solution2 [input]
  (let [almanac (read-input input)]
    (->> (range)
         (filter #(has-seed? almanac (location->seed almanac %)))
         first)))
