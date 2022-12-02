(ns aoc.core)

(defn ->inspect
  ([v] (prn "inspect: " v) v)
  ([v comment] (prn comment ": " v) v))

(defn ->>inspect
  ([v] (prn "inspect: " v) v)
  ([comment v] (prn comment ": " v) v))

(defn input [year day]
  (let [filename (format "src/aoc/%d/day%d-input.txt" year day)]
    (slurp filename)))
