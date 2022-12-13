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

(defn ->int [s] (Integer/parseInt s))
(defn ->long [s] (Long/parseLong s))

(defn color-text [color-code text] (str "\033[0;" color-code "m" text "\033[0m"))
(defn black [text] (color-text 30 text))
(defn red [text] (color-text 31 text))
(defn green [text] (color-text 32 text))
(defn yellow [text] (color-text 33 text))
(defn blue [text] (color-text 34 text))
(defn magenta [text] (color-text 35 text))
(defn cyan [text] (color-text 36 text))
(defn light-gray [text] (color-text 37 text))
(defn gray [text] (color-text 90 text))
(defn light-red [text] (color-text 91 text))
(defn light-green [text] (color-text 92 text))
(defn light-yellow [text] (color-text 93 text))
(defn light-blue [text] (color-text 94 text))
(defn light-magenta [text] (color-text 95 text))
(defn light-cyan [text] (color-text 96 text))
(defn white [text] (color-text 97 text))
