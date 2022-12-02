(ns aoc.new
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn establish-path
  "Create any missing directories in path"
  [path]
  (let [file   (io/file path)
        parent (.getParentFile file)]
    (when (not (.exists parent))
      (.mkdirs parent))))


(defn usage []
  (println "Generate files for a new day.")
  (println "Usage:")
  (println "  clj -Mnew <year> <day>"))

(defn parse-args [args]
  (try
    [(Integer/parseInt (first args))
     (Integer/parseInt (second args))]
    (catch Exception e
      (println (str e))
      (usage)
      nil)))

(def src-template
  "(ns aoc.<year>.day<day>
  (:require [aoc.core :as core]))")

(def spec-template
  "(ns aoc.<year>.day<day>-spec
   (:require [aoc.<year>.day<day> :as sut]
             [speclj.core :refer :all]))

(describe \"<year> Day <day>: Title\"

  (it \"something\"
    (should= 1 2))

)")

(defn populate [template year day]
  (-> (str/replace template "<year>" (str year))
      (str/replace "<day>" (str day))))

(defn -main [& args]
  (when-let [[year day] (parse-args args)]
    (println (format "Generating files for year %d, day %d" year day))
    (let [src-file   (format "src/aoc/%d/day%d.clj" year day)
          spec-file  (format "src/aoc/%d/day%d_spec.clj" year day)
          input-file (format "src/aoc/%d/day%d-input.txt" year day)
          src        (populate src-template year day)
          spec       (populate spec-template year day)]
      (establish-path src-file)
      (println "\t writing " src-file)
      (spit src-file src)
      (println "\t writing " spec-file)
      (spit spec-file spec)
      (println "\t writing " input-file)
      (spit input-file ""))))
