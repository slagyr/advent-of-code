(ns aoc.2022.day16
  (:require
    [aoc.core :as core]
    [clojure.string :as str]))

(defn parse-valve [valves line]
  (let [[left right] (str/split line #"; ")
        key         (subs left 6 8)
        rate        (core/->int (subs left 23))
        next-valves (rest (str/split right #", "))
        next-valves (if (str/starts-with? right "tunnels")
                      (cons (subs right 23 25) next-valves)
                      (cons (subs right 22 24) next-valves))]
    (assoc valves key {:rate rate :tunnels (vec next-valves)})))

(defn read-input [input]
  (->> (str/split-lines input)
       (reduce parse-valve {})))

(def start-sim-single {:location "AA" :opened #{} :relief 0 :actions []})
(def start-sim-double {:location1 "AA" :location2 "AA" :opened #{} :relief 0 :actions []})

(defn move-to
  ([sim new-location] (move-to sim new-location nil))
  ([sim new-location n]
   (let [key (if n (keyword (str "location" n)) :location)]
     (-> (assoc sim key new-location)
         (update :actions conj (str (if n (str n " ") "") "move to " new-location))))))

(defn open-valve
  ([sim] (open-valve sim nil))
  ([sim n]
   (let [key      (if n (keyword (str "location" n)) :location)
         location (get sim key)]
     (-> (update sim :opened conj location)
         (update :actions conj (str (if n (str n " ") "") "open " location))))))

(defn act [sim option n]
  (if (= :open option)
    (open-valve sim n)
    (move-to sim option n)))

(defn sim-options [valves {:keys [opened] :as sim}]
  (if-let [location (:location sim)]
    (let [move-options (map (partial move-to sim) (get-in valves [location :tunnels]))]
      (if (contains? opened location)
        move-options
        (conj move-options (open-valve sim))))
    (let [{:keys [location1 location2]} sim
          options1 (get-in valves [location1 :tunnels])
          options1 (if (contains? opened location1) options1 (cons :open options1))
          options2 (get-in valves [location2 :tunnels])
          options2 (if (contains? opened location2) options2 (cons :open options2))]
      (remove nil?
              (for [option1 options1 option2 options2]
                (if (and (= location1 location2) (= :open option1) (= :open option2))
                  nil
                  (-> sim (act option1 1) (act option2 2))))))))

(defn update-relief [valves {:keys [opened] :as sim}]
  (let [rates        (map #(get-in valves [% :rate]) opened)
        extra-relief (or (apply + rates) 0)]
    (update sim :relief + extra-relief)))

(def sim-cutoff 20000)

(defn breadth-first [valves start-sim minutes]
  (loop [minutes minutes sims [start-sim]]
    (prn "minutes (count sims): " minutes (count sims))
    (if (= 1 minutes)
      (->> (map (partial update-relief valves) sims) (sort-by :relief) last)
      (let [sims (map (partial update-relief valves) sims)
            sims (->> (sort-by :relief sims) reverse (take sim-cutoff))]
        (recur (dec minutes) (mapcat (partial sim-options valves) sims))))))

(defn solution1 [input]
  (let [valves   (read-input input)
        best-sim (breadth-first valves start-sim-single 30)]
    ;(prn "best-sim: " best-sim)
    (:relief best-sim)))

(defn solution2 [input]
  (let [valves   (read-input input)
        best-sim (breadth-first valves start-sim-double 26)]
    ;(prn "best-sim: " best-sim)
    (:relief best-sim)))
