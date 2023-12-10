(ns aoc.2023.day10
  (:require [aoc.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn ->pipe [c] (if (= \. c) nil c))

(defn read-input [input]
  (->> (str/split-lines input)
       (mapv #(mapv ->pipe %))))

(defn start-position [pipes]
  (->> (for [row (range (count pipes)) col (range (count (first pipes)))]
         (when (= \S (get-in pipes [row col])) [row col]))
       (filter some?)
       first))

(defn ends [pipes [row col]]
  (case (get-in pipes [row col])
    \| [[(dec row) col] [(inc row) col]]
    \- [[row (dec col)] [row (inc col)]]
    \L [[(dec row) col] [row (inc col)]]
    \J [[(dec row) col] [row (dec col)]]
    \7 [[row (dec col)] [(inc row) col]]
    \F [[row (inc col)] [(inc row) col]]
    \S [[(dec row) col] [row (inc col)] [(inc row) col] [row (dec col)]]
    []))

(defn connections [pipes position]
  (filter (fn [end] (some #(= position %) (ends pipes end)))
          (ends pipes position)))

(defn find-loop [pipes]
  (let [start (start-position pipes)
        curr  (first (connections pipes start))]
    (loop [result [start curr] prev start curr curr]
      (let [next (->> (connections pipes curr) (filter #(not= prev %)) first)]
        (if (= next start)
          (conj result next)
          (recur (conj result next) curr next))))))

(defn solution1 [input]
  (-> (read-input input)
      find-loop
      count
      dec
      (/ 2)))

(defn frames [pipes]
  (let [max-row (count pipes)
        max-col (count (first pipes))]
    (loop [min-row 0 min-col 0 max-row max-row max-col max-col result []]
      (if (or (<= max-row min-row) (<= max-col min-col))
        result
        (let [frame (set (concat (map #(vector min-row %) (range min-col max-col))
                                 (map #(vector (dec max-row) %) (range min-col max-col))
                                 (map #(vector % min-col) (range min-row max-row))
                                 (map #(vector % (dec max-col)) (range min-row max-row))))]
          (recur (inc min-row) (inc min-col) (dec max-row) (dec max-col) (conj result frame)))))))

(defn touches? [tile-set position]
  (some #(contains? tile-set %) (core/+neighbor-coords position)))

(defn turn [pipes [prev cur]]
  (let [c (get-in pipes cur)
        [pr pc] prev
        [cr cc] cur]
    (cond (and (= \F c) (= (inc cr) pr)) {:dir :right :position cur :inside [(inc cr) (inc cc)] :outside [[cr (dec cc)] [(dec cr) cc] [(dec cr) (dec cc)]]}
          (and (= \F c) (= (inc cc) pc)) {:dir :left :position cur :inside [(inc cr) (inc cc)] :outside [[cr (dec cc)] [(dec cr) cc] [(dec cr) (dec cc)]]}
          (and (= \7 c) (= (inc cr) pr)) {:dir :left :position cur :inside [(inc cr) (dec cc)] :outside [[cr (inc cc)] [(dec cr) cc] [(dec cr) (inc cc)]]}
          (and (= \7 c) (= (dec cc) pc)) {:dir :right :position cur :inside [(inc cr) (dec cc)] :outside [[cr (inc cc)] [(dec cr) cc] [(dec cr) (inc cc)]]}
          (and (= \L c) (= (dec cr) pr)) {:dir :left :position cur :inside [(dec cr) (inc cc)] :outside [[cr (dec cc)] [(inc cr) cc] [(inc cr) (dec cc)]]}
          (and (= \L c) (= (inc cc) pc)) {:dir :right :position cur :inside [(dec cr) (inc cc)] :outside [[cr (dec cc)] [(inc cr) cc] [(inc cr) (dec cc)]]}
          (and (= \J c) (= (dec cr) pr)) {:dir :right :position cur :inside [(dec cr) (dec cc)] :outside [[cr (inc cc)] [(inc cr) cc] [(inc cr) (inc cc)]]}
          (and (= \J c) (= (dec cc) pc)) {:dir :left :position cur :inside [(dec cr) (dec cc)] :outside [[cr (inc cc)] [(inc cr) cc] [(inc cr) (inc cc)]]}
          )))

(defn turns [pipes loop-steps]
  (->> (partition 2 1 loop-steps)
       (map (partial turn pipes))
       (filter some?)))

(defn insides [cw? turn]
  (if (and cw? (= :right (:dir turn)))
    [(:inside turn)]
    (:outside turn)))

(defn draw-pipes [pipes loop-set free-tiles enclosed]
  (doseq [row (range (count pipes))]
    (doseq [col (range (count (first pipes)))]
      (let [p [row col]
            c (or (get-in pipes p) \.)]
        (cond (= \S c) (print (core/yellow (str c)))
              (contains? loop-set p) (print (core/magenta (str c)))
              (contains? enclosed p) (print (core/green (str c)))
              (contains? free-tiles p) (print (core/light-gray (str c)))
              :else (print (core/red (str c))))))
    (println)))

(defn solution2 [input]
  (let [pipes          (read-input input)
        loop-steps     (-> pipes find-loop)
        loop-set       (set loop-steps)
        turns          (turns pipes loop-steps)
        cw?            (let [{:keys [right left]} (frequencies (map :dir turns))] (> (or right 0) (or left 0)))
        insides        (mapcat (partial insides cw?) turns)
        insides        (set (remove #(contains? loop-set %) insides))
        all-tiles      (for [row (range (count pipes)) col (range (count (first pipes)))] [row col])
        non-loop-tiles (set (filter #(not (contains? loop-set %)) all-tiles))
        insides        (set/intersection insides non-loop-tiles)
        pending        (set/difference non-loop-tiles insides)
        enclosed       (loop [pending pending enclosed insides]
                         (let [new-free (set (filter (partial touches? enclosed) pending))]
                           (if (seq new-free)
                             (recur (set/difference pending new-free) (set/union enclosed new-free))
                             enclosed)))]
    ;(draw-pipes pipes loop-set insides enclosed)
    (count enclosed)))
