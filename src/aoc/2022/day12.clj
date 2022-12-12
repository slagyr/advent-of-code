(ns aoc.2022.day12
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(defn ->elevation [c]
  (cond (= \S c) 0
        (= \E c) 27
        :else (- (int c) 96)))

(defn ->terrain [line] (mapv ->elevation line))

(defn elevation-at [elevations [x y]]
  (get-in elevations [x y]))

(defn find-elevation [elevations width height e]
  (->> (for [x (range 0 height) y (range 0 width)] [x y])
       (filter #(= e (elevation-at elevations %)))
       first))

(defn read-input [input]
  (let [elevations (mapv ->terrain (str/split-lines input))
        width      (count (first elevations))
        height     (count elevations)]
    {:elevations elevations
     :width      width
     :height     height
     :start      (find-elevation elevations width height 0)
     :end        (find-elevation elevations width height 27)}))

(def neighbor-deltas [[-1 0] [0 -1] [0 1] [1 0]])

(defn valid-neighbor? [{:keys [width height elevations]} elevation [x y]]
  (and (< -1 x height)
       (< -1 y width)
       (<= (elevation-at elevations [x y]) (inc elevation))))

(defn neighbors [{:keys [elevations] :as card} [x y]]
  (let [possible-neighbors (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) neighbor-deltas)]
    (filter (partial valid-neighbor? card (elevation-at elevations [x y])) possible-neighbors)))

(defn my-shortest-path
  ([{:keys [start] :as card}] (my-shortest-path card [start] #{start} nil))
  ([{:keys [end] :as card} path visited best]
   (let [location (last path)]
     (if (= location end)
       path
       (let [steps   (remove visited (neighbors card location))
             visited (set/union visited (set steps))]
         (loop [steps steps best best]
           (if (empty? steps)
             best
             (let [path (my-shortest-path card (conj path (first steps)) visited best)]
               (if (and path (or (nil? best) (< (count path) (count best))))
                 (recur (rest steps) path)
                 (recur (rest steps) best))))))))))

(defn distance [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn reconstruct-path [came-from current]
  (reverse
    (loop [path [current] current current]
      #_(when (not (= (count path) (count (set path))))
          (throw (Exception. (pr-str "path cycle: " current path came-from))))
      (if-let [previous (came-from current)]
        (recur (conj path previous) previous)
        path))))

(defn a* [{:keys [start end] :as card}]
  (loop [open-set #{start} came-from {} best {start 0} guess {start (distance start end)} current nil next-steps []]
    (if (seq next-steps)
      (let [step           (first next-steps)
            tentative-best (+ (best current) 1)
            best-for-step  (best step)]
        (if (or (nil? best-for-step) (< tentative-best best-for-step))
          (recur (conj open-set step) (assoc came-from step current) (assoc best step tentative-best)
                 (assoc guess step (+ tentative-best (distance step end))) current (rest next-steps))
          (recur open-set came-from best guess current (rest next-steps))))
      (if (empty? open-set)
        (throw (Exception. "no path found"))
        (let [current (first (sort-by guess open-set))]
          (if (= current end)
            (reconstruct-path came-from current)
            (recur (disj open-set current) came-from best guess current (neighbors card current))))))))

(defn add-steps [card visited path]
  (map #(conj path %) (remove visited (neighbors card (last path)))))

(defn breadth-first [{:keys [start end] :as card}]
  (loop [paths [[start]] visited #{start}]
    (let [new-paths (mapcat #(add-steps card visited %) paths)
          new-steps (set (map last new-paths))
          new-paths (map (fn [step] (first (filter #(= step (last %)) new-paths))) new-steps)
          visited   (set/union visited new-steps)]
      (if-let [path (first (filter #(= end (last %)) new-paths))]
        path
        (recur new-paths visited)))))

;(def shortest-path my-shortest-path)
;(def shortest-path a*)
(def shortest-path breadth-first)

(defn print-path [{:keys [width height elevations]} path]
  (let [trail (set path)]
    (prn "(count path): " (count path))
    (prn "(count trail): " (count trail))
    (doseq [row (range 0 height)]
      (doseq [col (range 0 width)]
        (let [e (elevation-at elevations [row col])
              c (cond (= 0 e) \S (= 27 e) \E :else (char (+ e 96)))]
          (if (trail [row col])
            (print (str "\033[0;32m" c "\033[0m"))
            (print c))))
      (print "\n"))))

(defn solution1 [input]
  (let [card (read-input input)
        path (shortest-path card)]
    ;(print-path card path)
    (dec (count path))))
