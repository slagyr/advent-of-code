(ns aoc.2022.day17)

(defn ->jets [input] (cycle (seq input)))

(def rock- [[0 0] [1 0] [2 0] [3 0]])
(def rock+ [[1 0] [0 1] [1 1] [2 1] [1 2]])
(def rock> [[0 0] [1 0] [2 0] [2 1] [2 2]])
(def rock| [[0 0] [0 1] [0 2] [0 3]])
(def rock# [[0 0] [1 0] [0 1] [1 1]])
(defn rocks [] (cycle [rock- rock+ rock> rock| rock#]))

(defn shift-right [rock] (map (fn [[x y]] [(inc x) y]) rock))
(defn shift-left [rock] (map (fn [[x y]] [(dec x) y]) rock))
(defn shift-down [rock] (map (fn [[x y]] [x (dec y)]) rock))
(defn move-to [rock [sx sy]] (map (fn [[x y]] [(+ x sx) (+ y sy)]) rock))

(defn hit-wall? [rock]
  (let [xs (map first rock)]
    (or (> 0 (apply min xs))
        (< 6 (apply max xs)))))

(defn top-of [rock] (apply max (map second rock)))
(defn bottom-of [rock] (apply min (map second rock)))

(def empty-row [false false false false false false false])

(defn add-rock-to-pile [pile rock]
  (reduce (fn [pile [x y]]
            (let [pile (if (>= y (count pile)) (conj pile empty-row) pile)]
              (assoc-in pile [y x] true)))
          pile rock))

(defn new-rock [{:keys [rocks rock pile top drops] :as state}]
  (let [new-top (max top (if rock (inc (top-of rock)) 0))]
    (merge state {:rocks (rest rocks)
                  :rock  (move-to (first rocks) [2 (+ new-top 3)])
                  :top   new-top
                  :pile  (add-rock-to-pile pile rock)
                  :drops (inc drops)})))

(defn start-state [input]
  (new-rock
    {:jets  (->jets input)
     :rocks (rocks)
     :rock  nil
     :top   0
     :pile  []
     :drops -1}))

(defn hit-pile? [pile rock] (some (fn [[x y]] (get-in pile [y x])) rock))

(defn jet [{:keys [rock jets pile] :as state}]
  (let [shifted (if (= \< (first jets)) (shift-left rock) (shift-right rock))]
    (if (or (hit-wall? shifted) (hit-pile? pile shifted))
      (assoc state :rock rock :jets (rest jets))
      (assoc state :rock shifted :jets (rest jets)))))

(defn fall [{:keys [rock pile top] :as state}]
  (let [fallen (shift-down rock)
        bottom (bottom-of fallen)]
    (if (or (< bottom 0)
            (and (< bottom top) (hit-pile? pile fallen)))
      (new-rock state)
      (assoc state :rock fallen))))

(defn tick [state]
  (-> state
      jet
      fall))

(defn print-chamber [{:keys [rock pile top] :as state}]
  (let [rock  (set rock)
        max-y (max (top-of rock) top)]
    (doseq [y (range max-y 0 -1)]
      (let [row (for [x (range 0 7)]
                  (cond (rock [x y]) "@"
                        (get-in pile [y x]) "#"
                        :else "."))]
        (println (str "|" (apply str row) "| " y))))
    (println "+-------+")
    (println))
  state)

(defn solution1 [input]
  (->> (start-state input)
       (iterate tick)
       (drop-while #(> 2022 (:drops %)))
       first
       ;print-chamber
       :top))

(defn indices-of-row [pile row]
  (reduce
    (fn [result i] (if (= row (get pile i)) (conj result i) result))
    [] (range 0 (count pile))))

(defn find-pattern-start-at [pile i]
  (let [row      (get pile i)
        hits     (drop-while #(< % i) (indices-of-row pile row))
        segments (loop [hit (first hits) hits (rest hits) to-check (rest hits) result []]
                   (cond (and (empty? hits) (> 2 (count to-check))) result
                         (empty? hits) (recur (first to-check) (rest to-check) (rest to-check) result)
                         :else (recur hit (rest hits) to-check (conj result [hit (first hits)]))))
        segments (filter (fn [[a b]] (< 5 (- b a))) segments)]
    (prn "i (count segments): " i (count segments))
    (first (filter (fn [[start end]]
                     (let [len     (- end start)
                           repeats (partition len (drop start pile))
                           pattern (first repeats)]
                       (and (< 1 (count repeats)) (every? #(= pattern %) (rest repeats)))))
                   segments))))

(defn find-pattern [pile]
  (->> (map (partial find-pattern-start-at pile) (range 0 (/ (count pile) 2)))
       (remove nil?)
       first))

(defn solution2 [input target-drops sample-size]
  (let [states (->> (start-state input)
                    (iterate tick))
        state  (->> (drop-while #(> sample-size (:top %)) states) first)
        pile   (:pile state)]
    ;(sut/print-chamber state)
    (if-let [pattern (find-pattern pile)]
      (let [[start end] pattern
            len                    (- end start)
            start-state            (->> states
                                        (filter #(= start (count (:pile %))))
                                        first)
            end-state              (->> states
                                        (filter #(= end (count (:pile %))))
                                        first)
            start-drops            (:drops start-state)
            end-drops              (:drops end-state)
            drops-per-repeat       (- end-drops start-drops)
            repeats                (long (/ (- target-drops start-drops) drops-per-repeat))
            tail-drops             (rem (- target-drops start-drops) drops-per-repeat)
            state-after-tail-drops (->> states
                                        (filter #(= (+ start-drops tail-drops) (:drops %)))
                                        first)
            tail-size              (- (count (:pile state-after-tail-drops)) start)
            answer                 (+ start tail-size (* len repeats))]
        (prn "len: " len)
        (prn "start-drops: " start-drops)
        (prn "repeats: " repeats)
        (prn "tail-drops: " tail-drops)
        (prn "tail-size: " tail-size)
        (prn "answer: " answer)
        answer)
      (throw (Exception. "No pattern found")))))
