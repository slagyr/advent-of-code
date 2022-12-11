(ns aoc.2022.day11
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn ->items [lines] (mapv core/->int (str/split (subs (second lines) 18) #", ")))

(defn ->operation [lines]
  (let [line (nth lines 2)
        [a operator b] (str/split (subs line 19) #" ")]
    [(case operator "+" + "*" *)
     (if (= "old" a) :old (core/->int a))
     (if (= "old" b) :old (core/->int b))]))

(defn ->test [lines] (core/->int (subs (nth lines 3) 21)))
(defn ->pass [lines] (core/->int (subs (nth lines 4) 29)))
(defn ->fail [lines] (core/->int (subs (nth lines 5) 30)))

(defn ->monkey [lines]
  {:items       (->items lines)
   :operation   (->operation lines)
   :test        (->test lines)
   :pass        (->pass lines)
   :fail        (->fail lines)
   :inspections 0})

(defn read-input [input]
  (let [lines        (str/split-lines input)
        monkey-lines (partition 6 7 lines)]
    (mapv ->monkey monkey-lines)))

(defn operate [[operator a b] item]
  (let [a (if (= :old a) item a)
        b (if (= :old b) item b)]
    ;(prn "operator a b: " operator a b)
    (operator a b)))

(defn inspect-item [relief? {:keys [operation test pass fail]} monkeys item]
  (let [updated-item (operate operation item)
        updated-item (if relief?
                       (int (/ updated-item 3))
                       ;; hackish... had to get help.  Since all the tests are primes, we can simplify the worry value
                       (mod updated-item (apply * (map :test monkeys))))]
    (if (= 0 (rem updated-item test))
      (update-in monkeys [pass :items] conj updated-item)
      (update-in monkeys [fail :items] conj updated-item))))

(defn monkey-round [relief? monkeys i]
  (let [monkey (get monkeys i)
        items  (:items monkey)]
    (-> (reduce (partial inspect-item relief? monkey) monkeys items)
        (assoc-in [i :items] [])
        (update-in [i :inspections] + (count items)))))

(defn round [relief? monkeys]
  (reduce (partial monkey-round relief?) monkeys (range 0 (count monkeys))))

(defn monkey-business [relief? rounds input]
  (let [monkeys (read-input input)
        monkeys (reduce (fn [monkeys i] (round relief? monkeys)) monkeys (range rounds))
        inspections (reverse (sort (map :inspections monkeys)))]
    (->> (take 2 inspections)
         (apply *))))
