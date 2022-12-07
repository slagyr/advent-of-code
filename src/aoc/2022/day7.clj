(ns aoc.2022.day7
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(defn cd [state line]
  (let [path (subs line 5)]
    (if (= ".." path)
      (update state :path #(vec (butlast %)))
      (update state :path conj path))))

(defn add-dir [state line]
  (let [dirname (subs line 4)
        path (vec (conj (:path state) dirname))]
    (assoc-in state (cons :fs path) {})))

(defn add-file [state line]
  (let [[size-str filename] (str/split line #" ")
        size (core/->long size-str)
        path (vec (conj (:path state) filename))]
    (assoc-in state (cons :fs path) size)))

(defn read-line [state line]
  (cond (str/starts-with? line "$ cd") (cd state line)
        (= line "$ ls") state
        (str/starts-with? line "dir") (add-dir state line)
        :else (add-file state line)))

(defn read-input [input]
  (let [lines (str/split-lines input)
        state {:fs {} :path []}
        result (reduce read-line state lines)]
    (:fs result)))

(defn count-directory-sizes [fs result path]
  (let [contents (get-in fs path)
        files (filter (fn [[k v]] (number? v)) contents)
        dirs (filter (fn [[k v]] (map? v)) contents)
        dir-paths (map #(conj path %) (keys dirs))
        result (reduce (partial count-directory-sizes fs) result dir-paths)
        files-total (or (apply + (vals files)) 0)
        dirs-total (or (apply + (map #(get result %) dir-paths)) 0)]
    (assoc result path (+ files-total dirs-total))))

(defn directory-sizes [fs] (count-directory-sizes fs {} ["/"]))

(defn solution1 [input]
  (->> (read-input input)
       directory-sizes
       vals
       (filter #(< % 100000))
       (apply +)))

(defn solution2 [input]
  (let [sizes (directory-sizes (read-input input))
        available (- 70000000 (get sizes ["/"]))
        needed (- 30000000 available)]
    (->> (vals sizes)
         (filter #(> % needed))
         sort
         first)))

