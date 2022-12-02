(ns aoc.2015.day2-spec
  (:require [aoc.2015.day2 :as sut]
            [aoc.core :as core]
            [speclj.core :refer :all]))

(describe "2015 Day 2: I Was Told There Would Be No Math"

  (it "read sizes"
    (let [result (sut/read-sizes "2x3x4\n1x1x10")]
      (should= [2 3 4] (first result))
      (should= [1 1 10] (second result))))

  (it "paper-needed"
    (should= 58 (sut/paper-needed [2 3 4]))
    (should= 43 (sut/paper-needed [1 1 10])))

  (it "total paper"
    (should= 101 (sut/total-paper "2x3x4\n1x1x10"))
    (should= 1606483 (sut/total-paper (core/input 2015 2))))

  (it "ribbon needed"
    (should= 34 (sut/ribbon-needed [2 3 4]))
    (should= 14 (sut/ribbon-needed [1 1 10])))

  (it "total ribbon"
    (should= 48 (sut/total-ribbon "2x3x4\n1x1x10"))
    (should= 3842356 (sut/total-ribbon (core/input 2015 2))))

)
