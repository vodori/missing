(ns missing.topology-test
  (:require [clojure.test :refer :all])
  (:require [missing.topology :refer :all]))

(deftest normalization-test
  (let [g {:a [:b :c] :b [:d]}]
    (is (= {:a #{:c :b}
            :b #{:d}
            :c #{}
            :d #{}}
           (normalize g)))))

(deftest incoming-edges-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:b :c :d} (incoming (normalize g)))))

(deftest no-incoming-edges-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:a} (no-incoming (normalize g)))))

(deftest outgoing-edges-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:a :b} (outgoing (normalize g)))))

(deftest no-outgoing-edges-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:c :d} (no-outgoing (normalize g)))))

(deftest topological-sort-test
  (let [g {:a [:b :c] :b [:d]}]
    (is (= [:a :c :b :d] (topological-sort g)))))

(deftest topological-sort-with-grouping-test
  (let [g {:a [:b :c] :b [:d]}]
    (is (= [#{:a} #{:c :b} #{:d}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :c] :b [:d] :c [:d]}]
    (is (= [#{:a} #{:c :b} #{:d}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :m] :b [:c] :c [:d] :d [:e] :m [:e]}]
    (is (= [#{:a} #{:m :b} #{:c} #{:d} #{:e}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :c]
           :b [:d :e]
           :c [:e :d]
           :e [:f]
           :d [:f]}]
    (is (= [#{:a} #{:c :b} #{:e :d} #{:f}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :c :l]
           :b [:d :e :m]
           :c [:e :d :m]
           :d [:f]
           :e [:f]
           :l [:d :e]}]
    (is (= [#{:a} #{:l :c :b} #{:m :e :d} #{:f}]
           (topological-sort-with-grouping g)))))