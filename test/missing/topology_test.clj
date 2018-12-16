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

(deftest consumers-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:b :c :d} (consumers g))))

(deftest producers-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:a :b} (producers g))))

(deftest sources-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:a} (sources g))))

(deftest sinks-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:c :d} (sinks g))))

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

(deftest topological-sorts-return-nil-if-cyclical
  (let [g {:a [:b :c] :b [:a]}]
    (is (nil? (topological-sort g)))
    (is (nil? (topological-sort-with-grouping g)))))