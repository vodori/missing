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