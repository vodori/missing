(ns missing.core-test
  (:require [clojure.test :refer :all]
            [missing.core :refer :all]))



(deftest filter-keys-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (filter-keys any? nil)))
    (is (= {1 2 3 4} (filter-keys odd? m)))))

(deftest filter-vals-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (filter-vals any? nil)))
    (is (= {6 5 8 7} (filter-vals odd? m)))))

(deftest map-keys-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (map-keys inc nil)))
    (is (= {2 2 4 4 7 5 9 7} (map-keys inc m)))))

(deftest map-vals-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (map-vals inc nil)))
    (is (= {1 3 3 5 6 6 8 8} (map-vals inc m)))))

(deftest reverse-map-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (reverse-map nil)))
    (is (= {2 1 4 3 5 6 7 8} (reverse-map m)))))

(deftest index-by-test
  (let [coll [{:id 1} {:id 2} {:id 3}]]
    (is (= {1 {:id 1} 2 {:id 2} 3 {:id 3}} (index-by :id coll)))))

(deftest contains-all?-test
  (let [coll #{1 2 3 4 5}]
    (is (contains-all? coll []))
    (is (contains-all? coll [1 2]))
    (is (contains-all? coll [1 2 3 4 5]))
    (is (not (contains-all? coll [1 6])))))

(deftest intersect?-test
  (is (intersect? #{1 2} #{2} #{2 3}))
  (is (not (intersect? #{1} #{2} #{3})))
  (is (not (intersect? #{1 2} #{2} #{3}))))
