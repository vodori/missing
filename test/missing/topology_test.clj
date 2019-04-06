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

(deftest inverse-preserves-nodes-with-no-edges
  (let [g {:a [:b :c] :d #{}}]
    (is (= {:c #{:a}, :b #{:a}, :d #{}, :a #{}} (inverse g)))))

(deftest shortest-paths-test
  (let [g {:a [:b :c] :c [:d] :d [:e] :b [:e]}]
    (is (= {[:b :e] {:distance 1, :path [:b :e]},
            [:c :d] {:distance 1, :path [:c :d]},
            [:c :e] {:distance 2, :path [:c :d :e]},
            [:e :e] {:distance 0, :path [:e]},
            [:a :d] {:distance 2, :path [:a :c :d]},
            [:d :e] {:distance 1, :path [:d :e]},
            [:a :a] {:distance 0, :path [:a]},
            [:a :b] {:distance 1, :path [:a :b]},
            [:d :d] {:distance 0, :path [:d]},
            [:b :b] {:distance 0, :path [:b]},
            [:a :c] {:distance 1, :path [:a :c]},
            [:a :e] {:distance 2, :path [:a :b :e]},
            [:c :c] {:distance 0, :path [:c]}}
           (shortest-paths g)))))

(deftest cycles-and-shortest-paths
  (let [g     {"application/pdf" #{"image/*"}
               "image/*"         #{"image/png" "image/gif" "image/jpeg"}
               "image/png"       #{"image/*"}
               "image/gif"       #{"image/*"}
               "image/jpeg"      #{"image/*"}
               "image/tiff"      #{"application/pdf"}}
        paths (shortest-paths g)]
    (are [source target] (contains? paths [source target])
      "image/tiff" "image/png"
      "image/tiff" "image/gif"
      "image/tiff" "image/jpeg"
      "image/tiff" "application/pdf"
      "image/png" "image/png"
      "image/png" "image/jpeg"
      "image/png" "image/gif"
      "image/png" "image/*"
      "image/gif" "image/gif"
      "image/gif" "image/jpeg"
      "image/gif" "image/png"
      "image/jpeg" "image/jpeg"
      "image/jpeg" "image/png"
      "image/jpeg" "image/gif"
      "image/jpeg" "image/*"
      "image/png" "image/*"
      "image/gif" "image/*")))
