(ns missing.paths-test
  (:require [clojure.test :refer :all])
  (:require [missing.paths :refer :all]))

(deftest path-seq-test
  (testing "I can extract all paths to all values contained in a structure."
    (let [structure {:test [:things [{:more 1} {:more 2}] #{"one" {:whoa :sweet :things [1 2]}}]}]
      (is (= [[:things [:test 0]]
              [1 [:test 1 0 :more]]
              [2 [:test 1 1 :more]]
              [:sweet [:test 2 {:whoa :sweet :things [1 2]} :whoa]]
              [1 [:test 2 {:whoa :sweet :things [1 2]} :things 0]]
              [2 [:test 2 {:whoa :sweet :things [1 2]} :things 1]]
              ["one" [:test 2 "one"]]]
             (path-seq structure))))))
