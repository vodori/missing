(ns missing.core-test
  (:require [clojure.test :refer :all :exclude (testing)]
            [missing.core :refer :all]
            [clojure.set :as sets])
  (:import (java.time Duration)))

(def invokes (atom []))
(def capture (fn [x] (swap! invokes conj x) x))
(def clear! (fn [] (reset! invokes [])))

(use-fixtures :each
  (fn [tests]
    (clear!)
    (tests)
    (clear!)))

(defmacro testing [description & body]
  `(clojure.test/testing ~description
     (do (clear!) ~@body (clear!))))

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

(deftest exclusive?-test
  (is (not (exclusive? #{1 2} #{2} #{2 3})))
  (is (exclusive? #{1} #{2} #{3}))
  (is (exclusive? #{1 2} #{3} #{4 5})))

(deftest keepcat-test
  (let [coll [[1 2 3 nil nil 4] [nil 5 6]]]
    (is (= [1 2 3 4 5 6] (into [] (keepcat identity) coll)))
    (is (= [1 2 3 4 5 6] (vec (keepcat identity coll))))))

(deftest lift-by-test
  (let [f (fn [x] (* x x))]
    (is (= 9 ((lift-by (partial * 3) f) 1)))))

(deftest deep-merge-test
  (let [m1 {1 {:stuff 3 :thing 2 :other 100} 2 4 :ten 10} m2 {1 {:things 4 :stuff 5 :other nil} :ten nil}]
    (is (= {1 {:thing 2 :stuff 5 :things 4 :other nil} 2 4 :ten nil} (deep-merge m1 m2)))))

(deftest key=-test
  (is (key= :stuff :stuff))
  (is (key= "stuff" :stuff))
  (is (not (key= :badger :stuff)))
  (is (not (key= "badger" :stuff))))

(deftest nor-test
  (let [state (atom [])]
    (is (not (nor true (swap! state conj 1))))
    (is (= [] @state)))
  (let [state (atom [])]
    (is (not (nor (identity false) (swap! state conj 1))))
    (is (= [1] @state)))
  (let [state (atom true) state2 (atom true)]
    (is (nor (identity false) (reset! state false) (reset! state2 false)))
    (is (false? @state))
    (is (false? @state2))))

(deftest sort-by-value-test
  (let [m {1 2 4 5 3 4 0 5}]
    (is (= [[4 5] [0 5] [3 4] [1 2]] (seq (sorted-map-by-value m (flip compare)))))
    (is (= [[1 2] [3 4] [0 5] [4 5]] (seq (sorted-map-by-value m))))))

(deftest dissoc-in-test
  (let [m {1 {:stuff 3 :thing 2} 2 4}]
    (is (= {1 {:thing 2} 2 4} (dissoc-in m [1 :stuff])))))

(deftest distinct-by-test
  (let [vs [{:stuff 5 :thing 2} {:thing 3 :stuff 7} {:thing 4 :stuff 5}]]
    (is (= vs (distinct-by :thing vs)))
    (is (= vs (into [] (distinct-by :thing) vs)))
    (is (= [(get vs 0) (get vs 1)] (distinct-by :stuff vs)))
    (is (= [(get vs 0) (get vs 1)] (into [] (distinct-by :stuff) vs)))))

(deftest merge-sort-test
  (let [v1 (range 0 10 2) v2 (range 1 10 2)]
    (is (= (range 0 10) (merge-sort [v1 v2])))))

(deftest do-force-test
  (let [state (atom [])]
    (is (nil?
          (doforce
            (do (swap! state conj 1) (throw (ex-info "" {})))
            (do (swap! state conj 2) (throw (ex-info "" {})))
            (do (swap! state conj 3) (throw (ex-info "" {})))
            (do (swap! state conj 4) (throw (ex-info "" {}))))))
    (is (= [1 2 3 4] @state))))

(deftest with-timeout-test
  (let [start (System/currentTimeMillis)
        [success result] (with-timeout 1000 (Thread/sleep 5000))
        stop  (System/currentTimeMillis)]
    (is (not success))
    (is (nil? result))
    (is (> 2000 (- stop start)))))

(deftest run-par!-test
  (let [task-durations [500 600 800]
        [millis _] (timing (run-par! #(Thread/sleep %) task-durations))]
    (is (> (/ (reduce + 0 task-durations) 2) millis))))

(deftest together-test
  (let [[millis [one two three]]
        (timing (together
                  (do (Thread/sleep 500) 1)
                  (do (Thread/sleep 600) 2)
                  (do (Thread/sleep 800) 3)))]
    (is (> (/ (reduce + 0 [500 600 800]) 2) millis))
    (is (= one 1))
    (is (= two 2))
    (is (= three 3))))

(deftest get-extension-test
  (is (nil? (get-extension "stuff")))
  (is (= ".pdf" (get-extension "stuff.txt.pdf")))
  (is (= "." (get-extension "stuff.")))
  (is (= ".stuff" (get-extension ".stuff")))
  (is (= ".txt" (get-extension ".stuff.txt"))))

(deftest get-filename-test
  (is (= "stuff" (get-filename "stuff")))
  (is (= "stuff.txt" (get-filename "stuff.txt.pdf")))
  (is (= "stuff" (get-filename "stuff.")))
  (is (= ".stuff" (get-filename ".stuff"))))

(deftest not-blank?-test
  (is (not-blank? "testing"))
  (is (not (not-blank? "")))
  (is (not (not-blank? nil)))
  (is (not (not-blank? "   \n\t \r\n"))))

(deftest duration-explain-test
  (is (= "2 days, 22 hours, 50 seconds, 233 milliseconds"
         (duration-explain (Duration/ofMillis 252050233)))))

(deftest subset-test
  (is (= #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}} (subsets #{1 2 3}))))

(deftest group-by-labels-test
  (let [maps    [{:labels {:one 1 :two 2 :three 3 :four 5}}
                 {:labels {:one 2 :two 2 :three 4 :four 5}}]
        grouped (group-by-labels :labels maps)]
    (is (nil? (get grouped {:bananas true})))
    (is (= [(first maps)] (get grouped {:one 1})))
    (is (= maps (get grouped {})))
    (is (= maps (get grouped {:two 2})))
    (is (= maps (get grouped {:two 2 :four 5})))
    (is (= [(second maps)] (get grouped {:three 4})))))

(deftest greatest-by-test
  (let [data [{:one 1 :two 2} {:one 2 :two 1}]]
    (is (= (first data) (greatest-by :two data)))
    (is (= (second data) (greatest-by :one data)))))

(deftest least-by-test
  (let [data [{:one 1 :two 2} {:one 2 :two 1}]]
    (is (= (first data) (least-by :one data)))
    (is (= (second data) (least-by :two data)))))

(deftest contiguous-by-test
  (let [data [{:startIndex 0 :stopIndex 20} {:startIndex 5 :stopIndex 26} {:startIndex 28 :stopIndex 50}]]
    (is (= [[{:startIndex 0 :stopIndex 20} {:startIndex 5 :stopIndex 26}] [{:startIndex 28 :stopIndex 50}]]
           (contiguous-by :startIndex :stopIndex data)))))

(deftest invert-grouping-test
  (let [m        (group-by even? (range 10))
        inverted (reverse-grouping m)]
    (dotimes [x 10]
      (is (= (even? x) (get inverted x))))))

(deftest find-first-test
  (let [coll [1 2 3 4 5 6 7]]
    (is (= 4 (find-first (partial < 3) coll)))
    (is (nil? (find-first (partial < 100) coll)))))

(deftest find-indexed-test
  (let [coll [1 2 3 4 5 6 7]]
    (is (= [3 4] (find-indexed (partial < 3) coll)))
    (is (nil? (find-indexed (partial < 100) coll)))))

(deftest indexcat-by-test
  (let [a {:keys [1 2] :a true}
        b {:keys [3 4] :b true}]
    (is (= {1 a 2 a 3 b 4 b}
           (indexcat-by :keys [a b])))))

(deftest join-paths-test
  (let [s1 "https://google.com"]
    (is (= s1 (join-paths s1)))
    (is (= "https://google.com/results/vodori/employees/paul"
           (join-paths s1 ["results/" "//vodori/" ["/employees" ["paul/"]]])))))

(deftest collate-test
  (let [pk           "paul.rutledge@example.com"
        person-info  [{:id pk :name "Paul" :age 26}]
        account-info [{:email pk :year-joined 2018}]
        table        (collate [[:id person-info]
                               [:email account-info]])]
    (is (contains? table pk))
    (is (= [(first person-info)
            (first account-info)]
           (table pk)))))

(deftest paths-test
  (testing "I can extract all paths to all values contained in a structure."
    (let [structure {:test [:things [{:more 1} {:more 2}] #{"one" {:whoa :sweet :things [1 2]}}]}]
      (is (= {[:test 0]                                         :things,
              [:test 1 0 :more]                                 1,
              [:test 1 1 :more]                                 2,
              [:test 2 {:whoa :sweet, :things [1 2]} :whoa]     :sweet,
              [:test 2 {:whoa :sweet, :things [1 2]} :things 0] 1,
              [:test 2 {:whoa :sweet, :things [1 2]} :things 1] 2,
              [:test 2 "one"]                                   "one"}
             (index-values-by-paths structure))))))

(deftest select-structure-test
  (let [example {:a [:b {:test [:thing [{:one :two :three 4}]]} :d]}]
    (is (= {:a [1 {:test [4 [{:one 5, :three nil}]]} nil]}
           (select-structure {:a [1 {:test [4 [{:one 5}]]}]} example)))))

(deftest paging-test
  (let [source  (vec (range 100))
        counter (atom 0)
        fetch   (fn [offset limit]
                  (swap! counter inc)
                  (subvec source
                          (min offset (count source))
                          (min (+ offset limit) (count source))))]

    (let [stream (paging 25 fetch)]
      (is (= 0 @counter))
      (is (= source (vec stream)))
      (is (= 5 @counter)))

    (reset! counter 0)

    (let [stream (paging 25 fetch)]
      (is (= (range 25) (take 25 stream)))
      (is (= 1 @counter))
      (is (= (range 26) (take 26 stream)))
      (is (= 2 @counter)))))

(deftest uniqueifier-test
  (testing "Uniquifying a few names"
    (let [uniq (uniqueifier)]
      (is (= "stuff.pdf" (uniq "stuff.pdf")))
      (is (= "stuff(1).pdf" (uniq "stuff.pdf")))
      (is (= "stuff(2).pdf" (uniq "stuff.pdf")))
      (is (= "stuff(1)(1).pdf" (uniq "stuff(1).pdf")))))

  (testing "Trying weird filenames"
    (let [uniq (uniqueifier)]
      (is (= "stuff(1)" (uniq "stuff(1)")))
      (is (= "cats" (uniq "cats")))
      (is (= "stuff(1)(1)" (uniq "stuff(1)")))
      (is (= "cats(1)" (uniq "cats")))
      (is (= "dogs.stuff.txt.pdf" (uniq "dogs.stuff.txt.pdf")))
      (is (= "dogs.stuff.txt(1).pdf" (uniq "dogs.stuff.txt.pdf")))
      (is (= "dogs.stuff.txt(2).pdf" (uniq "dogs.stuff.txt.pdf"))))))

(deftest glob-seq-test
  (testing "Globs against an exact dot file."
    (is (= 1 (count (glob-seq (System/getenv "PWD") ".gitignore")))))

  (testing "Globs against a wildcard extension."
    (is (= #{".clj"} (set (map #(get-extension (.getName %)) (glob-seq (System/getenv "PWD") "*.clj")))))))

(deftest distinct-by?-test
  (let [xs [{:x 1 :y 1} {:x 2 :y 1} {:x 3 :y 3} {:x 4 :y 4}]]
    (is (distinct-by? identity []))
    (is (distinct-by? :x xs))
    (is (not (distinct-by? :y xs)))))

(deftest =ic-test
  (is (=ic "test" "TEST" "tesT"))
  (is (not (=ic "test" "TEST" "tset")))
  (is (=ic "test"))
  (is (not (=ic "test" "tset")))
  (is (=ic "test" "test")))

(deftest =select-test
  (is (=select {:a "stuff" :b "things"} {:a "stuff" :b "things" :c "other-things"}))
  (is (not (=select {:a "stuff" :b "things"} {:a "stuff" :b "thoughts" :c "other-things"})))
  (is (=select {:a odd? :b even?} {:a 1 :b 2 :c "other-things"}))
  (is (not (=select {:a odd? :b even?} {:a 2 :b 1 :c "other-things"}))))

(deftest diff-by-test
  (let [a #{{:x 1 :v 1} {:x 1 :v 11} {:x 2 :v 2} {:x 3 :v 3}}
        b #{{:x 2 :v 2} {:x 3 :v 3} {:x 4 :v 4}}
        [only-a only-b both] (diff-by :x a b)]
    (is (sets/subset? only-a a))
    (is (not (intersect? only-a b)))
    (is (sets/subset? only-b b))
    (is (not (intersect? only-b a)))
    (is (sets/subset? both a))
    (is (sets/subset? both b))))

(deftest default-test
  (let [m  {:a 1 :b 2}
        m' (default m 3)]
    (is (= 3 (m' :c)))))


(deftest letd-test
  (testing "unusued bindings are never evaluated."
    (letd [a (capture 1)
           b (capture (+ a 1))]
      (is (empty? @invokes))))
  (testing "usage of early bindings doesn't force later bindings"
    (letd [a (capture 1)
           b (capture (+ a 1))]
      (is (= 1 a))
      (is (= [1] @invokes))))
  (testing "referencing bindings multiple times only evaluates once"
    (letd [a (capture 1)
           b (capture (+ a 1))]
      (is (= 2 b))
      (is (= 2 b))
      (is (= [1 2] @invokes))))
  (testing "bindings can be evaluated in any order."
    (letd [x (capture 10)
           {:keys [a b]} (capture {:a 1 :b 2})
           c (capture (+ a b))
           d (capture (+ a b x))]
      (is (empty? @invokes))
      (is (= x 10))
      (is (= [10] @invokes))
      (is (= 13 d))
      (is (= [10 {:a 1 :b 2} 13] @invokes))
      (is (= 3 c))
      (is (= [10 {:a 1 :b 2} 13 3] @invokes))))
  (testing "bindings can be nested"
    (letd [y (capture (letd [x (capture 1)
                             y (capture (+ x x x))]
                        y))]
      (is (empty? @invokes))
      (is (= y 3))
      (is (= [1 3 3] @invokes))))
  (testing "bindings aren't evaluated if fully shadowed."
    (let [f (fn [x]
              (letd [{:keys [a b c]} (capture {:a 1 :b 2 :c 3})]
                (let [{:keys [a b]} {:a 3 :b 4}]
                  (if (odd? x)
                    (+ a b)
                    (+ a b c)))))
          v (f 3)]
      (is (empty? @invokes))
      (is (= v 7))
      (is (empty? @invokes))
      (let [v2 (f 4)]
        (is (= [{:a 1 :b 2 :c 3}] @invokes))
        (is (= v2 10)))))
  (testing "bindings aren't evaluated if shadowed fn"
    (let [f (letd [x (capture 1)]
              (fn [x] (+ x x)))]
      (is (empty? @invokes))
      (is (= 8 (f 4)))
      (is (empty? @invokes))))
  (testing "supports linear deps"
    (letd [a 1 b (+ a 1)]
      (is (= b 2))))
  (testing "supports shadowing by self"
    (letd [a 1]
      (is (= a 1))
      (letd [a 2]
        (is (= a 2)))
      (is (= a 1))))
  (testing "supports shadowing by let"
    (letd [a 1]
      (is (= a 1))
      (let [a 2]
        (is (= a 2)))
      (is (= a 1))))
  (testing "supports shadowing by loop"
    (letd [a 1]
      (is (= a 1))
      (is (= 2 (loop [a 2]
                 a)))
      (is (= a 1))))
  (testing "supports shadowing by fn"
    (letd [a 1]
      (is (= a 1))
      (is (= 2 ((fn [a] a) 2)))
      (is (= a 1))))
  (testing "supports destructuring"
    (letd [{:keys [a b]} {:a 1 :b 2}]
      (is (= a 1))
      (is (= b 2)))))

(deftest letp-test
  (let [v (letp [a (+ 1 2 3)
                 b (if (even? a) (preempt 4) 5)
                 c (capture b)]
            c)]
    (is (= v 4))
    (is (empty? @invokes))))

(deftest zip-test
  (is (= [[1] [2] [3]] (into [] (zip) [1 2 3])))
  (is (= [] (zip [])))
  (is (= [] (zip [] [])))
  (is (= [] (zip [] [1])))
  (is (= [[1 3] [2 4]] (zip [1 2] [3 4])))
  (is (= [[1 3 5] [2 4 6]] (zip [1 2] [3 4] [5 6]))))

(deftest map-groups-test
  (is (= {:a []} (map-groups inc {:a []})))
  (is (= {:a [1 2 3] :b [4 5 6]} (map-groups inc {:a [0 1 2] :b [3 4 5]}))))

(deftest mapcat-groups-test
  (is (= {:a []} (mapcat-groups #(vector % %) {:a []})))
  (is (= {:a [1 1 2 2] :b [3 3]} (mapcat-groups #(repeat 2 %) {:a [1 2] :b [3]}))))

(deftest filter-groups-test
  (is (= {:a []} (filter-groups odd? {:a []})))
  (is (= {:a [1 3] :b []} (filter-groups odd? {:a [1 2 3] :b [4]}))))

(deftest remove-groups-test
  (is (= {:a []} (remove-groups inc {:a []})))
  (is (= {:a [2] :b [4]} (remove-groups odd? {:a [1 2 3] :b [4]}))))

(deftest reduce-groups-test
  (is (= {:a 0 :b 0} (reduce-groups + {:a [] :b []})))
  (is (= {:a 6 :b 15} (reduce-groups + {:a [1 2 3] :b [4 5 6]}))))

(deftest iterable?-test
  (is (iterable? [1 2 3]))
  (is (iterable? '(1 2 3)))
  (is (iterable? #{1 2 3}))
  (is (iterable? (take 1 [1 2])))
  (is (iterable? (keys {:a :b :c :d})))
  (is (iterable? (vals {:a :b :c :d})))
  (is (not (iterable? "test")))
  (is (not (iterable? {:a [1 2 3]}))))

(deftest one?-test
  (is (one? even? [1 2 3]))
  (is (not (one? odd? [1 2 3]))))

(deftest extrema-test
  (is (= [nil nil] (extrema [])))
  (is (= [Double/MAX_VALUE Double/MAX_VALUE] (extrema [Double/MAX_VALUE])))
  (is (= [-3 6] (extrema [-1 -2 -3 6 5 4])))
  (is (= [[1 -2] [50 9]] (extrema [[1 2] [1 -2] [50 -9] [50 9] [50 8]]))))
