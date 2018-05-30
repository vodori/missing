(ns missing.core
  (:require [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets]
            [clojure.edn :as edn])
  (:import (java.io File)
           (java.util.concurrent TimeUnit Future)))


(defn load-edn-resource [path]
  (edn/read-string (slurp (io/resource path))))

(defn filter-keys [pred m]
  (into {} (filter (comp pred key) m)))

(defn filter-vals [pred m]
  (into {} (filter (comp pred val) m)))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v])) m))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn reverse-map [m]
  (into {} (map (comp vec reverse)) m))

(defn not-empty? [coll]
  ((complement empty?) coll))

(defn not-blank? [s]
  ((complement strings/blank?) s))

(defn index-by [key-fn coll]
  (into {} (map (juxt key-fn identity)) coll))

(defn contains-all? [coll keys]
  (every? (partial contains? coll) keys))

(defn lift-by [lift f]
  (fn [& args] (apply f (map lift args))))

(defn intersect? [s1 s2 & ss]
  (let [intersection (sets/intersection (set s1) (set s2))]
    (if (or (empty? intersection) (empty? ss))
      (not-empty? intersection)
      (recur intersection (first ss) (rest ss)))))

(defn shared-keys [m1 m2 & ms]
  (apply sets/intersection (map (comp set keys) (concat [m1 m2] ms))))

(defn shared-entries [m1 m2 & ms]
  (let [maps (concat [m1 m2] ms)
        keys (apply shared-keys maps)]
    (map #(select-keys % keys) maps)))

(defn deep-merge [& maps]
  (letfn [(inner-merge [& maps]
            (let [ms (remove nil? maps)]
              (if (every? map? ms)
                (apply merge-with inner-merge ms)
                (last ms))))]
    (apply inner-merge maps)))

(defn key= [& more]
  (apply = (map keyword more)))

(defmacro nor [& more]
  (conj `~(partition 2 (interleave (repeat 'not) more)) 'and))

(defn find-first [f coll]
  (first (drop-while (complement f) coll)))

(defn sort-by-value [m]
  (into
    (sorted-map-by
      (fn [key1 key2]
        (compare
          [(get m key2) key2]
          [(get m key1) key1])))
    m))

(defn dissoc-in [m [k & ks]]
  (if ks
    (if (map? (get m k))
      (update m k #(dissoc-in % ks))
      m)
    (dissoc m k)))

(defn distinct-by
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [fx (f x) k (hash fx)]
            (if (contains? @seen k)
              result
              (do (vswap! seen conj k)
                  (rf result x)))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [fx (f x) k (hash fx)]
                         (if (contains? seen k)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen k)))))))
                    xs seen)))]
     (step coll #{}))))


(defn merge-sort [comp colls]
  (let [begin-marker (Object.)
        end-marker   (Object.)]
    (letfn [(next-item [[_ colls]]
              (if (nil? colls)
                [end-marker nil]
                (let [[[yield & p] & q]
                      (sort-by first comp colls)]
                  [yield (if p (cons p q) q)])))]
      (->> colls
           (vector begin-marker)
           (iterate next-item)
           (drop 1)
           (map first)
           (take-while #(not (identical? end-marker %)))))))


(defmacro do-force
  ([] nil)
  ([x] `(try ~x (catch Exception _# nil)))
  ([x & next]
   `(do (try ~x (catch Exception _# nil)) (do-force ~@next))))



(defmacro with-timeout [millis & body]
  `(let [future# (future ~@body)]
     (try (^Future .get future# ~millis TimeUnit/MILLISECONDS)
          (catch Exception _#
            (when-not (.isDone future#)
              (try (future-cancel future#)
                   (catch Exception _# nil)))))))

(defn run-par! [f coll]
  (run! deref (doall (map #(future (f %)) coll))))

(defmacro together [& expressions]
  (let [expanded (conj `~(partition 2 (interleave (repeat 'future) expressions)) 'list)]
    `(map deref (doall ~expanded))))

(defn get-extension [filename]
  (first (re-find #"(\.[^.]*)$" filename)))

(defn get-filename [filename]
  (second (re-find #"(.+?)(\.[^.]*$|$)" filename)))

(defn delete-recursively [^File file]
  (letfn [(func [f]
            (when (.isDirectory f)
              (doseq [f2 (.listFiles f)]
                (func f2)))
            (io/delete-file f))]
    (func file)))

(defn merge-entries-with [f & ms]
  (letfn [(merge-entry [m [k v]]
            (if (contains? m k)
              (assoc m k (f k (get m k) v))
              (assoc m k v)))
          (merge-entries [m1 m2]
            (reduce merge-entry (or m1 {}) (seq m2)))]
    (reduce merge-entries {} ms)))