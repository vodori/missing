(ns missing.core
  (:require [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets]
            [clojure.edn :as edn])
  (:import (java.util.concurrent TimeUnit TimeoutException Future)
           (java.util EnumSet)
           (java.time Duration)))


(defn load-edn-resource [path]
  (edn/read-string (slurp (io/resource path))))

(defn filter-keys [pred m]
  (into {} (filter (comp pred key)) m))

(defn filter-vals [pred m]
  (into {} (filter (comp pred val)) m))

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

(defn flip [f]
  (fn [& args] (apply f (reverse args))))

(defn keepcat
  ([f] (comp (map f) cat (filter some?)))
  ([f & colls]
   (filter some? (apply concat (apply map f colls)))))

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

(defn dedupe-by
  ([f]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv
                nv    (f input)]
            (vreset! pv nv)
            (if (= prior nv)
              result
              (rf result input))))))))
  ([f coll] (sequence (dedupe f) coll)))

(defn least-by [f coll]
  (letfn [(inner-least-by
            ([_] nil)
            ([_ a] a)
            ([f a b] (if (neg? (compare (f a) (f b))) a b))
            ([f a b & more] (reduce (partial least-by f) (inner-least-by f a b) more)))]
    (apply inner-least-by f coll)))

(defn greatest-by [f coll]
  (letfn [(inner-greatest-by
            ([_] nil)
            ([_ a] a)
            ([f a b] (if (pos? (compare (f a) (f b))) a b))
            ([f a b & more] (reduce (partial greatest-by f) (inner-greatest-by f a b) more)))]
    (apply inner-greatest-by f coll)))

(defn least [coll]
  (least-by identity coll))

(defn greatest [coll]
  (greatest-by identity coll))

(defn merge-sort
  ([colls]
   (merge-sort compare colls))
  ([comp colls]
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
            (take-while #(not (identical? end-marker %))))))))


(defmacro doforce
  ([] nil)
  ([x] `(try ~x (catch Exception _# nil)))
  ([x & next]
   `(do (try ~x (catch Exception _# nil)) (doforce ~@next))))

(defmacro with-timeout [millis & body]
  `(let [future# ^Future (future ~@body)]
     (try [true (.get future# ~millis TimeUnit/MILLISECONDS)]
          (catch TimeoutException _#
            (try (if-not (future-cancel future#)
                   [true (.get future# 0 TimeUnit/MILLISECONDS)]
                   [false nil])
                 (catch Exception _#
                   [false nil]))))))

(defmacro timing [& body]
  `(let [start#  (System/currentTimeMillis)
         result# (do ~@body)
         stop#   (System/currentTimeMillis)]
     [(- stop# start#) result#]))

(defn run-par! [f coll]
  (run! deref (doall (map #(future (f %)) coll))))

(defmacro together [& expressions]
  (let [expanded (conj `~(partition 2 (interleave (repeat 'future) expressions)) 'list)]
    `(map deref (doall ~expanded))))

(defn human-readable [duration]
  (let [values (into [] (EnumSet/allOf TimeUnit))
        lowest ^TimeUnit (get values 0)]
    (->> values
         (reverse)
         (reduce
           (fn [[^String s ^Long rem :as agg] ^TimeUnit next]
             (let [in-unit (.convert next rem lowest)]
               (if (pos? in-unit)
                 (let [pass-down (- rem (.convert lowest in-unit next))
                       base-name (strings/lower-case (apply str (butlast (.name next))))]
                   [(->> [s (format (if (> in-unit 1) "%d %ss" "%d %s") in-unit base-name)]
                         (remove strings/blank?)
                         (strings/join ", "))
                    pass-down])
                 agg)))
           ["" (.toNanos (if (instance? Duration duration)
                           duration (Duration/ofMillis duration)))])
         (first))))

(defn get-extension [filename]
  (first (re-find #"(\.[^.]*)$" filename)))

(defn get-filename [filename]
  (second (re-find #"(.+?)(\.[^.]*$|$)" filename)))

(defn merge-entries-with [f & ms]
  (letfn [(merge-entry [m [k v]]
            (if (contains? m k)
              (assoc m k (f k (get m k) v))
              (assoc m k v)))
          (merge-entries [m1 m2]
            (reduce merge-entry (or m1 {}) (seq m2)))]
    (reduce merge-entries {} ms)))

(defn subsets [coll]
  (reduce (fn [a x] (into a (map #(conj % x)) a)) #{#{}} coll))

(defn submaps [m]
  (->> m (seq) (subsets) (map (partial into {})) (set)))

(defn indexcat-by [f coll]
  (reduce #(apply assoc %1 (interleave (f %2) (repeat %2))) {} coll))

(defn groupcat-by [f coll]
  (-> (fn [agg x]
        (-> (fn [agg* k]
              (update agg* k (fnil conj []) x))
            (reduce agg (f x))))
      (reduce {} coll)))

(defn index-by-labels [f coll]
  (indexcat-by (comp submaps f) coll))

(defn group-by-labels [f coll]
  (groupcat-by (comp submaps f) coll))