(ns missing.core
  (:require [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets]
            [clojure.edn :as edn])
  (:import (java.util.concurrent TimeUnit TimeoutException Future)
           (java.util EnumSet UUID)
           (java.time Duration)))

(defn uuid
  "Get a uuid as string"
  []
  (str (UUID/randomUUID)))

(defn load-edn-resource
  "Load and parse an edn file from the classpath."
  [path]
  (edn/read-string (slurp (io/resource path))))

(defn filter-keys
  "Filter a map by a predicate on its keys"
  [pred m]
  (into {} (filter (comp pred key)) m))

(defn filter-vals
  "Filter a map by a predicate on its values"
  [pred m]
  (into {} (filter (comp pred val)) m))

(defn map-keys
  "Transform the keys of a map"
  [f m]
  (into {} (map (fn [[k v]] [(f k) v])) m))

(defn map-vals
  "Transform the values of a map"
  [f m]
  (into {} (map (fn [[k v]] [k (f v)])) m))

(defn reverse-map
  "Invert a map"
  [m]
  (into {} (map (comp vec reverse)) m))

(defn not-empty? [coll]
  ((complement empty?) coll))

(defn not-blank? [s]
  ((complement strings/blank?) s))

(defn index-by
  "Index the items of a collection into a map by a key"
  [key-fn coll]
  (into {} (map (juxt key-fn identity)) coll))

(defn contains-all? [coll keys]
  (let [ks (set keys)]
    (= ks (sets/intersection (set (seq coll)) ks))))

(defn lift-by
  "Returns a function that first applies the lift to each argument before applying the original function."
  [lift f]
  (fn [& args] (apply f (map lift args))))

(defn flip
  "Returns a new function that applies the provided arguments in reverse order."
  [f]
  (fn [& args] (apply f (reverse args))))

(defn keepcat
  "A transducer like mapcat except removes nil elements."
  ([f] (comp (map f) cat (filter some?)))
  ([f & colls]
   (filter some? (apply concat (apply map f colls)))))

(defn intersect?
  "Returns true if the provided collections overlap."
  [s1 s2 & ss]
  (let [intersection (sets/intersection (set s1) (set s2))]
    (if (or (empty? intersection) (empty? ss))
      (not-empty? intersection)
      (recur intersection (first ss) (rest ss)))))

(defn exclusive?
  "Returns true if the provided collections are all mutually exclusive."
  [s1 s2 & ss]
  (let [set1 (set s1) set2 (set s2)]
    (cond
      (intersect? set1 set2) false
      (empty? ss) true
      :otherwise (recur (sets/union set1 set2) (first ss) (rest ss)))))

(defn deep-merge
  "Merges nested maps."
  [& maps]
  (letfn [(inner-merge [& maps]
            (let [ms (remove nil? maps)]
              (if (every? map? ms)
                (apply merge-with inner-merge ms)
                (last ms))))]
    (apply inner-merge maps)))

(defn key=
  "Equality after conversion to keywords. Use when you're unsure if the
  arguments are strings or keywords"
  [& more]
  (apply = (map keyword more)))

(defmacro nor
  "Expands to (and (not form1) (not form2) ...)"
  [& more]
  (conj `~(partition 2 (interleave (repeat 'not) more)) 'and))

(defn find-first
  "Find the first element in the collection that matches the pred"
  [pred coll]
  (first (drop-while (complement pred) coll)))

(defn sort-by-value
  "Sort a map by its values"
  [m]
  (into
    (sorted-map-by
      (fn [key1 key2]
        (compare
          [(get m key2) key2]
          [(get m key1) key1])))
    m))

(defn dissoc-in
  "Dissociate a key/value from a map at a given path."
  [m [k & ks]]
  (if ks
    (if (map? (get m k))
      (update m k #(dissoc-in % ks))
      m)
    (dissoc m k)))

(defn distinct-by
  "Like distinct but according to a key-fn instead of the element itself."
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
  "Like dedupe but according to a key-fn instead of the element itself."
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
  ([f coll] (sequence (dedupe-by f) coll)))


(defn lt
  "Like < but for comparables."
  ([_] true)
  ([a b] (neg? (compare a b)))
  ([a b & more]
   (if (lt a b)
     (if (next more)
       (recur b (first more) (next more))
       (lt b (first more)))
     false)))

(defn lte
  "Like <= but for comparables."
  ([_] true)
  ([a b] (not (pos? (compare a b))))
  ([a b & more]
   (if (lte a b)
     (if (next more)
       (recur b (first more) (next more))
       (lte b (first more)))
     false)))

(defn gt
  "Like > but for comparables."
  ([_] true)
  ([a b] (pos? (compare a b)))
  ([a b & more]
   (if (gt a b)
     (if (next more)
       (recur b (first more) (next more))
       (gt b (first more)))
     false)))

(defn gte
  "Like >= but for comparables."
  ([_] true)
  ([a b] (not (neg? (compare a b))))
  ([a b & more]
   (if (gte a b)
     (if (next more)
       (recur b (first more) (next more))
       (gte b (first more)))
     false)))

(defn least-by
  "Returns the smallest element according to some fn of the element"
  [f coll]
  (letfn [(inner-least
            ([] nil)
            ([a] a)
            ([a b] (if (lt (f a) (f b)) a b)))]
    (reduce inner-least coll)))

(defn greatest-by
  "Returns the largest element according to some fn of the element"
  [f coll]
  (letfn [(inner-greatest
            ([] nil)
            ([a] a)
            ([a b] (if (gt (f a) (f b)) a b)))]
    (reduce inner-greatest coll)))

(defn least
  "Returns the smallest element in the collection."
  [coll]
  (least-by identity coll))

(defn greatest
  "Returns the largest element in the collection."
  [coll]
  (greatest-by identity coll))

(defn merge-sort
  "Lazily merges sequences that are already sorted in the same order."
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

(defn contiguous-by
  "Transducer that partitions collections into contiguous segments
   according to the comparables returned by f-start and f-stop."
  ([f-start f-stop]
   (let [sentinel (Object.)
         state    (volatile! [sentinel sentinel])]
     (partition-by
       (fn [item]
         (let [[prev-start prev-stop] (deref state)
               [next-start next-stop] ((juxt f-start f-stop) item)]
           (-> (if (and (not (identical? prev-start sentinel))
                        (not (identical? prev-stop sentinel))
                        (gte prev-stop next-start prev-start))
                 (vreset! state [(least [prev-start next-start])
                                 (greatest [prev-stop next-stop])])
                 (vreset! state [next-start next-stop]))
               (first)))))))
  ([f-start f-stop coll]
   (sequence (contiguous-by f-start f-stop) coll)))

(defmacro quietly
  "Execute the body and return nil if there was an error"
  [& body]
  `(try ~@body (catch Throwable _# nil)))

(defmacro doforce
  "Execute each top-level form of the body even if they throw,
  and return nil if there was an error."
  ([] nil)
  ([x] `(quietly ~x))
  ([x & next] `(do (quietly ~x) (doforce ~@next))))

(defmacro with-timeout
  "Run body on a separate thread subject to a timeout. If reaches timeout
  a vector of [false nil] will be returned, otherwise [true result]"
  [millis & body]
  `(let [future# ^Future (future ~@body)]
     (try [true (.get future# ~millis TimeUnit/MILLISECONDS)]
          (catch TimeoutException _#
            (try (if-not (future-cancel future#)
                   [true (.get future# 0 TimeUnit/MILLISECONDS)]
                   [false nil])
                 (catch Exception _#
                   [false nil]))))))

(defmacro timing
  "Returns a vector of [millis-taken result]"
  [& body]
  `(let [start#  (System/currentTimeMillis)
         result# (do ~@body)
         stop#   (System/currentTimeMillis)]
     [(- stop# start#) result#]))

(defn run-par!
  "Like run! but executes each element concurrently."
  [f coll]
  (run! deref (doall (map #(future (f %)) coll))))

(defmacro together
  "Executes each top level form in body concurrently and returns
  a sequence of the results."
  [& expressions]
  (let [expanded (conj `~(partition 2 (interleave (repeat 'future) expressions)) 'list)]
    `(map deref (doall ~expanded))))

(defn human-readable
  "Converts millis or a java.date.Duration into human readable text."
  [duration]
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

(defn get-extension
  "Get the file extension from a filename."
  [filename]
  (first (re-find #"(\.[^.]*)$" filename)))

(defn get-filename
  "Get the filename (without extension) from a filename"
  [filename]
  (second (re-find #"(.+?)(\.[^.]*$|$)" filename)))

(defn merge-entries-with
  "Merges entries in maps according to f. f is called for
  every key value combo even when the key does not appear in the
  second map. f should return a sequence of [k v] vectors
  that should be added to the aggregate."
  [f & ms]
  (letfn [(merge-entry [m [k v]]
            (let [additional-entries (f k (get m k) v)]
              (reduce (fn [m* [k* v*]] (assoc m* k* v*)) m (or additional-entries []))))
          (merge-entries [m1 m2] (reduce merge-entry (or m1 {}) (seq m2)))]
    (reduce merge-entries {} ms)))

(defn merge+
  "Merges maps by adding keys and adding sets"
  [& maps]
  (letfn [(merger [k v1 v2]
            (cond
              (and (set? v1) (set? v2)) [k (sets/union v1 v2)]
              (set? v1) [k (conj v1 v2)]
              :otherwise [k v2]))]
    (apply merge-entries-with merger maps)))

(defn merge-
  "Merges maps by subtracting keys and subtracting sets"
  [& maps]
  (letfn [(merger [k v1 v2]
            (cond
              (and (set? v1) (set? v2)) [k (sets/difference v1 v2)]
              (set? v1) [k (disj v1 v2)]
              :otherwise []))]
    (apply merge-entries-with merger maps)))

(defn subsets
  "Returns all the subsets of a collection"
  [coll]
  (reduce (fn [a x] (into a (map #(conj % x)) a)) #{#{}} coll))

(defn submaps
  "Returns all the submaps of a map"
  [m]
  (->> m (seq) (subsets) (map (partial into {})) (set)))

(defn indexcat-by
  "Like index-by except f is allowed to return a sequence of keys
  that the element should be indexed by."
  [f coll]
  (reduce #(apply assoc %1 (interleave (f %2) (repeat %2))) {} coll))

(defn groupcat-by
  "Like group-by except f is allowed to return a sequence of keys
  that the element should be bucketed by."
  [f coll]
  (-> (fn [agg x]
        (-> (fn [agg* k]
              (update agg* k (fnil conj []) x))
            (reduce agg (f x))))
      (reduce {} coll)))

(defn index-by-labels
  "Indexes elements in coll according to all of
  the submaps of the map returned by f."
  [f coll]
  (indexcat-by (comp submaps f) coll))

(defn group-by-labels
  "Groups elements in coll according to all of
  the submaps of the map returned by f."
  [f coll]
  (groupcat-by (comp submaps f) coll))