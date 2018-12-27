(ns missing.core
  (:require [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets]
            [clojure.edn :as edn]
            [clojure.walk :as walk])
  (:import (java.util.concurrent TimeUnit)
           (java.util EnumSet UUID)
           (java.time Duration)
           (java.util.regex Pattern)))

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
  (letfn [(f [agg k v] (if (pred k) (assoc! agg k v) agg))]
    (persistent! (reduce-kv f (transient (or (empty m) {})) m))))

(defn filter-vals
  "Filter a map by a predicate on its values"
  [pred m]
  (letfn [(f [agg k v] (if (pred v) (assoc! agg k v) agg))]
    (persistent! (reduce-kv f (transient (or (empty m) {})) m))))

(defn remove-keys
  "Filter a map by the complement of predicate on its keys"
  [pred m]
  (filter-keys (complement pred) m))

(defn remove-vals
  "Filter a map by the complement of predicate on its values"
  [pred m]
  (filter-vals (complement pred) m))

(defn map-keys
  "Transform the keys of a map"
  [f m]
  (letfn [(f* [agg k v] (assoc! agg (f k) v))]
    (persistent! (reduce-kv f* (transient (or (empty m) {})) m))))

(defn map-vals
  "Transform the values of a map"
  [f m]
  (letfn [(f* [agg k v] (assoc! agg k (f v)))]
    (persistent! (reduce-kv f* (transient (or (empty m) {})) m))))

(defn reverse-map
  "Invert a map"
  [m]
  (into {} (map (comp vec reverse)) m))

(defn invert-grouping
  "Take a map of categories to items and turn it into a map of item to category."
  [m]
  (->> m (mapcat #(map vector (val %) (repeat (key %)))) (into {})))

(defn not-empty? [coll]
  ((complement empty?) coll))

(defn not-blank? [s]
  ((complement strings/blank?) s))

(defn lstrip
  "Strip a prefix from a string."
  [s strip]
  (let [re (format "^(%s)+" (Pattern/quote strip))]
    (strings/replace s (Pattern/compile re) "")))

(defn rstrip
  "Strip a suffix from a string."
  [s strip]
  (let [re (format "(%s)+$" (Pattern/quote strip))]
    (strings/replace s (Pattern/compile re) "")))

(defn join-paths
  "Join paths together. Accepts string arguments or collections
   (which will be flattened). '/' delimiters already at the
   beginning or end of a segment will be removed leaving only
   a single '/' between each segment."
  [& paths]
  (letfn [(join [p1 p2]
            (let [part1 (rstrip p1 "/") part2 (lstrip p2 "/")]
              (if-not (strings/blank? part1)
                (str part1 "/" part2)
                part2)))]
    (let [[s1 :as segments] (filter some? (flatten paths))
          naked (rstrip (reduce join "" segments) "/")]
      (if (and s1 (strings/starts-with? s1 "/")) (str "/" naked) naked))))

(defn index-by
  "Index the items of a collection into a map by a key"
  [key-fn coll]
  (into {} (map (juxt key-fn identity)) coll))

(def ^:dynamic *preempt* nil)

(defn preempt
  "To be used inside of a preemptable. Call preempt within a preemptable
  to deliver a return value for the entire preemptable and abort further
  computation by unwinding the stack with an exception."
  [result]
  (if (thread-bound? #'*preempt*)
     (throw (ex-info "" {::stone (reset! *preempt* result)}))
     (throw (ex-info "Cannot preempt code not wrapped in preemptable!" {}))))

(defmacro preemptable
  "Mark a point in the stack that can be the target of a preemption.
   Calling preempt within a preemtable will result in the value of
   the preemptable being the value passed to preempt if called, otherwise
   the value of the preemptable will be the result of the complete evaluation
   of the interior."
  [& body]
  `(binding [*preempt* (atom ::none)]
     (try
       (let [result# (do ~@body) stone# @*preempt*]
         (if (= stone# ::none) result# stone#))
       (catch Exception e#
         (let [stone# @*preempt*]
           (if (= stone# ::none) (throw e#) stone#))))))

(defn dfs-preorder
  "Depth first search (preorder) through a form for the first form that matches pred."
  [pred form]
  (preemptable (walk/prewalk #(if (pred %) (preempt %) %) form)))

(defn dfs-postorder
  "Depth first search (postorder) through a form for the first form that matches pred."
  [pred form]
  (preemptable (walk/postwalk #(if (pred %) (preempt %) %) form)))

(defn none? [pred coll]
  (every? (complement pred) coll))

(defn contains-all?
  "Does coll contain every key?"
  [coll [k & more :as keys]]
  (if (nil? (seq keys))
    true
    (let [res (contains? coll k)]
      (if (or (false? res) (empty? more))
        res
        (recur coll more)))))

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
  "Returns true if the provided collections are mutually exclusive."
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

(defn find-indexed
  "Returns [index item] for the first item that matches pred."
  [pred coll]
  (->> (map vector (range) coll)
       (drop-while (comp (complement pred) second))
       (first)))

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


(defn partition-with
  "Returns a lazy sequence of partitions where a new
   partition is created every time pred returns true.
   Returns a transducer when only provided pred."
  ([pred]
   (let [ret (volatile! 0)]
     (partition-by
       (fn [item]
         (if (pred item)
           (vswap! ret inc)
           @ret)))))
  ([pred coll]
   (let [ret (volatile! 0)]
     (partition-by
       (fn [item]
         (if (pred item)
           (vswap! ret inc)
           @ret)) coll))))


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
  `(let [future# (future ~@body)
         result# (deref future# ~millis ::aborted)]
     (if (= ::aborted result#)
       (if-not (future-cancel future#)
         (let [inner# (deref future# 0 ::aborted)]
           (if (= ::aborted inner#)
             [false nil]
             [true inner#]))
         [false nil])
       [true result#])))

(defmacro timing
  "Returns a vector of [millis-taken result]"
  [& body]
  `(let [start#  (System/nanoTime)
         result# (do ~@body)
         stop#   (System/nanoTime)]
     [(/ (- stop# start#) (double 1E6)) result#]))

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

(defn duration-parts
  "Given millis or a java.time.Duration return a map of time unit
   to amount of time in that unit. Bucket the duration into larger
   time units before smaller time units."
  [duration]
  (let [nanos
        (.toNanos
          (if (instance? Duration duration)
            duration (Duration/ofMillis duration)))]
    (loop [aggregate (array-map) [this-unit & other-units] (reverse (EnumSet/allOf TimeUnit)) remainder nanos]
      (let [in-unit (.convert this-unit remainder TimeUnit/NANOSECONDS)]
        (let [updated  (assoc aggregate (keyword (strings/lower-case (.name this-unit))) in-unit)
              leftover (- remainder (.convert TimeUnit/NANOSECONDS in-unit this-unit))]
          (if (empty? other-units) updated (recur updated other-units leftover)))))))

(defn duration-explain
  "Converts millis or a java.time.Duration into a human readable description."
  [duration]
  (letfn [(reduction [text [unit amount]]
            (let [base-name (apply str (butlast (name unit)))]
              (->> [text (format (if (> amount 1) "%d %ss" "%d %s") amount base-name)]
                   (remove strings/blank?)
                   (strings/join ", "))))]
    (->> (duration-parts duration)
         (filter (comp pos? val))
         (reduce reduction ""))))

(defn get-extension
  "Get the file extension from a filename."
  [filename]
  (first (re-find #"(\.[^.]*)$" filename)))

(defn get-filename
  "Get the filename (without extension) from a filename"
  [filename]
  (second (re-find #"(.+?)(\.[^.]*$|$)" filename)))

(defn subsets
  "Returns all the subsets of a collection"
  [coll]
  (reduce (fn [a x] (into a (map #(conj % x)) a)) #{#{}} coll))

(defn symmetric-difference
  "Returns the union of the exclusive portions of s1 and s2."
  [s1 s2] (sets/union (sets/difference s1 s2) (sets/difference s2 s1)))

(defn submaps
  "Returns all the submaps of a map"
  [m]
  (->> m (seq) (subsets) (map (partial into {})) (set)))

(defn indexcat-by
  "Like index-by except f is allowed to return a sequence of keys
  that the element should be indexed by."
  [f coll]
  (->> coll
       (mapcat #(map vector (f %) (repeat %)))
       (into {})))

(defn groupcat-by
  "Like group-by except f is allowed to return a sequence of keys
  that the element should be bucketed by."
  [f coll]
  (->> coll
       (mapcat #(map vector (f %) (repeat %)))
       (reduce (fn [m [k v]] (update m k (fnil conj []) v)) {})))

(defn group-by-labels
  "Groups elements in coll according to all of
  the submaps of the map returned by f."
  [f coll]
  (groupcat-by (comp submaps f) coll))

(defn collate
  "Given sequence of [key-fn coll] pairs, create a lookup table
   from disparate data sets. Define how to compute the
   primary key from each set and it'll give you back a map
   of rows indexed by the row key."
  [f+colls]
  (let [idxs (mapv (partial apply index-by) f+colls)]
    (->> (set (mapcat keys idxs))
         (map (juxt identity #(mapv (fn [idx] (get idx %)) idxs)))
         (into {}))))