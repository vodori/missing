(ns missing.core
  (:require [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets]
            [clojure.edn :as edn]
            [missing.paths :as paths]
            [clojure.data :as data]
            [clojure.pprint :as pprint]
            [missing.cwm :as cwm])
  (:import (java.util.concurrent TimeUnit)
           (java.util EnumSet UUID)
           (java.time Duration)
           (java.nio.file FileSystems)
           (java.io File)
           (java.security MessageDigest)))

(defn uuid
  "Get a uuid as string"
  [] (str (UUID/randomUUID)))

(defn read-edn-string
  "Reads a string of edn and returns the parsed data. Applies any loaded data
   readers and falls back to propagating tagged literals when no applicable reader exists."
  [s] (edn/read-string {:readers *data-readers* :default tagged-literal} s))

(defn locate-file
  "Given a path attempts to find the best matching file.
     file:      prefix to mandate file system path
     classpath: prefix to mandate classpath
     leading slash presumes file system
     otherwise, check classpath first then filesystem"
  [path]
  (when-some
    [f (and path
            (cond
              (strings/starts-with? path "/")
              (io/file path)
              (strings/starts-with? path "file:")
              (some-> path (strings/replace-first "file:" "") io/file)
              (strings/starts-with? path "classpath:")
              (some-> path (strings/replace-first "classpath:" "") io/resource io/file)
              :otherwise
              (or (locate-file (str "classpath:" path)) (locate-file (str "file:" path)))))]
    (when (.exists f) f)))

(defn load-edn-resource
  "Load and parse an edn file from the filesystem if given an absolute path or the classpath otherwise.
   See read-edn-string for notes on tagged literals."
  [path]
  (when-some [f (locate-file path)]
    (when (.canRead f) (read-edn-string (slurp f)))))

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

(defn filter-entries [pred m]
  (letfn [(f [agg k v] (if (and (pred k) (pred v)) (assoc! agg k v) agg))]
    (persistent! (reduce-kv f (transient (or (empty m) {})) m))))

(defn remove-keys
  "Filter a map by the complement of predicate on its keys"
  [pred m] (filter-keys (complement pred) m))

(defn remove-vals
  "Filter a map by the complement of predicate on its values"
  [pred m] (filter-vals (complement pred) m))

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

(defn map-entries
  "Transform the entries of a map"
  [f m]
  (letfn [(f* [agg k v] (assoc! agg (f k) (f v)))]
    (persistent! (reduce-kv f* (transient (or (empty m) {})) m))))

(defn keep-keys
  "Map and only keep non-nil keys."
  [f m] (->> (map-keys f m) (filter-keys some?)))

(defn keep-vals
  "Map and only keep non-nil values."
  [f m] (->> (map-vals f m) (filter-vals some?)))

(defn keep-entries
  "Map and only keep entries with non-nil keys and values."
  [f m] (->> (map-entries f m) (filter-entries some?)))

(defn reverse-map
  "Invert a map"
  [m] (into {} (map (comp vec reverse)) m))

(defn grouping->pairs
  "Turn a map of groupings into a flat sequence of pairs of key and single value."
  [m] (mapcat #(map vector (repeat (key %)) (val %)) m))

(defn reverse-grouping
  "Take a map of categories to items and turn it into a map of item to category."
  [m]
  (reduce (fn [m' [k v]] (assoc m' v k)) {} (grouping->pairs m)))

(defn pivot-grouping
  "Take a map of categories to items and turn it into a map of items to categories."
  [m]
  (reduce (fn [m' [k v]] (update m' v (fnil conj []) k)) {} (grouping->pairs m)))

(defmacro if-text
  "bindings => binding-form test

   If test is a string and contains text, evaluates then with binding-form bound to the
   value of test, if not, yields else"
  ([bindings then]
   `(if-text ~bindings ~then nil))
  ([bindings then else & oldform]
   (#'clojure.core/assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (or (not (string? temp#)) (strings/blank? temp#))
          ~else
          (let [~form temp#]
            ~then))))))

(defmacro when-text [bindings & body]
  `(if-text ~bindings (do ~@body)))

(defmacro if-seq
  "bindings => binding-form test

   If (seq test) is non-nil evaluates then with binding-form bound to the
   value of test, if not, yields else"
  ([bindings then]
   `(if-seq ~bindings ~then nil))
  ([bindings then else & oldform]
   (#'clojure.core/assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (empty? temp#)
          ~else
          (let [~form temp#]
            ~then))))))

(defmacro when-seq [bindings & body]
  `(if-seq ~bindings (do ~@body)))

(defn not-empty? [coll]
  (boolean (seq coll)))

(defn not-blank? [s]
  (not (strings/blank? s)))

(defn assoc*
  "Like assoc, but assumes associng into nil with an integer
   key means 'I want a vector' and not 'I want a map'"
  ([m k v]
   (assoc (or m (if (int? k) [] {})) k v))
  ([map key val & kvs]
   (reduce (fn [agg [k v]] (assoc* agg k v)) (assoc* map key val) (partition 2 kvs))))

(defn assoc*-in
  "Like assoc-in but with assoc* semantics."
  [m [k & ks] v]
  (if ks
    (assoc* m k (assoc*-in (get m k) ks v))
    (assoc* m k v)))

(defn update*
  "Like update, but with assoc* semantics."
  ([m k f]
   (assoc* m k (f (get m k))))
  ([m k f x]
   (assoc* m k (f (get m k) x)))
  ([m k f x y]
   (assoc* m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc* m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc* m k (apply f (get m k) x y z more))))

(defn update*-in
  "Like update-in, but with assoc* semantics."
  ([m ks f & args]
   (let [up (fn up [m ks f args]
              (let [[k & ks] ks]
                (if ks
                  (assoc* m k (up (get m k) ks f args))
                  (assoc* m k (apply f (get m k) args)))))]
     (up m ks f args))))

(defn fixed-point
  "Finds the fixed point of f given initial input x. Optionally
   provide a max number of iterations to attempt before returning
   nil, else runs indefinitely if no fixed point is found."
  ([f x]
   (loop [[[x1 x2] :as parts]
          (partition 2 1 (iterate f x))]
     (if (= x1 x2) x1 (recur (rest parts)))))
  ([max f x]
   (loop [counter max
          [[x1 x2] :as parts]
          (partition 2 1 (iterate f x))]
     (if (= x1 x2)
       x1
       (when (pos? counter)
         (recur (dec counter) (rest parts)))))))

(defn lstrip
  "Strip a prefix from a string."
  [s strip]
  (let [result
        (if (strings/starts-with? s strip)
          (subs s (.length strip))
          s)]
    (if (and (not= result s) (= 1 (.length strip)))
      (recur result strip)
      result)))

(defn rstrip
  "Strip a suffix from a string."
  [s strip]
  (let [result
        (if (strings/ends-with? s strip)
          (subs s 0 (- (.length s) (.length strip)))
          s)]
    (if (and (not= result s) (= 1 (.length strip)))
      (recur result strip)
      result)))

(defn join-paths
  "Join paths together. Accepts string arguments or collections
   (which will be flattened). '/' delimiters already at the
   beginning or end of a segment will be removed leaving only
   a single '/' between each segment."
  [& paths]
  (letfn [(join [p1 p2]
            (let [part1 (rstrip p1 "/")
                  part2 (lstrip p2 "/")]
              (if-not (strings/blank? part1)
                (str part1 "/" part2)
                part2)))]
    (let [[s1 :as segments] (filter some? (flatten paths))
          naked (rstrip (reduce join "" segments) "/")]
      (if (and s1 (strings/starts-with? s1 "/")) (str "/" naked) naked))))

(defn left-pad
  "Pad a string on the left until it satisfies a desired width."
  [s length pad]
  (let [pad-length (max 0 (- length (.length (str s))))]
    (reduce (fn [ss s] (str s ss)) s (repeat pad-length pad))))

(defn right-pad
  "Pads a string on the right until it satisfies a desired width."
  [s length pad]
  (let [pad-length (max 0 (- length (.length (str s))))]
    (reduce (fn [ss s] (str ss s)) s (repeat pad-length pad))))

(defn index-by
  "Index the items of a collection into a map by a key"
  [key-fn coll] (into {} (map (juxt key-fn identity)) coll))

(def ^:dynamic *preempt*)

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

(defn zip
  "Create tuples from sequences."
  [& colls] (apply map vector colls))

(defmacro letp
  "Like clojure.core/let but allows early returns via (preempt return-value)"
  [bindings & body] `(preemptable (let ~bindings ~@body)))

(defn map-groups
  "Map items in groups for the groups in a map of category to group."
  [f m] (map-vals (partial mapv f) m))

(defn mapcat-groups
  "Mapcat items in groups for the groups in a map of category to group."
  [f m] (map-vals (comp vec (partial mapcat f)) m))

(defn filter-groups
  "Filter items in groups for the groups in a map of category to group."
  [f m] (map-vals (partial filterv f) m))

(defn remove-groups
  "Remove items from groups in a map of category to group."
  [f m] (filter-groups (complement f) m))

(defn reduce-groups
  "Reduce items in groups for the groups in a map of category to group."
  ([f m]
   (map-vals (partial reduce f) m))
  ([f val m]
   (map-vals (partial reduce f val) m)))

(defn iterable?
  "Is collection like or a single value?"
  [x]
  (and (not (string? x))
       (not (map? x))
       (seqable? x)))

(defn single?
  "Is this a collection of a single value?"
  [x] (and (iterable? x)
           (not-empty? x)
           (= 1 (bounded-count 2 x))))

(defn n?
  "Are there exactly n things in coll that satisfy pred?"
  [n pred coll]
  (letfn [(reduction [agg next]
            (let [agg' (if (pred next) (inc agg) agg)]
              (if (< n agg') (reduced agg') agg')))]
    (= n (reduce reduction 0 coll))))

(defn one?
  "Is there exactly one thing in coll that satisfies pred?"
  [pred coll] (n? 1 pred coll))

(defn lasts-by
  "Filter a sequence to only the last elements of each partition determined by key-fn."
  [key-fn coll] (map last (partition-by key-fn coll)))

(defn firsts-by
  "Filter a sequence to only the first elements of each partition determined by key-fn."
  [key-fn coll] (map first (partition-by key-fn coll)))

(defn walk-seq
  "Returns a lazy sequence of all forms within a data structure."
  [form] (cwm/walk-seq form))

(defn paging
  "A function that returns a lazily generating sequence
  backed by a paged source. Takes a function that receives
  an offset and limit and returns the page for those parameters.

    :f A function of two arguments (offset and limit) that fetches some kind of results.

    :limit A number representing how many objects to fetch per page. Defaults to 512.

    :offset A number representing what index to start getting results from. Defaults to 0.
  "
  ([f] (paging 512 f))
  ([limit f] (paging 0 limit f))
  ([offset limit f]
   (let [last-page-size (volatile! 0)]
     (letfn [(augmented-f [offset limit]
               (let [result (f offset limit)]
                 (vreset! last-page-size (count result)) result))
             (fetcher [offset limit]
               (lazy-seq
                 (concat
                   (augmented-f offset limit)
                   (let [c @last-page-size]
                     (if-not (< c limit)
                       (fetcher (+ offset c) limit)
                       '())))))]
       (fetcher offset limit)))))

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

(defmacro default
  "Wrap f to return a default value if f returns nil."
  [f default]
  `(let [f# ~f default# (delay ~default)]
     (fn [& args#] (or (apply f# args#) (force default#)))))

(defmacro defmemo
  "Define a function with a memoized implementation."
  [& defnargs]
  `(doto (defn ~@defnargs)
     (alter-var-root #(with-meta (memoize %1) (meta %1)))))

(defn concatv
  "Returns the concatenation as a vector."
  [& xs] (vec (apply concat xs)))

(defn keyset
  "Returns the keys of a map as a set."
  [m] (set (keys m)))

(defn diff-by
  "Like clojure.data/diff when used on sets except keyed by
   key-fn instead of the elements themselves."
  [key-fn a b]
  (let [indexed-a (group-by key-fn a)
        indexed-b (group-by key-fn b)
        a-keys    (keyset indexed-a)
        b-keys    (keyset indexed-b)
        mapcats   (comp set mapcat)
        [a-only b-only both] (data/diff a-keys b-keys)]
    [(mapcats (default indexed-a []) a-only)
     (mapcats (default indexed-b []) b-only)
     (sets/union (mapcats (default indexed-a []) both)
                 (mapcats (default indexed-b []) both))]))

(defn deep-merge
  "Merges nested maps."
  [& maps]
  (letfn [(inner-merge [& maps]
            (let [ms (remove nil? maps)]
              (if (every? map? ms)
                (apply merge-with inner-merge ms)
                (last maps))))]
    (apply inner-merge maps)))

(defn pp
  "Prints the argument and returns it."
  [x] (pprint/pprint x) x)

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

(defn dfs
  "Depth first search through a form for the first form that matches pred."
  [pred form]
  (find-first pred (walk-seq form)))

(defn find-indexed
  "Returns [index item] for the first item that matches pred."
  [pred coll]
  (->> (map vector (range) coll)
       (drop-while (comp (complement pred) second))
       (first)))

(defn sorted-map-by-value
  "Returns a sorted map with entries sorted by values. Supply
   your own comparator if you want to reverse order or customize
   the sort."
  ([m] (sorted-map-by-value m compare))
  ([m comparator]
   (into (sorted-map-by
           (fn [key1 key2]
             (comparator
               [(get m key1) key1]
               [(get m key2) key2])))
         m)))

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

(defn distinct-by?
  "Like distinct? but according to a key-fn instead of the element itself."
  [f coll]
  (or (empty? coll) (apply distinct? (map f coll))))

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
   (let [ret (volatile! false)]
     (partition-by
       (fn [item]
         (if (pred item)
           (vswap! ret not)
           @ret)))))
  ([pred coll]
   (let [ret (volatile! false)]
     (partition-by
       (fn [item]
         (if (pred item)
           (vswap! ret not)
           @ret)) coll))))


(defn lt
  "Like < but for comparables."
  ([] true)
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
  ([] true)
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
  ([] true)
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
  ([] true)
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

(defn extrema-by
  "Returns a tuple of [smallest largest] in coll according to some fn of an element."
  [f coll]
  (letfn [(reduction
            ([] [[nil nil] [nil nil]])
            ([x] x)
            ([[[_ min-v :as min']
               [_ max-v :as max']]
              [[_ v :as x'] _]]
             [(if (lt v min-v) x' min')
              (if (gt v max-v) x' max')]))
          (mapper [x]
            (let [v [x (f x)]] [v v]))]
    (mapv first (reduce reduction (map mapper coll)))))

(defn least
  "Returns the smallest element in the collection."
  [coll] (least-by identity coll))

(defn greatest
  "Returns the largest element in the collection."
  [coll] (greatest-by identity coll))

(defn extrema
  "Returns a tuple of [smallest largest] element in the collection."
  [coll] (extrema-by identity coll))

(defn filter-nth
  "Filters a seq based on a function of the nth position."
  [n pred coll] (filter (fn [x] (pred (nth x n))) coll))

(defn filter1
  "Filters a seq of tuples on a predicate of the first elements."
  [pred coll] (filter-nth 0 pred coll))

(defn filter2
  "Filters a seq of tuples on a predicate of the second elements."
  [pred coll] (filter-nth 1 pred coll))

(defn map-nth
  "Updates the nth position of each element in a seq of tuples."
  [n f coll] (map (comp (fn [x] (update x n f)) vec) coll))

(defn map1
  "Updates the first position of each element in a seq of tuples."
  [f coll] (map-nth 0 f coll))

(defn map2
  "Updates the second position of each element in a seq of tuples."
  [f coll] (map-nth 1 f coll))

(defmacro keyed
  "Creates a map of keyword => value from symbol names and the values they refer to."
  [& keys]
  `(into {} ~(mapv (fn [k#] [(keyword k#) k#]) keys)))

(defmacro stringed
  "Creates a map of string => value from symbol names and the values they refer to."
  [& keys]
  `(into {} ~(mapv (fn [k#] [(name k#) k#]) keys)))

(defn piecewise
  "Returns a new function that will apply f to respective elements
   across each provided collection. f should be an associative
   function of two elements."
  [f & colls]
  (cond
    (empty? colls)
    ()
    (= 1 (bounded-count 2 colls))
    (seq (first colls))
    :else
    (for [cells (apply zip colls)]
      (reduce f cells))))

(defn pmapcat
  "Like pmap, but for mapcatting."
  ([f coll] (mapcat identity (pmap f coll)))
  ([f coll & colls] (mapcat identity (apply pmap f coll colls))))

(defmacro atomic-init!
  "Used to update a reference type to acquire a 'place' and only
   after acquiring the place does it run the code to initialize
   the value."
  [ref path & body]
  `(let [path#    ~path
         updater# (fn [x#] (or x# (delay ~@body)))
         updated# (swap! ~ref update-in path# updater#)]
     (force (get-in updated# path#))))

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
  [& body] `(try ~@body (catch Throwable _# nil)))

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

(defn polling-atom
  "Returns an atom backed by background polling of a function. Change
   the value of the atom to :stop in order to halt the polling process.
   Exceptions during the invocation of the process log but will not mutate
   the value. Supply your own reducer to combine results of successive polls."
  ([freq f]
   (polling-atom {} freq f))
  ([init freq f]
   (polling-atom #(identity %2) init freq f))
  ([reducer init freq f]
   (let [state  (atom init)
         stop   (atom false)
         millis (if (instance? Duration freq) (.toMillis freq) freq)]
     (add-watch state :closer #(when (= :stop %4) (reset! stop true)))
     (doto (Thread.
             ^Runnable
             (fn []
               (let
                 [stop? @stop
                  [time]
                  (timing
                    (quietly
                      (let [[success? result] (with-timeout millis (f))]
                        (when success? (swap! state reducer result)))))]
                 (Thread/sleep (max (- millis time) 0))
                 (when-not stop? (recur)))))
       (.setDaemon true)
       (.start))
     state)))

(defn glob-matcher
  "Returns a predicate that will match a file against a glob pattern."
  ([glob]
   (glob-matcher (io/file ".") glob))
  ([dir glob]
   (let [matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" glob))]
     (fn [& args]
       (->> (.toPath ^File (apply io/file args))
            (.relativize (.toPath ^File (io/file dir)))
            (.matches matcher))))))

(defn glob-seq
  "Returns a sequence of files and directories nested within dir that match one of the provided glob patterns."
  [dir & globs]
  (let [rr (io/file dir)]
    (if (empty? globs)
      (file-seq rr)
      (let [pred (apply some-fn (map (partial glob-matcher rr) globs))]
        (->> (file-seq rr)
             (filter pred)
             (sort-by #(.getPath %)))))))

(defn run-par!
  "Like run! but executes each element concurrently."
  [f coll] (run! deref (doall (map #(future (f %)) coll))))

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
  [filename] (first (re-find #"(\.[^.]*)$" filename)))

(defn get-filename
  "Get the filename (without extension) from a filename"
  [filename] (second (re-find #"(.+?)(\.[^.]*$|$)" filename)))

(defn uniqueifier
  "Returns a function that will always produce a unique name
  for any provided name. The first time it will be the name as-is
  and subsequent calls will append some count. You're guaranteed that
  the range of the produced function has zero duplicates. When these
  names are filenames the number is appended conveniently before the extension."
  []
  (let [seen-names
        (volatile! #{})
        get-modified
        (fn [name]
          (let [extension (or (get-extension name) "")
                filename  (get-filename name)
                seen      @seen-names]
            (->> (range)
                 (map #(str filename "(" (inc %) ")" extension))
                 (filter #(not (contains? seen %)))
                 (first))))]
    (fn [name]
      (if (contains? @seen-names name)
        (recur (get-modified name))
        (do (vswap! seen-names conj name)
            name)))))

(defn subsets
  "Returns all the subsets of a collection"
  [coll] (reduce (fn [a x] (into a (map #(conj % x)) a)) #{#{}} coll))

(defn symmetric-difference
  "Returns the union of the exclusive portions of s1 and s2."
  [s1 s2] (sets/union (sets/difference s1 s2) (sets/difference s2 s1)))

(defn submaps
  "Returns all the submaps of a map"
  [m] (->> m (seq) (subsets) (map (partial into {})) (set)))

(defn submap?
  "Is m1 a submap of m2?"
  [m1 m2] (sets/subset? (set (seq m1)) (set (seq m2))))

(defn supermap?
  "Is m1 a supermap of m2?"
  [m1 m2] (sets/superset? (set (seq m1)) (set (seq m2))))

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
  "Given a map or sequence of [key-fn coll] pairs, create a
   lookup table from disparate data sets. Define how to compute
   the primary key from each set and it'll give you back a map
   of primary key to vector of 'columns'."
  [f+colls]
  (let [idxs (mapv #(index-by (first %) (second %)) f+colls)]
    (->> (set (mapcat keys idxs))
         (map (juxt identity #(mapv (fn [idx] (get idx %)) idxs)))
         (into {}))))

(defn paths
  "Returns all the paths into a data structure. Paths are compatible
   with `(get-in form path)`."
  [form]
  (map second (paths/path-seq form)))

(defn index-values-by-paths
  "Returns a map of path => value at path for any data structure"
  [form]
  (->> (paths/path-seq form) (map (comp vec reverse)) (into {})))

(defn structural-extractor
  "Given any clojure structure, return a function that will extract
  that same structure from data that may only share part of the
  structure (or may have more than the original structure)."
  [structure]
  (let [paths (paths structure)]
    (fn [form]
      (letfn [(reducer [agg path]
                (assoc*-in agg path (get-in form path)))]
        (reduce reducer (empty structure) paths)))))

(defn select-structure
  "Like select-keys except mimics the structure provided by the second argument."
  [m structure]
  ((structural-extractor structure) m))

(defn =ic
  "Like = but ignores casing."
  ([_] true)
  ([s1 s2]
   (= (when s1 (strings/lower-case s1))
      (when s2 (strings/lower-case s2))))
  ([s1 s2 & ss]
   (if (=ic s1 s2)
     (if (next ss)
       (recur s2 (first ss) (rest ss))
       (=ic s2 (first ss)))
     false)))

(defn =select
  "Checks equality at only the positions referred to by expected. If
   a leaf of expected is a predicate, then that predicate will be called
   with the value found in actual, otherwise equality will be used."
  [expected actual]
  (loop [[[value path] & remaining :as paths] (paths/path-seq expected)]
    (if (empty? paths)
      true
      (cond
        (and (fn? value) (value (get-in actual path)))
        (recur remaining)
        (= value (get-in actual path))
        (recur remaining)
        :otherwise
        false))))

(defn bytes->hex
  "Converts a byte array into a hex encoded string."
  [bytes]
  (loop [buf (StringBuffer.) counter 0]
    (if (= (alength bytes) counter)
      (.toString buf)
      (let [hex (Integer/toHexString (bit-and 0xff (aget bytes counter)))]
        (recur (.append buf (left-pad hex 2 "0")) (inc counter))))))

(defn string->md5-hex
  "Hashes a string and returns the md5 checksum (hex encoded)"
  [s]
  (-> "MD5"
      (MessageDigest/getInstance)
      (.digest (.getBytes s))
      (bytes->hex)))

(defmacro once
  "Runs a piece of code that evaluates only once (per ns) until the source changes."
  [& body]
  (let [sym (symbol (string->md5-hex (pr-str &form)))]
    `(do (defonce ~sym ~@body) (var-get (var ~sym)))))

(defmacro defonce-protocol
  "Like defprotocol but won't reload the protocol when you reload the ns."
  [sym & body]
  `(once (defprotocol ~sym ~@body)))

(defmacro defmethodset
  "Like defmethod but allows for specifying implementations of multiple dispatch keys at once."
  [symbol dispatch-keys & body]
  `(doseq [dispatch# ~dispatch-keys] (defmethod ~symbol dispatch# ~@body)))

(defmacro letd
  "Like clojure.core/let except delays and forces each binding value. Use
   this when you don't want to evaluate potentially expensive bindings
   until you refer to their value in the body of the code. Supports nesting,
   dependencies between bindings, shadowing, destructuring, and closures."
  [bindings & body]
  (#'clojure.core/assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (letfn [(reduction [{:keys [replacements] :as agg} [symbol value]]
            (let [new-form (cwm/replace-symbols-except-where-shadowed replacements value)]
              (-> agg
                  (update :bindings conj [symbol (list `delay new-form)])
                  (update :replacements assoc symbol (list `force symbol)))))]
    (let [{:keys [bindings replacements]}
          (reduce reduction
                  {:bindings [] :replacements {}}
                  (partition 2 (destructure bindings)))]
      `(let* ~(vec (mapcat identity bindings))
         ~@(cwm/replace-symbols-except-where-shadowed replacements body)))))