(ns missing.topology
  "Simple graph functions for graphs in adjacency map form."
  (:require [missing.core :as miss]
            [clojure.set :as sets]))

(defn normalize
  "Given an adjacency with potentially missing entries, populate
   the entries based on the appearance of other nodes on the right
   side of an edge. Tags the result with metadata so it doesn't
   recompute for repeat invocations."
  [g]
  (if (some-> g meta ::normalized)
    g
    (letfn [(nodes [g] (apply sets/union (set (keys g)) (map set (vals g))))]
      (let [result (reduce (fn [agg next] (update agg next (fnil set #{}))) g (nodes g))]
        (vary-meta result (fnil merge {}) {::normalized true})))))

(defn nodes
  "Get all nodes from the graph g."
  [g] (set (keys (normalize g))))

(defn edges
  "Get the set of edges of graph g."
  [g] (->> g (normalize) (mapcat (fn [[k v]] (map vector (repeat k) v))) (set)))

(defn incoming
  "Get all nodes with inbound edges."
  [g] (apply sets/union (vals (normalize g))))

(defn no-incoming
  "Get all nodes with no inbound edges."
  [g] (let [g* (normalize g)]
        (sets/difference (nodes g*) (incoming g*))))

(defn outgoing
  "Get all nodes with outbound edges."
  [g] (->> (miss/filter-vals not-empty g) (keys) (set)))

(defn no-outgoing
  "Get all nodes with no outbound edges."
  [g] (let [g* (normalize g)]
        (sets/difference (nodes g*) (outgoing g*))))

(defn graph
  "Create a graph from a set of edges"
  [edges]
  (letfn [(f [m [n1 n2]] (update m n1 (fnil conj #{}) n2))]
    (normalize (reduce f {} (set edges)))))

(defn inverse
  "Invert the graph by reversing all edges."
  [g] (->> (edges g) (map (comp vec reverse)) (graph)))

(defn union
  "Union two graphs together."
  [g1 g2] (graph (sets/union (edges g1) (edges g2))))

(defn intersection
  "Intersect two graphs."
  [g1 g2] (graph (sets/intersection (edges g1) (edges g2))))

(defn difference
  "Subtract g2 from g1."
  [g1 g2] (graph (sets/difference (edges g1) (edges g2))))

(defn symmetric-difference
  "Returns the union of the exclusive sections of g1 and g2."
  [g1 g2] (union (difference g1 g2) (difference g2 g1)))

(defn outgoing-edges
  "Get all edges that go from n to another node."
  [g n]
  (let [g* (normalize g)]
    (set (map vector (repeat n) (get g* n #{})))))

(defn incoming-edges
  "Get all edges that go from another node to n."
  [g n] (->> (edges g) (filter #(= (second %) n)) (set)))

(defn intersect?
  "Returns whether the graphs overlap."
  [g1 g2] (not (empty? (intersection g1 g2))))

(defn exclusive?
  "Returns whether the graphs don't overlap."
  [g1 g2] (empty? (intersection g1 g2)))

(defn supergraph?
  "Is g1 a supergraph of g2?"
  [g1 g2] (sets/subset? (edges g2) (edges g1)))

(defn subgraph?
  "Is g1 a subgraph of g2?"
  [g1 g2] (supergraph? g2 g1))

(defn traversal
  "Return a depth first traversal of the graph, beginning at node start."
  [g start] (tree-seq #(not-empty (get g % #{})) #(get g % #{}) start))

(defn incoming-neighbors
  "Gets the neighbors of n from any inbound edges."
  [g n]
  (->> (incoming-edges g n)
       (map first)
       (set)))

(defn outgoing-neighbors
  "Gets the neighbors of n from any outbound edges."
  [g n]
  (->> (outgoing-edges g n)
       (map second)
       (set)))

(defn neighbors
  "Gets all the neighbors of n."
  [g n]
  (sets/union
    (incoming-neighbors g n)
    (outgoing-neighbors g n)))

(defn incoming-degree
  "Gets the incoming degree of n."
  [g n]
  (count (incoming-edges g n)))

(defn outgoing-degree
  "Gets the outgoing degree of n."
  [g n]
  (count (outgoing-edges g n)))

(defn source?
  "Is n a node with no incoming edges?"
  [g n]
  (zero? (incoming-degree g n)))

(defn sink?
  "Is n a node with no outgoing edges?"
  [g n]
  (zero? (outgoing-degree g n)))

(defn degree
  "How many edges does this node have?"
  [g n]
  (let [g* (normalize g)]
    (+ (incoming-degree g* n)
       (outgoing-degree g* n))))

(defn walk?
  "Check if the given walk is valid for the graph."
  [g [x1 x2 & xs]]
  (let [g* (normalize g)]
    (cond
      (nil? x1) false
      (nil? x2) (contains? g* x1)
      (not (contains? (get g* x1) x2)) false
      (empty? xs) true
      :otherwise (recur g* (cons x2 xs)))))

(defn topological-sort-with-grouping
  "Returns a topological sort of the adjacency map and
   partition items into sets where order is arbitrary."
  [g]
  (loop [g* (normalize g) results []]
    (let [nnodes (no-incoming g*)]
      (if (empty? nnodes)
        (when (= (set (mapcat identity results)) (nodes g))
          results)
        (recur
          (apply dissoc g* nnodes)
          (conj results (set nnodes)))))))

(defn topological-sort
  "Returns a topological sort of the adjacency map"
  ([g]
   (some->>
     (topological-sort-with-grouping g)
     (mapcat identity)
     (vec))))

(defn cyclical?
  "Are there cycles in this graph?"
  [g]
  (nil? (topological-sort g)))
