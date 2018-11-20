(ns missing.topology
  "Basic graph functions to inquire a graph in adjacency list form."
  (:require [missing.core :as miss]
            [clojure.set :as sets]))

(defn nodes
  "Get all nodes from the graph g."
  [g]
  (sets/union (apply sets/union (vals g)) (set (keys g))))

(defn incoming
  "Get all nodes with inbound edges."
  [g]
  (apply sets/union (vals g)))

(defn no-incoming
  "Get all nodes with no inbound edges."
  [g]
  (sets/difference (nodes g) (incoming g)))

(defn outgoing
  "Get all nodes with outbound edges."
  [g]
  (->> (miss/filter-vals not-empty g) (keys) (set)))

(defn no-outgoing
  "Get all nodes with no outbound edges."
  [g]
  (sets/difference (nodes g) (outgoing g)))

(defn edges
  "Get the set of edges of graph g."
  [g]
  (set (mapcat (fn [[k v]] (map vector (repeat k) v)) (seq g))))

(defn supergraph?
  "Is g1 a supergraph of g2?"
  [g1 g2]
  (sets/subset? (edges g2) (edges g1)))

(defn subgraph?
  "Is g1 a subgraph of g2?"
  [g1 g2]
  (sets/subset? (edges g1) (edges g2)))

(defn normalize
  "Given an adjacency with potentially missing entries, populate
   the entries based on the appearance of other nodes on the right
   side of an edge."
  [g]
  (reduce (fn [agg next] (update agg next (fnil set #{}))) g (nodes g)))

(defn traversal
  "Return a depth first traversal of the graph, beginning at node start."
  [g start]
  (tree-seq #(not-empty (get g % #{})) #(get g % #{}) start))

(defn topological-sort
  "Apply a topological sort to an adjacency map"
  ([g]
   (let [normalized (normalize g)]
     (topological-sort normalized [] (no-incoming normalized))))
  ([g l s]
   (if (empty? s)
     (when (every? empty? (vals g)) l)
     (let [[n s'] (let [item (first s)]
                    [item (disj s item)])
           m  (g n)
           g' (reduce #(update-in % [n] disj %2) g m)]
       (recur g' (conj l n) (sets/union s' (sets/intersection (no-incoming g') m)))))))

(defn topological-sort-with-grouping
  "Apply a topological sort to an adjacency map and partition items into sets when order is arbitrary."
  [g]
  (let [normalized (normalize g)]
    (loop [nodes (no-incoming normalized) counts {} level 0]
      (if (not-empty nodes)
        (recur (mapcat normalized nodes)
               (reduce (fn [agg node]
                         (if-some [existing (get agg node)]
                           (let [new-count (max existing level)]
                             (if (not= existing new-count)
                               (let [delta (- new-count existing)]
                                 (->> (traversal normalized node)
                                      (remove (partial = node))
                                      (reduce (fn [agg next] (update agg next (fnil + 0) delta)) agg)))
                               agg))
                           (assoc agg node level)))
                       counts nodes)
               (inc level))
        (->> (topological-sort normalized)
             (partition-by counts)
             (map set))))))