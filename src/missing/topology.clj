(ns missing.topology
  (:require [missing.core :as miss]
            [clojure.set :as sets]))

(defn nodes [g]
  (sets/union (apply sets/union (vals g)) (set (keys g))))

(defn incoming [g]
  (apply sets/union (vals g)))

(defn no-incoming [g]
  (sets/difference (nodes g) (incoming g)))

(defn outgoing [g]
  (->> (miss/filter-vals not-empty g) (keys) (set)))

(defn no-outgoing [g]
  (sets/difference (nodes g) (outgoing g)))

(defn normalize [g]
  (reduce (fn [agg next] (update agg next (fnil set #{}))) g (nodes g)))

(defn traversal [g root]
  (tree-seq #(not-empty (get g %)) #(get g %) root))

(defn topological-sort
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

(defn topological-sort-with-grouping [g]
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