(ns missing.topology
  "Simple graph functions for graphs in adjacency map form."
  (:refer-clojure :exclude (empty complement))
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

(defmacro defgn
  "Like defn but handles normalizing any 'g or g{digit}' arguments first."
  [symbol docs bindings & body]
  `(defn ~symbol ~docs ~bindings
     (let [~@(->> bindings
                  (miss/walk-seq)
                  (filter symbol?)
                  (filter #(re-find #"^g\d*$" (name %)))
                  (mapcat (fn [k] [k `(normalize ~k)])))]
       ~@body)))

(defgn nodes
  "Get all nodes from the graph g."
  [g] (set (keys g)))

(defgn edges
  "Get the set of edges of graph g."
  [g] (set (mapcat (fn [[k v]] (map vector (repeat k) v)) g)))

(defgn empty
  "Returns a graph with the same nodes as g but no edges."
  [g] (zipmap (nodes g) (repeat #{})))

(defgn consumers
  "Get all nodes with inbound edges."
  [g] (apply sets/union (vals g)))

(defgn producers
  "Get all nodes with outbound edges."
  [g] (->> (miss/filter-vals not-empty g) (keys) (set)))

(defgn sources
  "Get all nodes with no inbound edges."
  [g] (sets/difference (nodes g) (consumers g)))

(defgn sinks
  "Get all nodes with no outbound edges."
  [g] (sets/difference (nodes g) (producers g)))

(defgn exterior
  "Returns the exterior nodes of the graph."
  [g] (sets/union (sources g) (sinks g)))

(defgn interior
  "Returns the interior nodes of the graph."
  [g] (sets/difference (nodes g) (exterior g)))

(defn graph
  "Create a graph from a set of edges"
  ([edges]
   (graph (set (mapcat identity edges)) edges))
  ([nodes edges]
   (letfn [(reduction [m [n1 n2]]
             (if (and (contains? nodes n1) (contains? nodes n2))
               (update m n1 (fnil conj #{}) n2)
               m))]
     (let [init (into {} (map vector nodes (repeat #{})))]
       (normalize (reduce reduction init (set edges)))))))

(defgn walk?
  "Check if the given walk is valid for the graph."
  [g [x1 x2 & xs]]
  (cond
    (nil? x1) false
    (nil? x2) (contains? g x1)
    (not (contains? (get g x1) x2)) false
    (empty? xs) true
    :otherwise (recur g (cons x2 xs))))

(defgn append-edge
  "Appends an edge to a graph."
  [g [from to]]
  (update g from (fnil conj #{}) to))

(defgn remove-edge
  "Removes an edge from a graph."
  [g [from to]]
  (if (contains? g from)
    (update g from (fnil disj #{}) to)
    g))

(defgn append-path
  "Appends a path onto a graph."
  [g path] (reduce append-edge g (partition 2 1 path)))

(defgn remove-path
  "Removes a path from a graph."
  [g path] (reduce remove-edge g (partition 2 1 path)))

(defgn inverse
  "Invert the graph by reversing all edges."
  [g] (graph (nodes g) (map (comp vec reverse) (edges g))))

(defgn complete
  "Returns the fully connected variant of g"
  [g]
  (let [vertices (nodes g)]
    (graph vertices
           (for [v vertices
                 o (disj vertices v)]
             [v o]))))

(defgn complete?
  "Is the graph complete?"
  [g] (= g (complete g)))

(defgn union
  "Union two graphs together."
  [g1 g2]
  (graph
    (sets/union (nodes g1) (nodes g2))
    (sets/union (edges g1) (edges g2))))

(defgn intersection
  "Intersect two graphs."
  [g1 g2]
  (graph
    (sets/intersection (nodes g1) (nodes g2))
    (sets/intersection (edges g1) (edges g2))))

(defgn difference
  "Subtract g2 from g1."
  [g1 g2]
  (graph
    (sets/difference (nodes g1) (nodes g2))
    (sets/difference (edges g1) (edges g2))))

(defgn complement
  "Returns the difference between fully connected g and g"
  [g] (graph (nodes g) (sets/difference (edges (complete g)) (edges g))))

(defgn bidirectional
  "Returns a new graph where all edges go both directions."
  [g] (union g (inverse g)))

(defgn symmetric-difference
  "Returns the union of the exclusive sections of g1 and g2."
  [g1 g2] (union (difference g1 g2) (difference g2 g1)))

(defgn outgoing-edges
  "Get all edges that go from n to another node."
  [g n] (set (map vector (repeat n) (get g n #{}))))

(defgn incoming-edges
  "Get all edges that go from another node to n."
  [g n] (->> (edges g) (filter #(= (second %) n)) (set)))

(defgn intersect?
  "Returns whether the graphs overlap."
  [g1 g2] (miss/intersect? (nodes g1) (nodes g2)))

(defgn exclusive?
  "Returns whether the graphs don't overlap."
  [g1 g2] (miss/exclusive? (nodes g1) (nodes g2)))

(defgn supergraph?
  "Is g1 a supergraph of g2?"
  [g1 g2]
  (and
    (sets/superset? (nodes g1) (nodes g2))
    (sets/superset? (edges g1) (edges g2))))

(defgn subgraph?
  "Is g1 a subgraph of g2?"
  [g1 g2]
  (and
    (sets/subset? (nodes g1) (nodes g2))
    (sets/subset? (edges g1) (edges g2))))

(defgn incoming-neighbors
  "Gets the neighbors of n from any inbound edges."
  [g n]
  (->> (incoming-edges g n)
       (map first)
       (set)))

(defgn outgoing-neighbors
  "Gets the neighbors of n from any outbound edges."
  [g n]
  (->> (outgoing-edges g n)
       (map second)
       (set)))

(defgn neighbors
  "Gets all the neighbors of n."
  [g n]
  (sets/union
    (incoming-neighbors g n)
    (outgoing-neighbors g n)))

(defgn incoming-degree
  "Gets the incoming degree of n."
  [g n]
  (count (incoming-edges g n)))

(defgn outgoing-degree
  "Gets the outgoing degree of n."
  [g n]
  (count (outgoing-edges g n)))

(defgn source?
  "Is n a node with no incoming edges?"
  [g n]
  (zero? (incoming-degree g n)))

(defgn sink?
  "Is n a node with no outgoing edges?"
  [g n]
  (zero? (outgoing-degree g n)))

(defgn degree
  "How many edges does this node have?"
  [g n]
  (+ (incoming-degree g n)
     (outgoing-degree g n)))

(defgn depth-first
  "Returns a depth first traversal of g beginning at start."
  [g start]
  (let [state (atom #{})]
    (letfn [(branch? [x]
              (let [[old] (swap-vals! state conj x)]
                (if (contains? old x) false (contains? g x))))
            (walk [node]
              (lazy-seq
                (cons node
                      (when (branch? node)
                        (mapcat walk (get g node #{}))))))]
      (if (contains? g start)
        (walk start)
        ()))))

(defgn connected?
  "Can you navigate from any starting node to any other node?"
  [g]
  (let [all (nodes g)]
    (loop [[x & remaining :as vertices] all]
      (cond
        (empty? vertices)
        true
        (= (set (depth-first g x)) all)
        (recur remaining)
        :otherwise
        false))))

(defgn topological-sort-with-grouping
  "Returns a topological sort of the adjacency map and
   partition items into sets where order is arbitrary."
  [g]
  (let [all-nodes (nodes g)]
    (loop [g* g results []]
      (let [nnodes (sources g*)]
        (if (empty? nnodes)
          (when (= (set (mapcat identity results)) all-nodes)
            results)
          (recur
            (apply dissoc g* nnodes)
            (conj results (set nnodes))))))))

(defgn topological-sort
  "Returns a topological sort of the adjacency map"
  [g]
  (some->>
    (topological-sort-with-grouping g)
    (mapcat identity)
    (vec)))

(defgn root
  "Returns the root of the graph if any, else nil."
  [g]
  (let [sources (sources g)]
    (when (= 1 (count sources))
      (first sources))))

(defgn tree?
  "Is this graph a tree?"
  [g]
  (let [root   (root g)
        others (disj (nodes g) root)]
    (and (some? root) (every? #(= 1 (incoming-degree g %)) others))))

(defgn leaves
  "Returns the leaves of the tree."
  [g] (sinks g))

(defgn branches
  "Returns the branches of the tree."
  [g] (sets/difference (nodes g) (leaves g)))

(defgn cyclical?
  "Are there cycles in this graph?"
  [g] (nil? (topological-sort g)))

(defgn transitive-closure
  "Creates new edges on g such that every node connects directly to any
   node that could previously have been connected indirectly."
  [g]
  (letfn [(expand-one [k g* v]
            (update g* k (fnil sets/union #{}) (get g* v #{})))
          (expand-entry [g* [k vs]]
            (reduce (partial expand-one k) g* vs))]
    (loop [graph* g]
      (let [expanded (reduce expand-entry graph* graph*)]
        (if (= expanded graph*) expanded (recur expanded))))))

(defgn filterg
  "Only keep nodes in the graph that satisfy pred. All
   inbound and outbound edges involving removed nodes are
   also removed."
  [pred g]
  (->> g
       (miss/filter-groups pred)
       (miss/filter-keys pred)
       (normalize)))

(defgn removeg
  "Remove nodes in the graph that satisfy pred. All
   inbound and outbound edges involving removed nodes
   are also removed."
  [pred g] (filterg (clojure.core/complement pred) g))

(defgn contractg
  "Removes nodes that match pred and adds new edges between
   inbound and outbound neighbors of each node impacted
   (leaves the transitive closure otherwise intact)."
  [pred g]
  (let [pairs    (edges g)
        verts    (nodes g)
        outbound (group-by first pairs)
        inbound  (group-by second pairs)
        purgeset (set (filter pred verts))
        edges-   (sets/union
                   (set (mapcat outbound purgeset))
                   (set (mapcat inbound purgeset)))
        edges+   (set
                   (for [purge purgeset
                         [_ o] (get outbound purge)
                         [i _] (get inbound purge)]
                     [i o]))]
    (graph (sets/difference verts purgeset)
           (-> pairs
               (sets/difference edges-)
               (sets/union edges+)))))

(defgn mapg
  "Transform nodes in the graph according to f."
  [f g]
  (->> (for [[k vs] g
             :let [k' (f k)]
             v vs
             :let [v' (f v)]]
         [k' v'])
       (reduce (fn [g' [k v]] (update g' k (fnil conj #{}) v)) {})
       (normalize)))

(defgn expandg
  "Expand nodes into more nodes while maintaining all the same edges
   between any new nodes."
  [f g]
  (let [memof (memoize f)]
    (->> (for [[k vs] g
               :let [ks' (memof k)]
               v  vs
               :let [vs' (memof v)]
               k' ks'
               v' vs']
           [k' v'])
         (reduce (fn [g' [k v]] (update g' k (fnil conj #{}) v)) {})
         (normalize))))

(defgn select
  "Return the subgraph of g that is within reach of 'from'"
  [g from]
  (miss/if-seq [extent (get (transitive-closure g) from)]
    (filterg (conj extent from) g)
    {}))

(defgn shortest-paths
  "Uses Floyd-Warshall to returns a map of
   {[source destination] {:distance <num> :path [source ... destination]}}
   for the given graph. Uses weight-fn (a function of two nodes) to assign a
   cost to edges."
  [g & [weight-fn]]
  (let [ns        (vec (nodes g))
        weight-fn (or weight-fn (constantly 1))
        es        (edges g)
        {:keys [dist next]}
        (as-> {:dist {} :next {}} reduction
              (reduce
                (fn [{:keys [dist next]} [u v]]
                  (cond
                    (contains? es [u v])
                    {:dist (assoc dist [u v] (weight-fn u v))
                     :next (assoc next [u v] v)}

                    (= u v)
                    {:dist (assoc dist [v v] 0)
                     :next (assoc next [v v] v)}

                    :otherwise
                    {:dist (assoc dist [u v] Double/POSITIVE_INFINITY)
                     :next (assoc next [u v] nil)}))
                reduction
                (for [i ns j ns] [i j]))
              (reduce
                (fn [{:keys [dist next] :as agg} [i j k]]
                  (let [candidate
                        (+ (get dist [i k] Double/POSITIVE_INFINITY)
                           (get dist [k j] Double/POSITIVE_INFINITY))]
                    (if (> (get dist [i j] Double/POSITIVE_INFINITY) candidate)
                      {:dist (assoc dist [i j] candidate)
                       :next (assoc next [i j] (get next [i k]))}
                      agg)))
                reduction
                (for [k ns i ns j ns] [i j k])))]
    (->> (for [u ns v ns]
           [[u v] (if (get next [u v])
                    (loop [u u path [u]]
                      (if-not (= u v)
                        (let [new-u (get next [u v])]
                          (recur new-u (conj path new-u)))
                        path))
                    [])])
         (remove (comp empty? second))
         (map (fn [[k v]] [k {:distance (get dist k) :path v}]))
         (into {}))))