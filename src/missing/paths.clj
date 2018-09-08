(ns missing.paths
  (:import (java.util Set Map List)))

(defprotocol PathSeq
  (path-seq* [form path] "Helper for path-seq"))

(extend-protocol PathSeq
  List
  (path-seq*
    [form path]
    (->> form
         (map-indexed
           (fn [idx item]
             (path-seq* item ((fnil conj []) path idx))))
         (mapcat identity)))

  Map
  (path-seq*
    [form path]
    (->> form
         (map
           (fn [[k v]]
             (path-seq* v ((fnil conj []) path k))))
         (mapcat identity)))

  Set
  (path-seq*
    [form path]
    (->> form
         (map
           (fn [v]
             (path-seq* v ((fnil conj []) path v))))
         (mapcat identity)))

  Object
  (path-seq* [form path] [[form path]])

  nil
  (path-seq* [_ path] [[nil path]]))


(defn path-seq
  "Returns a sequence of [value path] tuples."
  [form]
  (path-seq* form nil))

(defn index-values-by-paths
  "Returns a map of path => value at path for any data structure"
  [form]
  (->> (path-seq form)
       (map (comp vec reverse))
       (into {})))