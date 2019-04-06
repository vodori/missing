## 0.1.9

* Added `transitive-closure`.
* Fixed a mistake in `shortest-paths` that caused missed paths.

## 0.1.8

* Added `shortest-paths`, an implementation of floyd-warshall that produces
the transitive closure of a graph with minimum distance paths.

## 0.1.7

* Added `paging`, `keep-keys`, `keep-vals`, `keep-entries`, `uniqueifier`
* Added `defonce-protocol` and `defmethodset`

## 0.1.6

* Added `structural-extractor` and `select-structure`
* Fixed mistake in missing.topology/inverse implementation so that it now preserves nodes with no edges.

## 0.1.5

* Added `preemptable` / `preempt`
* Added `dfs-preorder` and `dfs-postorder` searches that walk clojure data
* Preserve map type for `map-keys`, `map-vals`, `filter-keys`, and `filter-vals`
* Added `partition-with` for partitioning according to a predicate that returns true on elements that should be the start of a new partition

## 0.1.4

* Added many graph functions (`missing.topology`) for graphs represented as adjacency maps.
* Simplified implementation of existing `topological-sort-with-grouping`.

## 0.1.3

* Added `join-paths`, `invert-grouping`, `duration-parts` and `duration-explain`

## 0.1.2

* Fixed TravisCI build against Java 10
* Updated implementation of `map-keys`, `map-vals`, `filter-keys`, and `filter-vals` to leverage transients and reduce-kv for better performance.

## 0.1.1

* Add source docs and readme.

## 0.1.0

* Initial Release