[![Build Status](https://travis-ci.org/vodori/missing.svg?branch=develop)](https://travis-ci.org/vodori/missing) [![Maven metadata URL](https://img.shields.io/maven-metadata/v/http/central.maven.org/maven2/com/vodori/missing/maven-metadata.xml.svg)](https://mvnrepository.com/artifact/com.vodori/missing)


### Missing

A utility library for Clojure of functions and macros that are frequently missed and recreated.

### Install

``` 
[com.vodori/missing "0.1.0"]
```

### Sample usages

___


#### Comparable operators for more than numbers and O(n) scans

The existing <, >, <=, >= unfortunately only work on numbers. Use these
for a more general alternative with similar semantics. If all you're looking
for is the smallest or largest item, you shouldn't have to sort your sequence.

```clojure 

(require '[missing.core :refer :all])

(lt "a" "b") ;=> true
(lte "a" "a") ;=> true
(gt "b" "a") ;=> true
(gte "b" "b") ;=> true

(least ["a" "b" "c"]) ;=> "a"
(greatest ["a" "b" "c"]) ;=> "c"
(least-by :count [{:count 0} {:count 1} {:count -1}]) ;=> {:count -1}
(greatest-by :count [{:count 0} {:count 1} {:count -1}]) ;=> {:count 1}
```

___


#### Transducers: distinct-by and dedupe-by

The xxx-by transducers are, in my opinion, always preferable to a xxx transducer.
The reason being that xxx-by degrades into xxx when f is `identity` and so is 
equivalent but more powerful.

```clojure 

(require '[missing.core :refer :all])

(distinct-by :x [{:x 1} {:x 2} {:x 1}]) 
;=> [{:x 1} {:x 2}]

(dedupe-by :x [{:x 1} {:x 1} {:x 2} {:x 1}]) ;
=> [{:x 1} {:x 2} {:x 1}]

```

___


#### Transducer: contiguous-by

Sometimes you have a sequence of elements that are potentially overlapping / abutting. You
might want to merge these segments into one item instead. This transducer just wraps 
partition-by with one-dimensional overlap tracking to chunk the sequence into contiguous segments.

```clojure 

(require '[missing.core :refer :all])

(def flat [{:x1 0 :x2 4} {:x1 1 :x2 5} {:x1 5 :x2 6} {:x1 10 :x2 12}])

(contiguous-by :x1 :x2 flat)
;=> [[{:x1 0 :x2 4} {:x1 1 :x2 5} {:x1 5 :x2 6}] {:x1 10 :x2 12}]
; after this you'll probably perform a map to merge the
; contiguous items in each partition into one element

```

___

#### Topological sort


Use this when you can declare an order of dependencies between tasks 
and you want to organize them into serial phases of concurrent tasks. 
You could use this as a core algorithm to create macros that optimize
remote calls like haxl.

```clojure
(require '[missing.topology :refer :all])

; define an adjacency graph of dependencies.
; a must happen before b and c
; b must happen before d
(def g {:a [:b :c] :b [:d]})

; sort them into serial phases where items
; in each phase can be resolved concurrently
(topological-sort-with-grouping g) 
; => [#{:a} #{:c :b} #{:d}]
```

___ 


### License
This project is licensed under [MIT license](http://opensource.org/licenses/MIT).