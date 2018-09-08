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