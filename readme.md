[![Build Status](https://travis-ci.org/vodori/missing.svg?branch=develop)](https://travis-ci.org/vodori/missing) [![Maven metadata URL](https://img.shields.io/maven-metadata/v/http/central.maven.org/maven2/com/vodori/missing/maven-metadata.xml.svg)](https://mvnrepository.com/artifact/com.vodori/missing)


### Missing

A utility library for Clojure of functions and macros that complement clojure.core.

### Install

``` 
[com.vodori/missing "0.1.4"]
```

### Sample usages

Below are some examples of functions available in this library. For 
a full indication of what the library supports, please see the tests.

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

#### Concurrent execution of forms

A little macro sugar on top of future to execute 
multiple independent tasks concurrently and return
a sequence of their results.

```clojure 

(require '[missing.core :refer :all])

(zipmap [:users :posts] (together (get-users) (get-posts)))

```

___

#### Combinatorics

```clojure 

(require '[missing.core :refer :all])

(subsets #{1 2 3}) 
;=> #{#{} #{1} #{2} #{3} #{1 2} #{2 3} #{1 2 3}}

(submaps {:a 1 :b 2 :c 3}) 
;=> {} {:a 1} {:c 3} {:b 2} {:c 3, :b 2} {:b 2, :a 1} {:c 3, :a 1} {:c 3, :b 2, :a 1}

(intersect? #{1 2 3} #{1 3} #{1}) 
;=> true

(exclusive? #{1 2 3} #{4 5} #{6}) 
;=> true

```

___



#### Lazy merge sort

Create a single sorted sequence by lazily interleaving already sorted sequences.

```clojure 

(require '[missing.core :refer :all])

(def x [:a :c :d :e :i])
(def y [:b :f :g :h])
(merge-sort [x y]) ;=> [:a :b :c :d :e :f :g :h :i]

```

___

#### Indexing collections

Use these when you're building lookup tables to efficiently perform
batch operations.

```clojure 

(require '[missing.core :refer :all])

(def users 
    (index-by :username 
        [{:username "fred" :email "fred@gmail.com"} 
         {:username "greg" :email "greg@gmail.com"}])

(get users "fred") 
;=> {:username "fred" :email "fred@gmail.com"}


(def resources 
  [{:meta {:app "sso" :version "2018-10" :stage "dev"}}
   {:meta {:app "db" :version "2018-07" :stage "prod"}}
   {:meta {:app "api" :version "2018-07" :stage "dev"}}])
    
(def table (group-by-labels :meta resources))

(get table {:version "2018-10"})
;=> [{:meta {:app "sso" :version "2018-10" :stage "dev"}}]

(get table {:stage "dev"}) 
;=> [{:meta {:app "sso" :version "2018-10" :stage "dev"}}
;    {:meta {:app "api" :version "2018-07" :stage "dev"}}]

(get table {:version "2018-07" :stage "dev"}) 
;=> [{:meta {:app "api" :version "2018-07" :stage "dev"}}]


(let [pk1           "paul.rutledge@example.com"
      pk2           "raul.putledge@example.com"
      person-info  [{:id pk1 :name "Paul" :age 26}
                    {:id pk2 :name "Raul" :age 62}]
      account-info [{:email pk1 :year-joined 2018}
                    {:email pk2 :year-joined 8102}]]
  (collate [[:id person-info] [:email account-info]])))
  
;=> {"raul.putledge@example.com"
;    [{:id "raul.putledge@example.com", :name "Raul", :age 62}
;      {:email "raul.putledge@example.com", :year-joined 8102}],
;
;    "paul.rutledge@example.com"
;    [{:id "paul.rutledge@example.com", :name "Paul", :age 26}
;     {:email "paul.rutledge@example.com", :year-joined 2018}]}
```

___

#### Locking by value

Clojure has lots of great ways to deal with state. Reference locking is probably 
least among them but if the use case is isolated and the exclusive operation involves 
side effects (can't run in a CAS loop) sometimes it's an appropriate choice. Missing 
provides reentrant locks that lock on values so you can ensure different parts of your 
program never act on behalf of the same value (often an identifier) at the same time.

```clojure 

(require '[missing.locks :refer :all])

(locking "user-id"
  (update-user (fn [user] (assoc user :email "new-email@gmail.com")))

; you can also subdivide the exclusive scope by wrapping 
; evaluation with your own lock tables.

(def read-locks (atom {}))
(def write-locks (atom {}))

(with-locks read-locks 
    (locking "user-id"
        (get-user "user-id")))

(with-locks write-locks 
    (locking "user-id"
        (update-user (fn [user] (assoc user :email "new-email@gmail.com"))))

```

___



#### Transducers: distinct-by and dedupe-by

The `xxx-by` transducers are, in my opinion, always preferable to a `xxx` transducer.
The reason being that `xxx-by` degrades into `xxx` when `f` is `identity` and so is 
equivalent but more powerful.

```clojure 

(require '[missing.core :refer :all])

(distinct-by :x [{:x 1} {:x 2} {:x 1}]) ;=> [{:x 1} {:x 2}]

(dedupe-by :x [{:x 1} {:x 1} {:x 2} {:x 1}]) ;=> [{:x 1} {:x 2} {:x 1}]

```

___


#### Transducer: contiguous-by

Sometimes you have a sequence of elements that are potentially overlapping / abutting. You
might want to merge these segments into one item instead. This transducer just wraps 
partition-by with one-dimensional overlap tracking to chunk the sequence into contiguous 
segments. It works on any comparables and is lazy.

```clojure 

(require '[missing.core :refer :all])

(def flat [{:x1 0 :x2 4} {:x1 1 :x2 5} {:x1 5 :x2 6} {:x1 10 :x2 12}])

(contiguous-by :x1 :x2 flat)
;=> [[{:x1 0 :x2 4} {:x1 1 :x2 5} {:x1 5 :x2 6}] [{:x1 10 :x2 12}]]

; after this you'll probably perform a map to merge the
; contiguous items in each partition into one element

```

___

#### Graph Functions

The `missing.topology` namespace contains simple implementations of
many basic graph functions. Every function that takes a graph accepts 
just a plain adjacency map (map of node to list of nodes it has edges to). 
If your nodes don't have value semantics then you should just
use a graph of identifiers and perform lookups back to your 
objects.

Perhaps most generally useful are the topological sorts:

```clojure
(require '[missing.topology :refer :all])

; define an adjacency graph with edges indicating
; left 'is a requirement of' right
; a is a requirement of b and c
; b is a requirement of d
(def g {:a [:b :c] :b [:d]})

; sort them into phases where items
; in each phase can be resolved 
; concurrently
(topological-sort-with-grouping g) 
; => [#{:a} #{:c :b} #{:d}]

; If you'd rather, you could define
; an adjacency graph with edges indicating
; left 'depends on' right
; b depends on a
; c depends on a
; d depends on b

(def g' {:b [:a] :c [:a] :d [:b]})

; and just invert it before sorting:
(topological-sort-with-grouping (inverse g')) 
; => [#{:a} #{:c :b} #{:d}]
```

___ 


### License
This project is licensed under [MIT license](http://opensource.org/licenses/MIT).
