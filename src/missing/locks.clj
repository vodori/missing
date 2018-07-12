(ns missing.locks
  (:require [clojure.core :as clj]))


(def ^:dynamic *locks* (atom {}))

(defn- new-lock []
  {:count 1 :ref (Object.)})

(defn- inc-lock [lock]
  (when lock (update lock :count inc)))

(defn- dec-lock [lock]
  (when lock (update lock :count dec)))

(defn- inc-or-new [lock]
  (or (inc-lock lock) (new-lock)))

(defn lease [k]
  (get-in (swap! *locks* update k inc-or-new) [k :ref]))

(defn release [k]
  (swap! *locks*
         (fn [locks]
           (when-some [entry (dec-lock (get locks k))]
             (if (zero? (:count entry))
               (dissoc locks k)
               (assoc locks k entry))))))

(defmacro locking [value & body]
  `(let [v# ~value]
     (try
       (clj/locking (lease v#) ~@body)
       (finally (release v#)))))

(defmacro with-locks [locks & body]
  `(binding [*locks* ~locks] ~@body))

(defmacro with-own-locks [& body]
  `(binding [*locks* (atom {})] ~@body))