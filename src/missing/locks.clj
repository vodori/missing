(ns missing.locks
  (:refer-clojure :exclude [locking]))


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

(defmacro locking
  "Lock on a value and only after obtaining the lock execute the body."
  [value & body]
  `(let [v# ~value]
     (try
       (clojure.core/locking (lease v#) ~@body)
       (finally (release v#)))))

(defmacro with-locks
  "Set the lock system to be used for any nested forms."
  [locks & body]
  `(binding [*locks* ~locks] ~@body))

(defmacro with-own-locks
  "Set the lock system to a new (dedicated) system to be used for any nested forms."
  [& body]
  `(binding [*locks* (atom {})] ~@body))