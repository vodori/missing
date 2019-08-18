(ns missing.logging)

(def namespace-exists?
  (memoize
    (fn [sym]
      (try
        (require sym) true
        (catch Throwable _ false)))))

(defmacro log [form & more]
  (let [fun     (name (first form))
        timbre  (symbol "taoensso.timbre" fun)
        logging (symbol "clojure.tools.logging" fun)]
    (cond
      (namespace-exists? 'taoensso.timbre)
      (with-meta `(~timbre ~@more) (meta form))
      (namespace-exists? 'clojure.tools.logging)
      (with-meta `(~logging ~@more) (meta form))
      :otherwise
      nil)))

(defmacro trace [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro tracef [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro debug [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro debugf [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro info [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro infof [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro warn [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro warnf [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro error [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro errorf [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro fatal [msg & more]
  `(log ~&form ~msg ~@more))

(defmacro fatalf [msg & more]
  `(log ~&form ~msg ~@more))
