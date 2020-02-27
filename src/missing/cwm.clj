(ns missing.cwm
  "Devious code-walking-macros."
  (:require [clojure.set :as sets]
            [clojure.walk :as walk]))

(defn fn-form? [form]
  (and (seq? form)
       (symbol? (first form))
       (contains? #{'fn*} (first form))))

(defn binding-form? [form]
  (and (seq? form)
       (symbol? (first form))
       (or (contains? #{'let* 'loop* 'letfn*} (first form)))))

(defn walk-seq [form]
  (tree-seq #(and (not (string? %)) (seqable? %)) seq form))

(defn walk-fun [context f form]
  (letfn [(expand [bindings impl]
            (let [symbols  (disj (set (filter symbol? (walk-seq bindings))) '&)
                  context' (update context :locals sets/union symbols)]
              (list bindings (f context' impl))))]
    (cond
      (symbol? (second form))
      (let [[prefix fun-name & impls] form]
        (apply list prefix fun-name (for [[bindings impl] impls] (expand bindings impl))))
      (seq? (second form))
      (let [[prefix & impls] form]
        (apply list prefix (for [[bindings impl] impls] (expand bindings impl))))
      (vector? (second form))
      (let [[prefix args impl] form]
        (list prefix (expand args impl)))
      :otherwise
      (throw (ex-info "Unknown function form." {:form form})))))

(defn walk-binding [context f [binding-symbol bindings & body]]
  (let [{:keys [bindings context]}
        (reduce
          (fn [{:keys [context] :as agg} [symbol v]]
            (-> agg
                (update :bindings conj symbol (f context v))
                (update-in [:context :locals] conj symbol)))
          {:bindings [] :context context}
          (partition 2 (destructure bindings)))]
    (apply list binding-symbol bindings
      (for [form body] (f context form)))))

(defn walk-form [context f form]
  (cond
    (fn-form? form)
    (walk-fun
      context
      (fn [context form]
        (walk-form context f form))
      form)
    (binding-form? form)
    (walk-binding
      context
      (fn [context form]
        (walk-form context f form))
      form)
    (seq? form)
    (map (fn [x] (walk-form context f x)) form)
    :otherwise
    (f context form)))

(defn walk
  ([f form]
   (walk {:locals #{}} f form))
  ([context f form]
   (walk-form context f (walk/macroexpand-all form))))

(defn replace-symbols-except-where-shadowed [replacements form]
  (walk
    (fn [{:keys [locals]} form]
      (let [replaceable (sets/difference (set (keys replacements)) locals)]
        (if (and (symbol? form) (contains? replaceable form))
          (get replacements form)
          form)))
    form))