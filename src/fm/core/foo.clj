(ns
  #^{:doc "Functional OO support."
     :author "Frank Mosebach"}
  fm.core.foo
  (:use clojure.contrib.def))

(defn- gen-slot-fn
  "Generates a defn-form that defines a slot function, using the given symbol
  as function name. It expects a map as its first argument, followed by any
  number of additional arguments.
  The generated function will look up the value associated to the keyword 
  :method-symbol in the given map. The value must be a function. It will be
  invoked with the arguments that have been passed to the slot function."
  [method-symbol]
  (let [method-keyword (keyword (name method-symbol))]
    `(defn ~method-symbol [this# & args#]
       (apply (~method-keyword this#) this# args#))))

(defn- add-this-param [params]
  "Returns a vector where a symbol named 'this' is the first element and params
  is the rest."
  (vec (cons 'this params)))

(defn- gen-method-entry
  "Generates a vector [:method-keyword function-form] from the given method
  form. A valid method form is either
    (method-name [params*] exprs*)
  or
    (method-name ([params*] exprs*)+).
  A symbol named 'this' is added as the first parameter to each parameter list.
  When the generated function is invoked through a slot function, 'this' will 
  be bound to the map that contains the generated method entry."
  [method-form]
  (let [[method-symbol & params-and-body] method-form
        [[params] body] (split-with vector? params-and-body)]
    [(keyword (name method-symbol))
     (if params
       `(fn ~(add-this-param params) ~@body)
       `(fn ~@(map
                (fn [[params & body]]
                  (cons (add-this-param params) body))
                body)))]))

(defn- gen-method-map-and-slot-fns
  "..."
  [method-forms]
  (reduce
    (fn [[method-map slot-fns] [method-symbol :as method-form]]
      (let [[method-keyword method-form] (gen-method-entry method-form)
            slot-fn (gen-slot-fn method-symbol)]
        [(assoc method-map method-keyword method-form)
         (conj slot-fns slot-fn)]))
    [{} []]
    method-forms))

(defn gen-foo-type
  "..."
  [type-symbol init-params bindings method-forms]
  (let [foo-type (keyword (str *ns*) (str type-symbol))
        [method-map slot-fns] (gen-method-map-and-slot-fns method-forms)]
    `(do
       (defn ~type-symbol ~init-params
         (let [~@bindings
               method-map# (with-meta
                             ~method-map
                             {::foo-type ~foo-type})
               init-fn# (:init method-map#)]
           (if init-fn#
             (or (init-fn# method-map#) method-map#)
             method-map#)))
       ~@slot-fns)))

(defmacro def-foo-type
  "..."
  [type-symbol init-params & bindings-and-method-forms]
  (let [[[bindings] method-forms] (split-with 
                                    vector?
                                    bindings-and-method-forms)]
    (gen-foo-type type-symbol init-params bindings method-forms)))

(defn foo-type
  "..."
  [this]
  (::foo-type (meta this)))
