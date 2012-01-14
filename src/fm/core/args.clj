(ns
  ^{:doc "Utilities for processing function arguments."
    :author "Frank Mosebach"}
  fm.core.args
  (:use [clojure.contrib.str-utils :only (chop)]))

(defn- option-type [option-spec]
  (::option-type (meta option-spec)))

(defn- flag-option? [option-spec]
  (= ::flag (option-type option-spec)))

(defn- value-option? [option-spec]
  (= ::value (option-type option-spec)))

(defn- validate-spec [option-spec]
  (let [[option-name default-value] option-spec
        option-name (name option-name)
        flag-option? (.endsWith option-name "?")
        option-name (keyword (if flag-option?
                               (chop option-name)
                               option-name))]
    (with-meta
      [option-name default-value]
      {::option-type (if flag-option? ::flag ::value)})))

(defn- validate-specs [option-specs]
  (map complete-spec option-specs))

(defn- flag-reader [option-spec]
  (let [[option-name default-value] option-spec]
    (fn [args]
      (let [[arg-name & args-tail] args]
        (if (= option-name arg-name)
          [true args-tail]
          [default-value args])))))

(defn- value-reader [option-spec]
  (let [[option-name default-value] option-spec]
    (fn [args]
      (let [[arg-name arg-value & args-tail] args]
        (if (= option-name arg-name)
          [arg-value args-tail]
          [default-value args])))))

(defn- option-reader [option-spec])

(defn make-options-builder
  ([option-specs]
    (make-options-builder option-specs true))
  ([option-specs validate?]
    (let [option-specs (if validate?
                         (validate-specs option-specs)
                         option-specs)
          specs-map (reduce
                      (fn [specs-map spec]
                        (let [[option-keyword default-value] spec]
                          (assoc specs-map option-keyword spec)))
                      {}
                      option-specs)]
      specs-map)))

(defmacro options-builder [& option-specs]
  (let [option-specs (validate-specs option-specs)]
   `(make-options-builder (list ~@option-specs) false)))
