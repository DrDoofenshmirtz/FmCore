(ns
  ^{:doc "Utilities for processing function arguments."
    :author "Frank Mosebach"}
  fm.core.args
  (:use [clojure.contrib.str-utils :only (chop)]))

(defn flag-reader [default-value]
  (fn
    ([] default-value)
    ([[_ & args-tail]] [true args-tail])))

(defn value-reader [default-value]
  (fn
    ([] default-value)
    ([[_ value & args-tail]] [value args-tail])))

(defn- option-reader [default-value flag-option?]
  ((if flag-option? flag-reader value-reader) default-value))

(defn- reader-map [option-specs make-reader-value]
  (reduce
    (fn [reader-map option-spec]
      (let [[option-name default-value] option-spec
            option-name (name option-name)
            flag-option? (.endsWith option-name "?")
            option-name (if flag-option? (chop option-name) option-name)
            option-name (keyword option-name)
            reader-value (make-reader-value default-value flag-option?)]
        (assoc reader-map option-name reader-value)))
    {}
    option-specs))

(defn- throw-illegal-option-name [option-name]
  (throw (IllegalArgumentException. (str "Illegal option name: '" option-name "'!"))))

(defn- read-options [args reader-map options]
  (if (seq args)
    (let [[option-name] args
          option-reader (reader-map option-name)]
      (if option-reader
        (let [[option-value args] (option-reader args)
              reader-map (dissoc reader-map option-name)
              options (assoc options option-name option-value)]
          (recur args reader-map options))
        (throw-illegal-option-name option-name)))
    [args reader-map options]))

(defn- add-defaults [reader-map options]
  (reduce
    (fn [options [option-name option-reader]]
      (let [default-value (option-reader)]
        (assoc options option-name default-value)))
    options
    reader-map))

(defn args-parser [reader-map]
  #(apply add-defaults (rest (read-options % reader-map {}))))

(defn make-options-builder [option-spec & option-specs]
  (args-parser (reader-map (cons option-spec option-specs) option-reader)))

(defn- gen-option-reader [default-value flag-option?]
  (if flag-option?
   `(flag-reader ~default-value)
   `(value-reader ~default-value)))

(defn gen-reader-map [option-specs]
  (reader-map option-specs gen-option-reader))

(defmacro options-builder [option-spec & option-specs]
 `(args-parser ~(gen-reader-map (cons option-spec option-specs))))
