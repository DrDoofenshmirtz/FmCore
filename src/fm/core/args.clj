(ns
  ^{:doc "Utilities for processing function arguments."
    :author "Frank Mosebach"}
  fm.core.args
  (:use [clojure.contrib.str-utils :only (chop)]))

(defn read-flag [option-name default-value args]
  (let [[arg-name & args-tail] args]
    (if (= option-name arg-name)
      [true args-tail]
      [default-value args])))

(defn read-value [option-name default-value args]
  (let [[arg-name arg-value & args-tail] args]
    (if (= option-name arg-name)
      [arg-value args-tail]
      [default-value args])))

(defn- flag-reader [option-name default-value]
  (partial read-flag option-name default-value))

(defn- value-reader [option-name default-value]
  (partial read-value option-name default-value))

(defn- option-reader [option-name default-value flag-option?]
  ((if flag-option? flag-reader value-reader) option-name default-value))

(defn- reader-map [option-specs]
  (reduce
    (fn [reader-map option-spec]
      (let [[option-name default-value] option-spec
            option-name (name option-name)
            flag-option? (.endsWith option-name "?")
            option-name (if flag-option? (chop option-name) option-name)
            option-name (keyword option-name)
            option-reader (option-reader option-name default-value flag-option?)]
        (assoc reader-map option-name option-reader)))
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
    (fn [options [option-name reader]]
      (let [[default-value _] (reader nil)]
        (assoc options option-name default-value)))
    options
    reader-map))

(defn args-parser [reader-map]
  (fn [args]
    (let [[args reader-map options] (read-options args reader-map {})]
      (add-defaults reader-map options))))

(defn make-options-builder [option-spec & option-specs]
  (args-parser (reader-map (cons option-spec option-specs))))

;; TODO: (defmacro options-builder [& option-specs])
