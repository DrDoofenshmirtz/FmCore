(ns
  ^{:doc "Utilities for processing function arguments."
    :author "Frank Mosebach"}
  fm.core.args
  (:use [clojure.contrib.str-utils :only (chop)]))

(defn- throw-unexpected-option-name [expected actual]
  (throw (IllegalArgumentException. (str
                                      "Unexpected option name (expected: '"
                                      expected "', actual: '" actual "')!"))))

(defn- throw-missing-option-value [option-name]
  (throw (IllegalStateException. (str
                                   "Missing value for option '"
                                   option-name "'!"))))

(defn- throw-illegal-option-name [option-name]
  (throw (IllegalArgumentException. (str
                                      "Illegal option name: '"
                                      option-name "'!"))))

(defn flag-reader [option-name default-value]
  (fn
    ([] default-value)
    ([[next-name & args-tail]]
      (if (= option-name next-name)
        [true args-tail]
        (throw-unexpected-option-name option-name next-name)))))

(defn value-reader [option-name default-value]
  (fn
    ([] default-value)
    ([args]
      (let [[next-name next-value :as name-and-value] (take 2 args)]
        (cond
          (not= option-name next-name)
            (throw-unexpected-option-name option-name next-name)
          (< (count name-and-value) 2)
            (throw-missing-option-value next-name)
          :else
            [next-value (drop 2 args)])))))

(defn- option-reader [option-name default-value flag-option?]
  ((if flag-option? flag-reader value-reader) option-name default-value))

(defn- reader-map [option-specs make-reader]
  (reduce
    (fn [reader-map option-spec]
      (let [[option-name default-value] option-spec
            option-name (name option-name)
            flag-option? (.endsWith option-name "?")
            option-name (if flag-option? (chop option-name) option-name)
            option-name (keyword option-name)
            option-reader (make-reader option-name default-value flag-option?)]
        (assoc reader-map option-name option-reader)))
    {}
    (partition 2 option-specs)))

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

(defn make-options-builder
  ([reader-map]
    (assert (instance? clojure.lang.IPersistentMap reader-map))
    (assert (instance? clojure.lang.IFn reader-map))
    #(apply add-defaults (rest (read-options % reader-map {}))))
  ([option-name default-value & option-specs]
    (let [option-specs (cons option-name (cons default-value option-specs))]
      (make-options-builder (reader-map option-specs option-reader)))))

(defn- gen-option-reader [option-name default-value flag-option?]
  (if flag-option?
   `(flag-reader ~option-name ~default-value)
   `(value-reader ~option-name ~default-value)))

(defn- gen-reader-map [option-specs]
  (reader-map option-specs gen-option-reader))

(defmacro options-builder [option-name default-value & option-specs]
  (let [option-specs (cons option-name (cons default-value option-specs))
        reader-map (gen-reader-map option-specs)]
   `(make-options-builder ~reader-map)))
