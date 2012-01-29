(ns
  ^{:doc "Tests for fm.core.options."
    :author "Frank Mosebach"}
  fm.core.test-options
  (:use [clojure.test :only (deftest is)])
  (:require [fm.core.options :as opts]))

(deftest flag-reader
  (let [flag-reader (opts/flag-reader ::flag ::default-value)]
    (is (= [true [::next]] (flag-reader [::flag ::next])))
    (is (= ::default-value (flag-reader)))
    (is (thrown? IllegalArgumentException (flag-reader [])))
    (is (thrown? IllegalArgumentException (flag-reader [::unexpected])))))

(deftest value-reader
  (let [value-reader (opts/value-reader ::flag ::default-value)]
    (is (= [::value [::next]] (value-reader [::flag ::value ::next])))
    (is (= ::default-value (value-reader)))
    (is (thrown? IllegalArgumentException (value-reader [])))
    (is (thrown? IllegalArgumentException (value-reader [::unexpected])))))

(deftest options-builder
  (let [options-builder (opts/options-builder
                          flag? ::nothing
                          value-1 ::value-1
                          value-2 ::value-2)]
    (is (=
          {:flag ::nothing
           :value-1 ::value-1
           :value-2 ::value-2}
          (options-builder [])))
    (is (=
          {:flag true
           :value-1 ::value-1
           :value-2 ::value-2}
          (options-builder [:flag])))
    (is (=
          {:flag true
           :value-1 1
           :value-2 ::value-2}
          (options-builder [:flag :value-1 1])))
    (is (=
          {:flag true
           :value-1 1
           :value-2 2}
          (options-builder [:flag :value-1 1 :value-2 2])))))

(defn run-tests []
  (clojure.test/run-tests 'fm.core.test-options))
