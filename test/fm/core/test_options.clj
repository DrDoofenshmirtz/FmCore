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
  (let [value-reader (opts/optional-value-reader ::flag ::default-value)]
    (is (= [::value [::next]] (value-reader [::flag ::value ::next])))
    (is (= ::default-value (value-reader)))
    (is (thrown? IllegalArgumentException (value-reader [])))
    (is (thrown? IllegalArgumentException (value-reader [::unexpected])))))

(defn run-tests []
  (clojure.test/run-tests 'fm.core.test-options))
