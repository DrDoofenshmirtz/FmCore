(ns
  #^{:doc "Tests for fm.core.shared-resource."
    :author "Frank Mosebach"}
  fm.core.test-shared-resource
  (:use [clojure.test :only (deftest is)])
  (:use [clojure.contrib.def :only (defvar-)])
  (:require [fm.core.shared-resource :as shr]))

(defvar- lifecycle [(fn create []
                      (atom 1))
                    (fn delete [resource]
                      (swap! resource dec)
                      resource)
                    identical?])

(deftest acquire-resource
  (let [shared-resource (apply shr/shared-resource lifecycle)
        resource (shr/acquire shared-resource)]
    (is (shr/in-use? shared-resource resource))
    (is (identical? resource (shr/acquire shared-resource)))
    (is (shr/in-use? shared-resource (shr/acquire shared-resource)))))

(deftest discard-resource
  (let [shared-resource (apply shr/shared-resource lifecycle)
        resource (shr/acquire shared-resource)]
    (is (shr/in-use? shared-resource resource))
    (is (identical? resource (shr/discard shared-resource resource)))
    (is (not (shr/in-use? shared-resource resource)))
    (is (zero? @resource))
    (is (identical? resource (shr/discard shared-resource resource)))
    (is (neg? @resource))
    (is (not (identical? resource (shr/acquire shared-resource))))))

(deftest reset-resource
  (let [shared-resource (apply shr/shared-resource lifecycle)
        resource (shr/acquire shared-resource)]
    (is (shr/in-use? shared-resource resource))
    (is (identical? resource (shr/reset shared-resource)))
    (is (not (shr/in-use? shared-resource resource)))
    (is (not (identical? resource (shr/reset shared-resource))))
    (is (nil? (shr/reset shared-resource)))
    (is (zero? @resource))    
    (is (not (identical? resource (shr/acquire shared-resource))))))

(deftest destroy-resource
  (let [shared-resource (apply shr/shared-resource lifecycle)
        resource (shr/acquire shared-resource)]
    (is (shr/in-use? shared-resource resource))
    (is (identical? resource (shr/destroy shared-resource)))
    (is (not (shr/in-use? shared-resource resource)))
    (is (nil? (shr/acquire shared-resource)))
    (is (zero? @resource))
    (is (nil? (shr/destroy shared-resource)))
    (is (zero? @resource))))

(defn run-tests []
  (clojure.test/run-tests 'fm.core.test-shared-resource))
