(ns
  #^{:doc "Tests for fm.core.exception."
     :author "Frank Mosebach"}
  fm.core.test-exception
  (:use
    [clojure.test :only (deftest is)])
  (:require
    [fm.core.exception :as exc])
  (:import
    (java.io StringReader Closeable IOException)))

(defn- closeable-mock
  ([name]
    (closeable-mock name false))
  ([name fail?]
    (proxy [Closeable] []
      (close []
        (println (str "Closing " name "..."))
        (if fail?
          (do
            (println "...failed!")
            (throw (IOException. (str "Closing " name " failed!"))))
          (println "...succeeded."))))))

(deftest do-silently
  (is (nil? (exc/do-silently)))
  (is (= 16 (exc/do-silently (* 2 2) (* 3 3) (* 4 4))))
  (is (exc/error? (exc/do-silently (* 2 2) (/ 3 0) (* 4 4))))
  (is (thrown? 
        ArithmeticException
        (exc/throw-if-error (exc/do-silently (* 2 2) (/ 3 0) (* 4 4))))))

(deftest retry-silently
  (is (= 16 (exc/retry-silently #(* 4 4))))
  (is (exc/error? (exc/retry-silently #(/ 1 0) 3 0))))

(deftest close-nothing-silently
  (is (nil? (exc/close-silently))))

(deftest close-one-silently
  (is (not (exc/error? (exc/close-silently (closeable-mock "A")))))
  (is (exc/error? (exc/close-silently (closeable-mock "B" true)))))

(deftest close-many-silently
  (is (not (exc/error? (exc/close-silently
                     (closeable-mock "A")
                     (closeable-mock "B")
                     (closeable-mock "C")))))
  (is (exc/error? (exc/close-silently
                (closeable-mock "A" true)
                (closeable-mock "B")
                (closeable-mock "C"))))
  (is (exc/error? (exc/close-silently
                (closeable-mock "A")
                (closeable-mock "B" true)
                (closeable-mock "C"))))
  (is (exc/error? (exc/close-silently
                (closeable-mock "A")
                (closeable-mock "B")
                (closeable-mock "C" true)))))

(defn run-tests []
  (clojure.test/run-tests 'fm.core.test-exception))
