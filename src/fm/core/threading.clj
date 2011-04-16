(ns
  #^{:doc "Threading utilities."
    :author "Frank Mosebach"}
  fm.core.threading
  (:use [clojure.contrib.def :only (defnk)])
  (:use [fm.core.seq :only (split-with-tags)]))

(defnk call-async
  "..."
  [job :thread-name nil :priority nil]
  (let [thread (Thread. job)]
    (doto thread
      (.setPriority (or priority Thread/NORM_PRIORITY))
      (.setName (str (or thread-name "do-async") "-" (.getId thread)))
      (.start))))

(defmacro do-async
  "..."
  [& forms-and-options]
  (let [[forms options] (split-with-tags 
                          forms-and-options
                          :thread-name :priority)]
    `(call-async (fn [] ~@forms) ~@options)))

(defmacro with-lock
  "..."
  [lock & body]
  `(do
     (.lock ~lock)
     (try ~@body (finally (.unlock ~lock)))))
