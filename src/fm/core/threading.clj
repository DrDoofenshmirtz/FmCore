(ns
  #^{:doc "Threading utilities."
    :author "Frank Mosebach"}
  fm.core.threading
  (:use [clojure.contrib.def :only (defnk)])
  (:use [fm.core.seq :only (split-with-tags)]))

(defnk call-async
  "Runs the given job function in a new thread. Returns the created thread.
  Supported options are: :thread-name name prefix (defaults to \"do-async\",
  a dash and the created thread's id will be appended), :priority thread
  priority (defaults to Thread/NORM_PRIORITY)."
  [job :thread-name nil :priority nil]
  (let [thread (Thread. job)]
    (doto thread
      (.setPriority (or priority Thread/NORM_PRIORITY))
      (.setName (str (or thread-name "do-async") "-" (.getId thread)))
      (.start))))

(defmacro do-async
  "Wraps the given forms in a job function that will be executed in a new
  thread. Supported options are: :thread-name name prefix (defaults to
  \"do-async\", a dash and the created thread's id will be appended),
  :priority thread priority (defaults to Thread/NORM_PRIORITY)."
  [& forms-and-options]
  (let [[forms options] (split-with-tags 
                          forms-and-options
                          :thread-name :priority)]
    `(call-async (fn [] ~@forms) ~@options)))

(defmacro with-lock
  "Executes the body forms after acquiring the lock. Ensures that the lock
  will be released afterwards."
  [lock & body]
  `(do
     (.lock ~lock)
     (try ~@body (finally (.unlock ~lock)))))
