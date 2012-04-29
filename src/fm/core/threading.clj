(ns
  ^{:doc "Threading utilities."
    :author "Frank Mosebach"}
  fm.core.threading
  (:use
    [clojure.contrib.def :only (defnk)]))

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
  "Wraps the given collection of forms in a job function that will be executed
  in a new thread. Supported options are: :thread-name name prefix (defaults to
  \"do-async\", a dash and the created thread's id will be appended), :priority
  thread priority (defaults to Thread/NORM_PRIORITY)."
  [forms & {:keys [thread-name priority]}]
  `(call-async (fn [] ~@forms) :thread-name ~thread-name :priority ~priority))

(defmacro with-lock
  "Executes the body forms after acquiring the lock. Ensures that the lock
  will be released afterwards."
  [lock & body]
  `(do
     (.lock ~lock)
     (try ~@body (finally (.unlock ~lock)))))
