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

(defn guarded-ref
  "Creates a function f that acts as a 'guarded reference' to a value.
  The created function accepts a 'mutator' function and any number of
  additional arguments.
  The mutator function will be called with the currently stored value
  and any additional arguments. It is expected to return a collection
  of [new-value return-value], where new-value is the new value to be
  stored and return-value will be returned as result of the invocation.
  Access to and mutation of the stored value is guarded guarded by a lock,
  the monitor of which will be held during the invocation of the mutator.
  If no lock is given, a java.lang.Object will be used.
  If no value is given, a default value of nil will be used."
  ([]
    (guarded-ref nil))
  ([value]
    (guarded-ref (Object.) value))
  ([lock value]
    (let [ref (atom value)]
      (fn [mutator & args]
        (locking lock
          (let [[value result] (apply mutator @ref args)]
            (reset! ref value)
            result))))))
