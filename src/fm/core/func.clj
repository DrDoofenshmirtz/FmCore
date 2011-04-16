(ns
  #^{:doc "Miscellaneous functions and macros to create functions."
     :author "Frank Mosebach"}
  fm.core.func)

(defn once
  "Creates a function that takes any number of arguments and returns the given
  value on the first invocation. Any subsequent invocations will return nil."
  [value]
  (let [value (atom value)]
    (fn [& ignored]
      (let [once @value]
        (or (nil? once) (reset! value nil))
        once))))

(defn with-delay
  "Creates a new function that sleeps the given delays before and optionally
  after invoking the given function."
  ([func before-millis]
    (with-delay func before-millis nil))
  ([func before-millis after-millis]
    (fn [& args]
      (if before-millis (Thread/sleep before-millis))
      (let [result (apply func args)]
        (if after-millis (Thread/sleep after-millis))
        result))))
