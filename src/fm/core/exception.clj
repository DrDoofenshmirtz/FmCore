(ns
  #^{:doc "Utilities for advanced exception handling."
     :author "Frank Mosebach"}
  fm.core.exception)

(defn- gen-silent-form
  "Wraps a given form inside a try-catch form. If yield-result? is true, the
  generated form will yield the result of the evaluation of the original form.
  If the evaluation of the given form throws an exception, the generated form
  will yield a map that contains the exception:
    (gen-silent-form '(+ 1 1) false)
      -> (try (+ 1 1) nil (catch Exception x {::error x}))
    (gen-silent-form '(+ 1 1) true)
      -> (try (+ 1 1) (catch Exception x {::error x}))."
  [form yield-result?]
  `(try 
     ~@(if yield-result? [form] [form nil])
     (catch Exception x# {::error x#})))

(defn gen-silent-forms
  "Given n forms, wraps the first n-1 forms inside a try-catch form that will
  discard the result of form's evaluation. The result of the last form's
  evaluation will be preserved."
  [forms]
  (conj
    (vec (map
           #(gen-silent-form % false)
           (drop-last forms)))
    (gen-silent-form (last forms) true)))

(defmacro do-silently
  "Evaluates each of the given forms inside a try-catch form, yielding the
  result of the evaluation of the last form. If evaluating any of the forms
  throws an exception, do-silently yields a map containing the first exception
  that has been caught. Regardless of any exception being thrown, each of the
  forms will be evaluated."
  [& forms]
  `(let [results# (list ~@(gen-silent-forms forms))]
     (or
       (some
         (fn [result#]
           (and (::error result#) result#))
         results#)
       (last results#))))

(defn error
  "Extracts the error from a result that has been yielded by do-silently."
  [result]
  (::error result))

(defn error?
  "Checks if the result that has been yielded by do-silently is an error."
  [result]
  (if (error result) true false))

(defn throw-if-error
  "Given the result that has been yielded by do-silently, throws an exception
  if the result is an error, otherwise returns the result."
  [result]
  (let [error (error result)]
    (if error (throw error))
    result))

(defn retry-silently
  "Repeatedly invokes the function func until an invocation returns without
  throwing an exception. 
  If a zero or positive maximum number of retries is given, invocation will
  be repeated for at most the given number of retries.
  If a positive delay is given, sleeps for the given amount of milliseconds
  between invocations.
  Yields the the result of the last invocation of func or an error object (see
  do-silently) if an exception is thrown."
  ([func]
    (retry-silently func 0))
  ([func delay-millis]
    (retry-silently func -1 delay-millis))
  ([func max-retries delay-millis]
    (let [result (do-silently (func))]
      (if (and (not (zero? max-retries)) (error? result))
        (do (and (pos? delay-millis) (Thread/sleep delay-millis))
          (recur
            func
            (if (neg? max-retries) max-retries (dec max-retries))
            delay-millis))
        result))))

(defn close-silently
  "Invokes the close method on each of the given objects, typically instances
  of java.io.Closeable, inside a try-catch form. The close method of each object
  will be invokeded regardless of any exception being thrown. For return values
  see do-silently."
  [& closeables]
  (reduce
    (fn
      ([] nil)
      ([left right] (if (error? left) left right)))
    (map #(and % (do-silently (.close %))) closeables)))
