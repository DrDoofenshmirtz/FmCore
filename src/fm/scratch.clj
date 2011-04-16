(ns fm.scratch
  (:use (clojure.contrib def))
  (:use (fm.core opts exception)))

(defn send-and-wait [agent job & args]
  (let [promise (promise)
        job-wrapper (fn [state & args]
                      (try
                        (let [result (apply job state args)]
                          (deliver promise result)
                          result)
                        (catch Throwable t
                          (deliver promise t)
                          (throw t))))]
    (apply send-off agent job-wrapper args)
    (if (instance? Throwable @promise)
      (throw @promise)
      @promise)))

(defn- element-processor [producers parallel?]
  (let [map-fn (if parallel? pmap map)]
    (fn [element]
      (map-fn
        (fn [producer]
          (producer element))
        producers))))

(defn- take-nth-with-offset [offset step coll]
  (take-nth step (drop offset coll)))

(defn- results-for-nth-producer [offset step skip-pred results]
  (let [results (take-nth-with-offset offset step results)]
    (if skip-pred
      (filter (complement skip-pred) results)
      results)))

(defnk produce
  "..."
  [coll :producers nil :skip-pred nil? :parallel? false]
  (when (next producers)
    (let [results (mapcat (element-processor producers parallel?) coll)          
          step (count producers)]
      (map
        #(results-for-nth-producer % step skip-pred results)
        (range step)))))

(defmacro capture-attributes [object & accessors])
