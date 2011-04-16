(ns fm.core.pipe
 (:use (clojure.contrib def except)))

(defn- next-write-point [write-point]
  (throw-if (nil? write-point)
    IllegalStateException "Pipe has already been closed!")
  (promise))

(defn pipe []
  (let [start (promise)
        write-point (ref start)
        pipe-seq (lazy-seq @start)]
    (letfn [(write-fn
              ([]
                (let [[end] (dosync [@write-point (ref-set write-point nil)])]
                  (and
                    end
                    (deliver end nil))))
              ([element]
                (let [[write-point end] (dosync
                                          [@write-point
                                           (alter 
                                             write-point
                                             next-write-point)])]
                  (deliver write-point (cons element (lazy-seq @end))))))]
      [pipe-seq write-fn])))

(defn elements [pipe]
  (let [[pipe-seq] pipe]
    pipe-seq))

(defn supply [pipe element]
  (let [[_ write-fn] pipe]
    (and write-fn (write-fn element))
    pipe))

(defn close [pipe]
  (let [[pipe-seq write-fn] pipe]
    (and write-fn (write-fn))
    [pipe-seq nil]))

(defn async-dump-pipe []
  (let [pipe (pipe)]
    (send (agent (elements pipe)) (fn [ps]
                                    (doseq [e ps]
                                      (println e))))
    pipe))
