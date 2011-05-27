(ns fm.scratch
  (:use (clojure.contrib def))
  (:use (fm.core opts exception)))

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

(defn- collect-children [branch? children nodes]
  (if (seq nodes)
    (lazy-cat nodes (collect-children
                      branch?
                      children
                      (mapcat #(if (branch? %) (children %)) nodes)))))

(defn breadth-first-seq [branch? children root]
  (collect-children branch? children [root]))

(def tree [1
           [2
            [6
             [11
              [13
               [15]
               [16]
               [17]]
              [14]]
             [12]]
            [7]]
           [3
            [8]
            [9]]
           [4
            [10]]
           [5]])

(defn branch? [node] (next node))

(defn children [node] (rest node))

(defn breadth-first-file-seq [root]
  (let [branch? (fn [file]
                  (and file (.isDirectory file)))
        children (fn [file]
                   (and file (.listFiles file)))]
    (breadth-first-seq branch? children root)))
