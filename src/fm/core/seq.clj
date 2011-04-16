(ns
  #^{:doc "A collection of useful functions for seqs."
     :author "Frank Mosebach"}
  fm.core.seq)

(defn produce
  "Produces a seq of colls by applying each of the given functions to each
  element in the given coll, adding the result to the corresponding coll with
  (clojure.core/conj coll result). nil results are skipped. The produced colls
  are of the same type as the given coll."
  [coll & fns]
  (when fns
    (reduce
      (fn [colls element]
        (map
          (fn [[func coll]]
            (let [result (func element)]
              (if-not (nil? result)
                (conj coll result)
                coll)))
          (partition 2 (interleave fns colls))))
      (repeat (count fns) (empty coll))
      coll)))

(defn split-with-tag-set
  "Splits the coll at the first occurrence of any tag contained in the tag set.
  Returns a vector of the 2 resulting colls. Same as 
    (clojure.core/split-with (complement tag-set) coll)"
  [tag-set coll]
  (split-with (complement tag-set) coll))

(defn split-with-tags
  "Splits the coll at the first occurrence of any of the given tags. Returns a
  vector of the 2 resulting colls. Same as
    (fm.core.seq/split-with-tag-set (set tags) coll)"
  [coll & tags]
  (split-with-tag-set (set tags) coll))
