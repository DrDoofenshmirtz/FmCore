(ns
  ^{:doc "Fully lazy sequence functions for when it matters
         (e. g. when working with blocking input...)."
    :author "Frank Mosebach"}
  fm.core.lazy-seqs)

(defn unsigned-byte-seq
  "Creates a sequence of unsigned bytes, lazyly reading byte by byte from the
  given input stream until the end of the stream has been reached."
  [input-stream]
  (lazy-seq (let [unsigned-byte (.read input-stream)]
              (if-not (neg? unsigned-byte)
                (cons unsigned-byte (unsigned-byte-seq input-stream))))))

(defn take-until
  "Lazyly takes element by element from the given collection until an element
  satisfies the given predicate. The first element that fulfills the predicate
  will be the last element of the resulting sequence."
  [pred coll]
  (lazy-seq (if (seq coll)
              (let [next (first coll)]
                (cons next (if-not (pred next)
                             (take-until pred (rest coll))))))))

(defn drop-until
  "Drops elements from the head of the given collection until an element
  satisfies the given predicate. The first element fulfilling the predicate
  will not be included in the resulting sequence."
  [pred coll]
  (letfn [(drop-next [coll]
            (if (and (seq coll) (not (pred (first coll))))
              (recur (rest coll))
              (rest coll)))]
    (lazy-seq (drop-next coll))))

(defn split-after
  "Returns a vector of [(take-until pred coll) (drop-until pred coll)]."
  [pred coll]
  [(take-until pred coll) (drop-until pred coll)])
