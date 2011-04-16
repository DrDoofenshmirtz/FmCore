(ns
  #^{:doc "Macros for extracting options from argument lists."
     :author "Frank Mosebach"}
  fm.core.opts
  (:use (fm.core seq)))

(defmacro with-args-and-opts
  "..."
  [arg-specs opt-specs args-and-opts & body]
  `(let [[args# opts#] (split-with-tag-set
                         (set (map keyword '~opt-specs))
                         ~args-and-opts)
         ~arg-specs args#
         {:keys ~opt-specs} (apply hash-map opts#)]
     ~@body))

(defmacro with-opts
  "..."
  [opt-specs opts & body]
  `(with-args-and-opts [] ~opt-specs ~opts ~@body))
