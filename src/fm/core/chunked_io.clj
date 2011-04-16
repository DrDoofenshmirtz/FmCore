(ns
  #^{:doc "Read chunks of characters from input streams."
     :author "Frank Mosebach"}
  fm.core.chunked-io
  (:refer-clojure :exclude (chunk read-string))
  (:use [clojure.contrib.def :only (defnk defmacro-)])
  (:import (java.io InputStreamReader) (java.nio.charset Charset)))

(def default-encoding (.name (Charset/defaultCharset)))

(def default-chunk-size 1024)

(defn- chunk [buffer size]
  (let [chunk (char-array size)]
    (System/arraycopy buffer 0 chunk 0 size)
    chunk))

(defn chunk-reader [input-stream chunk-size encoding]
  (let [reader (InputStreamReader. input-stream (or encoding default-encoding))
        buffer (char-array (or chunk-size default-chunk-size))]
    (fn []
      (let [chars-read (.read reader buffer)]
        (if-not (neg? chars-read)
          (chunk buffer chars-read))))))

(defnk chunk-seq [:chunk-reader nil
                  :input-stream nil
                  :chunk-size nil
                  :encoding nil
                  :eof-pred nil?]
  (let [chunk-reader (or chunk-reader (fm.core.chunked-io/chunk-reader
                                        input-stream
                                        chunk-size
                                        encoding))]
    (take-while (complement eof-pred) (repeatedly chunk-reader))))

(defmacro- with-chunk-seq [& body]
  `(let [~'chunk-reader (or ~'chunk-reader (fm.core.chunked-io/chunk-reader
                                             ~'input-stream
                                             ~'chunk-size
                                             ~'encoding))
         ~'chunk-seq (chunk-seq 
                       :chunk-reader ~'chunk-reader
                       :eof-pred ~'eof-pred)]
     ~@body))

(defnk char-seq [:chunk-reader nil
                 :input-stream nil
                 :chunk-size nil
                 :encoding nil
                 :eof-pred nil?]
  (with-chunk-seq (reduce concat chunk-seq)))

(defnk read-string [:chunk-reader nil
                    :input-stream nil
                    :chunk-size nil
                    :encoding nil
                    :eof-pred nil?]
  (with-chunk-seq (str (reduce #(.append %1 %2) (StringBuilder.) chunk-seq))))
