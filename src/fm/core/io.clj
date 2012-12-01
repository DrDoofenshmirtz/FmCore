(ns
  ^{:doc "IO utilities."
    :author "Frank Mosebach"}
  fm.core.io
  (:import
    (java.io InputStream ByteArrayOutputStream)))

(defn read-until-detected
  "Reads bytes from the given (non-nil) input stream until the given sequence
  of expected bytes has been read in the specified order or the end of the
  input stream has been reached.
  If successful, returns a byte array containing all bytes that have been read
  (including the sequence itself), otherwise nil."
  ^bytes [^InputStream input-stream expected]
  (if (nil? input-stream)
    (throw (IllegalArgumentException. "Illegal input stream: nil!")))
  (if (seq expected)
    (let [^ByteArrayOutputStream result (ByteArrayOutputStream.)
          ^bytes read-buffer (byte-array (count expected))]
      (loop [detect expected
             index  0
             size   (.read input-stream read-buffer 0 (count detect))]
        (cond

          ; The end of the input stream has been reached.
          (neg? size) nil

          ; The read bytes have been consumed.
          (>= index size) (do
                            (if (pos? size)
                              (.write result read-buffer 0 size))
                            (recur detect
                                   0
                                   (.read input-stream
                                          read-buffer
                                          0
                                          (count detect))))

          ; Compare the read bytes with the expected bytes.
          :else (if (== (aget read-buffer index) (first detect))
                  (if-let [detect (next detect)]
                    (recur detect (inc index) size)
                    (do
                      (.write result read-buffer 0 (inc index))
                      (.close result)
                      (.toByteArray result)))
                  (recur expected (inc index) size)))))))

(defn read-byte-array
  "Reads a byte array from the given (non-nil) input stream and returns it.
  The length of the returned byte array will be the minimum of given length
  and the number of bytes (still) available in the input stream."
  ^bytes [^InputStream input-stream length]
  (if (nil? input-stream)
    (throw (IllegalArgumentException. "Illegal input stream: nil!")))
  (if-not (pos? length)
    (throw (IllegalArgumentException. "Length must be greater than 0!")))
  (let [^bytes buffer (byte-array length)]
    (loop [offset 0 length length]
      (if (pos? length)
        (let [number-read (.read input-stream buffer offset length)]
          (if (neg? number-read)
            (byte-array offset buffer)
            (recur (+ offset number-read) (- length number-read))))
        buffer))))

(defn byte-array-seq
  "Creates a lazy seq of byte arrays that have been read from the given
  (non-nil) input stream.

  Optional keyword arguments:

    :available  The minimum number of bytes that is expected to be available
                in the input stream.

    :chunk-size The desired size of the byte arrays in the returned sequence.
                The actual size will be the minimum of the given value and the
                number of bytes (still) available in the input stream."
  [input-stream & {:keys [available chunk-size] :or {chunk-size 1024}}]
  (if (nil? input-stream)
    (throw (IllegalArgumentException. "Illegal input stream: nil!")))
  (letfn [(read-chunks [available]
            (lazy-seq
              (if (or (nil? available) (pos? available))
                (let [chunk-size  (if available
                                    (min available chunk-size)
                                    chunk-size)
                      bytes       (read-byte-array input-stream chunk-size)
                      number-read (alength bytes)]
                  (if (pos? number-read)
                    (cons bytes
                          (read-chunks (if available
                                         (- available number-read)))))))))]
    (read-chunks available)))
