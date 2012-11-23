(ns
  ^{:doc "IO utilities."
    :author "Frank Mosebach"}
  fm.core.io
  (:import
    (java.io InputStream ByteArrayOutputStream)))

(defn ^bytes read-until-detected
  "Reads bytes from the given (non-nil) input stream until the given sequence
  of expected bytes has been read in the specified order or the end of the
  input stream has been reached.
  If successful, returns a byte array containing all bytes that have been read
  (including the sequence itself), otherwise nil."
  [^InputStream input-stream expected]
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
