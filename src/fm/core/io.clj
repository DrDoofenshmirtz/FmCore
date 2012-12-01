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

(defn ^bytes read-byte-array
  "Reads a byte array from the given (non-nil) input stream and returns it.
  Returns nil if the end of the input stream has been reached before the
  requested number of bytes could be read.

  Optional keyword arguments (at least one must be specified!):

  :buffer A byte array that will be used to store the read bytes.
          If not specified, a byte array sized according to the given
          :length option will be created.

  :length An integer specifying the number of bytes to be read.
          Defaults to the size of the given buffer if not specified."
  [^InputStream input-stream & {:keys [buffer length]}]
  (if-not (or buffer length)
    (throw (IllegalArgumentException.
      "At least one of the :buffer or :lenght options must be specified!")))
  (if (nil? input-stream)
    (throw (IllegalArgumentException. "Illegal input stream: nil!")))
  (let [^bytes buffer (or buffer (byte-array length))
        buffer-size   (alength buffer)
        length        (int (if length (min buffer-size length) buffer-size))]
    (loop [offset 0 length length]
      (if (pos? length)
        (let [number-read (.read input-stream buffer offset length)]
          (if-not (neg? number-read)
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
  [^InputStream input-stream &
   {:keys [available chunk-size] :or {chunk-size 1024}}]
  (if (nil? input-stream)
    (throw (IllegalArgumentException. "Illegal input stream: nil!")))
  (let [chunk-size    (if available (min available chunk-size) chunk-size)
        ^bytes buffer (byte-array chunk-size)]
    (filter
      (complement nil?)
      ((fn read-chunks [available chunk-size]
         (lazy-seq
           (if (pos? chunk-size)
             (let [number-read (.read input-stream buffer 0 chunk-size)]
               (cond
                 (neg? number-read)  nil
                 (zero? number-read) (cons nil
                                           (read-chunks available chunk-size))
                 :else (if available
                         (let [available  (- available number-read)
                               chunk-size (min chunk-size available)]
                           (cons (byte-array number-read buffer)
                                 (read-chunks available chunk-size)))
                         (cons (byte-array number-read buffer)
                               (read-chunks available chunk-size))))))))
       available
       chunk-size))))
