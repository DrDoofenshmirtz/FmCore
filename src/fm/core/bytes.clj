(ns
  ^{:doc "Tools for fumbling with bytes."
    :author "Frank Mosebach"}
  fm.core.bytes)

(defn signed-byte
  "Converts the given number to a java byte value in the range [-128..127].
  Unlike the clojure.core.byte function, it doesn't throw an exception if
  the given value is out of the aforementioned range."
  [^Number number]
  (.byteValue number))

(defn unsigned-byte
  "Converts the given number to a signed byte value, i. e. a value in the
  range [0..255]."
  [number]
  (bit-and number 0xFF))

(defn number<-bytes
  "Builds a number from the given sequence of bytes, left to right. The first
  byte will be the leftmost byte of the resulting number, the last byte will
  be the rightmost byte."
  [bytes]
  (letfn [(shift-left-and-add [number bytes]
            (if (seq bytes)
              (recur
                (bit-or
                  (bit-shift-left number 8)
                  (unsigned-byte (first bytes)))
                (rest bytes))
              number))]
    (shift-left-and-add 0 bytes)))

(defn number->bytes
  "Builds a sequence from the bytes of the given number, left to right.
  The leftmost byte of the number will be the first byte of the resulting
  sequence, the rightmost byte will be the last one."
  [number]
  (letfn [(cons-and-shift-right [number bytes]
            (if (zero? number)
              bytes
              (let [rightmost-byte (bit-and 0xFF number)
                    number (bit-shift-right number 8)
                    bytes (cons rightmost-byte bytes)]
                (recur number bytes))))]
    (cons-and-shift-right number ())))
