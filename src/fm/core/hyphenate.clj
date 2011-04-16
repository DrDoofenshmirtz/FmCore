(ns
  #^{:doc "Convert camel cased identifiers to lispy hyphenated ones."
     :author "Frank Mosebach"}
  fm.core.hyphenate
  (:use [clojure.contrib.def :only (defvar-)]))

(defvar- +separators+ #{\_ \-})

(defn- char-tag [char]
  (and
    char
    (cond
      (Character/isUpperCase char) ::upper-case
      (Character/isLowerCase char) ::lower-case
      (+separators+ char) ::separator
      :else ::undefined)))

(defn- append-separator [string char]
  (and (= ::separator (char-tag char)) (str string char)))

(defn- ends-with-hump? [string]
  (let [[last-char separator] (reverse string)
        last-char-tag (char-tag last-char)]
    (or
      (= [::upper-case \-] [last-char-tag separator])
      (= [::upper-case nil] [last-char-tag separator]))))

(defn- flatten-hump [string char]
  (and
    (= ::lower-case (char-tag char))
    (ends-with-hump? string)
    (str
      (apply str (butlast string))
      (Character/toLowerCase (last string))
      char)))

(defn- char-tag-changed? [string char]
  (not= (char-tag (last string)) (char-tag char)))

(defn- append-char [string char]
  (str
    string
    (if (and
          (char-tag-changed? string char)
          (not= ::separator (char-tag (last string)))) "-")
    char))

(defn- append [string char]
  (if (empty? string)
    (str char)
    (or
      (append-separator string char)
      (flatten-hump string char)
      (append-char string char))))

(defn hyphenate
  "Converts the given camel cased name to a hyphenated one."
  [name]
  (reduce append "" name))
