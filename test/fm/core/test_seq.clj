(ns fm.core.test-seq
  (:use (clojure test)
        (fm.core seq)))

(deftest produce-nothing
  (is (nil? (produce nil)))
  (is (nil? (produce [])))
  (is (nil? (produce [1 2 3]))))

(deftest produce-empty-coll  
  (let [produced (first (produce [] (fn [_] nil)))]
    (is (not (nil? produced)))
    (is (empty? produced))))

(deftest produce-single-nil
  (is (nil? (first (produce nil (fn [_] nil)))))
  (is (nil? (first (produce nil (fn [e] e))))))

(deftest produce-single-copy
  (let [coll [1 2 3 4 5 6 7 8 9 10]]
    (is (= (first (produce coll (fn [e] e))) coll))))

(deftest produce-multiple-copies
  (let [n 3
        in-coll [1 2 3 4 5 6 7 8 9 10]
        produced (apply produce in-coll (repeat n (fn [e] e)))]
    (doseq [coll produced]
      (is (= coll in-coll)))))

(deftest produce-disjoint-colls
  (let [in-coll [1 2 3 4 5 6 7 8 9 10]
        even-coll (filter even? in-coll)
        odd-coll (filter odd? in-coll)
        produced (produce in-coll #(if (even? %) % nil) #(if (odd? %) % nil))]
    (is (= even-coll (first produced)))
    (is (= odd-coll (second produced)))))

(deftest produce-multiple-colls
  (let [in-coll [1 2 3 4 5 6 7 8 9 10]
        out-coll-1 [1 2 3 4]
        out-coll-2 [3 4 5 6]
        out-coll-3 [5 6 7 8]
        out-coll-4 [7 8 9 10]
        produced (produce 
                   in-coll
                   #(if (and (> % 0) (< % 5)) % nil)
                   #(if (and (> % 2) (< % 7)) % nil)
                   #(if (and (> % 4) (< % 9)) % nil)
                   #(if (and (> % 6) (< % 11)) % nil))]
    (is (some #(= % out-coll-1) produced))
    (is (some #(= % out-coll-2) produced))
    (is (some #(= % out-coll-3) produced))
    (is (some #(= % out-coll-4) produced))))
