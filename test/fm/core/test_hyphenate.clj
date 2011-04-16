(ns
  #^{:doc "Tests for fm.core.hyphenate."
     :author "Frank Mosebach"}
  fm.core.test-hyphenate
  (:use [clojure.test :only (deftest is)])
  (:require [fm.core.hyphenate :as hy]))

(deftest hyphenate
  (is (= "" (hy/hyphenate "")))
  (is (= 
        "first-second-third-fourth-fifth"
        (hy/hyphenate "firstSecondThirdFourthFifth")))
  (is (= 
        "first-second-third-fourth-fifth"
        (hy/hyphenate "FirstSecondThirdFourthFifth")))
  (is (= "FIRST_SECOND-third" (hy/hyphenate "FIRST_SECONDthird")))
  (is (= "FIRST_SECOND_N-ext" (hy/hyphenate "FIRST_SECOND_Next")))
  (is (= "first-SECOND-third" (hy/hyphenate "firstSECONDthird")))
  (is (= 
        "section-1-section-2-section-3"
        (hy/hyphenate "section1Section2Section3"))))

(defn run-tests []
  (clojure.test/run-tests 'fm.core.test-hyphenate))