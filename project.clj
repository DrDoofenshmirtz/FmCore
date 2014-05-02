;; Leiningen project file for the FmCore clojure project.

(defproject fm/fm-core "1.0.0-SNAPSHOT"
  :description "FmCore: Basic Clojure Utilities."  
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]]  
  :disable-deps-clean true    
  :jar-name "fm-core.jar"           
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/" 
                   #"(?:^|/).git/" 
                   #"(?:^|/)project.clj"])

