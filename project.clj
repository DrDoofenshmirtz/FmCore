(def local-repo-path (-> (java.io.File. "/home/frank/clojure/mvn-repo") .toURI str))

(defproject fm/core "1.0.2"
  :description "FmCore: Basic Clojure Utilities."  
  :dependencies [[fm.clojure/clojure "1.2.0"]
                 [fm.clojure/clojure-contrib "1.2.0"]]  
  :disable-deps-clean true    
  :repositories {"fm-local" ~local-repo-path}
  :jar-name "fm-core.jar"           
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/" 
                   #"(?:^|/).git/" 
                   #"(?:^|/)project.clj"
                   #"(?:^|/)scratch.clj"
                   #"(?:^|/)canvas.clj"])

