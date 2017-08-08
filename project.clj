(defproject traffic "0.1.0-SNAPSHOT"
  :description "A quick & dirty system for tracking cars and detecting speed"
  :url "https://github.com/ashenfad/traffic"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx4g" "-server"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.openimaj/xuggle-video "1.3.5"]
                 [org.openimaj/image-processing "1.3.5"]
                 [org.openimaj/core-video "1.3.5"]
                 [bigml/clj-bigml "0.3.0"]
                 [com.climate/claypoole "1.1.4"]
                 [clj-http "3.6.1"]])
