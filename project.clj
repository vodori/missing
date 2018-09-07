(defproject com.vodori/missing "0.1.0-SNAPSHOT"
  :description "A utility library for Clojure of functions and macros that are frequently missed and recreated."

  :url
  "https://github.com/vodori/missing"

  :license
  {:name "MIT License" :url "http://opensource.org/licenses/MIT" :year 2018 :key "mit"}

  :scm
  {:name "git" :url "https://github.com/vodori/missing"}

  :pom-addition
  [:developers
   [:developer
    [:name "Paul Rutledge"]
    [:url "https://github.com/rutledgepaulv"]
    [:email "rutledgepaulv@gmail.com"]
    [:timezone "-5"]]]

  :deploy-repositories
  [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/" :creds :gpg}
    "snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/" :creds :gpg}]]

  :dependencies [[org.clojure/clojure "1.10.0-alpha7"]])
