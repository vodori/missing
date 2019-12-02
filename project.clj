(defproject com.vodori/missing "0.1.19-SNAPSHOT"

  :description
  "A utility library for Clojure of functions and macros that are frequently missed and recreated."

  :url
  "https://github.com/vodori/missing"

  :license
  {:name "MIT License" :url "http://opensource.org/licenses/MIT" :year 2019 :key "mit"}

  :scm
  {:name "git" :url "https://github.com/vodori/missing"}

  :pom-addition
  [:developers
   [:developer
    [:name "Julia Cicale"]
    [:url "https://github.com/jcicale"]
    [:email "julia.cicale@vodori.com"]
    [:timezone "-5"]]
   [:developer
    [:name "Griffin Lacek"]
    [:url "https://github.com/griffinlacek"]
    [:email "griffin.lacek@vodori.com"]
    [:timezone "-5"]]
   [:developer
    [:name "Paul Rutledge"]
    [:url "https://github.com/rutledgepaulv"]
    [:email "paul.rutledge@vodori.com"]
    [:timezone "-5"]]
   [:developer
    [:name "Travis Stom"]
    [:url "https://github.com/travisstom"]
    [:email "travis.stom@vodori.com"]
    [:timezone "-4"]]]

  :deploy-repositories
  {"releases"  {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/" :creds :gpg}
   "snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/" :creds :gpg}}

  :dependencies [[org.clojure/clojure "1.10.1"]])
