(def +version+ "0.2.2")

(set-env!
 :source-paths #{"src"}
 :target-path "tmp"
 :dependencies '[[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/tools.reader "1.2.1"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 [adzerk/boot-test "1.2.0" :scope "test"]
                 [adzerk/boot-cljs "2.1.3" :scope "test"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [tailrecursion/cljs-priority-map "1.2.1"]
                 [chulper "1.2.3"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/test.check "0.9.0"]])

(require '[adzerk.bootlaces :refer :all])
(require '[adzerk.boot-test :refer :all])
(require '[adzerk.boot-cljs :refer [cljs]])

(task-options!
 pom {:project 'chu.graph
      :version +version+
      :description "a graph library"
      :url "https://github.com/CharlesHD/chu.graph"
      :scm {:url "https://github.com/CharlesHD/chu.graph"}}
 jar {:manifest {"Foo" "bar"}})

(bootlaces! +version+)

(deftask testing
  "Profile setup for running tests."
  []
  (set-env! :source-paths #(conj % "test"))
  identity)
