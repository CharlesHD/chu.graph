(def +version+ "0.0.1")

(set-env!
 :source-paths #{"src"}
 :target-path "tmp"
 :dependencies '[[org.clojure/clojure "1.8.0"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [chulper "1.1.1"]
                 [org.clojure/core.async "0.3.443"]])

(require '[adzerk.bootlaces :refer :all])

(task-options!
 pom {:project 'chu.graph
      :version +version+}
 jar {:manifest {"Foo" "bar"}})

(bootlaces! +version+)
