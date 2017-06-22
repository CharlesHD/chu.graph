(ns chu.graph.func-graph-test
  (:require [chu.graph.func-graph :as sut]
            [clojure.spec.alpha :as s]))

(def opts {:clojure.spec.test.check/opts {:num-tests 20 :max-size 50}
           :gen {:chu.graph/graph
                 (fn [] (s/gen :chu.graph.func-graph/func-graph))}})

(defspec-test nodes
  `g/nodes
  opts)

(defspec-test links
  `g/links
  opts)

(defspec-test adjency
  `g/adjency
  opts)

(defspec-test ancestry
  `g/ancestry
  opts)

(defspec-test reversed
  `g/reversed
  opts)

(defspec-test map-link
  `g/map-link
  opts)

(defspec-test map-node
  `g/map-node
  opts)

(defspec-test filter-node
  `g/filter-node
  opts)

(defspec-test filter-link
  `g/filter-link
  opts)

(defspec-test in-degrees
  `g/in-degrees
  opts)

(defspec-test out-degrees
  `g/out-degrees
  opts)

(defspec-test degrees
  `g/degrees
  opts)

(defspec-test empty-graph
  `g/empty-graph
  opts)

(defspec-test add-node
  `g/add-node
  opts)

(defspec-test add-link
  `g/add-link
  opts)

(defspec-test add-graph
  `g/add-graph
  opts)

(defspec-test intersection-graph
  `g/intersection-graph
  opts)

(defspec-test reduce-graph
  `g/reduce-graph
  opts)

(defspec-test make-graph
  `g/make-graph
  opts)
