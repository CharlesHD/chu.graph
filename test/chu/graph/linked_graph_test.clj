(ns chu.graph.linked-graph-test
  (:require [chu.graph :as g]
            [chu.graph.linked-graph :as sut]
            [chu.test :refer [defspec-test]]
            [clojure.spec.alpha :as s]))

(def opts {:clojure.spec.test.check/opts {:num-tests 20
                                          :max-size 50}
           :gen {:chu.graph/graph (fn [] (clojure.spec.alpha/gen :chu.graph.linked-graph/linked-graph))}})

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

(defspec-test remove-node
  `g/remove-node
  opts)

(defspec-test remove-link
  `g/remove-link
  opts)

(defspec-test undirect
  `g/undirect
  opts)

(defspec-test line-graph
  `g/line-graph
  opts)

(defspec-test seed-graph
  `g/seed-graph
  opts)

(defspec-test remove-unlinked-nodes
  `g/remove-unlinked-nodes
  opts)

(defspec-test dfs
  `g/dfs
  opts)

(defspec-test scc
  `g/scc
  opts)

(defspec-test dijkstra
  `g/dijkstra
  opts)

(defspec-test components
  `g/components
  opts)

(defspec-test weak-components
  `g/weak-components
  opts)

(defspec-test graph-coverage
  `g/graph-coverage
  opts)

(defspec-test chain-graph
  `g/chain-graph
  opts)
