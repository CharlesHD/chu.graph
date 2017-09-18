(ns chu.graph.build
  (:require [chu.graph.adjency-graph :as adjency-graph]
            [chu.graph.func-graph :as func-graph]
            [chu.graph.linked-graph :as linked-graph]))

(def implementation (atom :linked-graph))

(defn set-default-implementation!
  "Use to change default graph implementation.
  Accept :linked-graph or :adjency-graph"
  [implem-key]
  (swap! implementation (constantly implem-key)))

(defmulti default-empty-graph identity)

(defmethod default-empty-graph :linked-graph
  [_]
  linked-graph/EMPTY)

(defmethod default-empty-graph :adjency-graph
  [_]
  adjency-graph/EMPTY)

(defmethod default-empty-graph :func-graph
  [_]
  func-graph/EMPTY)

(defn empty-graph
  "Main function of the module. Return current default empty-graph."
  []
  (default-empty-graph @implementation))
