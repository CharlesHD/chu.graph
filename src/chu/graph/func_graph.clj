(ns chu.graph.func-graph
  "Func Graph keep only the node list and a predicate over Links.
  Most of algorithmic costs are O(V^2)
  but memory usage is only O(V + F) where F is the predicate memory usage."
  (:require [chu.graph :as g]
            [chulper.core :as h]
            [clojure.set :as set]))

(defrecord FuncGraph [nodes p])

(def EMPTY (->FuncGraph (hash-set) (constantly false)))

(defn- xf
  [{p :p}]
  (filter p))

(defn- lks
  [{nds :nodes}]
  (for [x nds
        y nds]
    (g/->Link x y)))

(defn- nodes
  [{nds :nodes}] nds)

(defn- links
  [g]
  (sequence (xf g) (lks g)))

(defn- adjency
  [{nds :nodes p :p}]
  (fn [x] (for [y nds
                :let [l (g/->Link x y)]
                :when (p l)]
            y)))

(defn- reversed
  [{nds :nodes p :p}]
  (->FuncGraph nds (h/key-fn g/flip-link p)))

(defn- map-node
  [{nds :nodes p :p} f]
  (let [f-inverse (zipmap (map f nds) nds)]
    (->FuncGraph (vals f-inverse) (h/key-fn (partial h/map-vals f-inverse) p))))

(defn- filter-link
  [{nds :nodes p :p} pred]
  (->FuncGraph nds (every-pred p pred)))

(defn- filter-node
  [{nds :nodes p :p} pred]
  (->FuncGraph (filter pred nds) p))

(defn- empty-graph
  [g]
  EMPTY)

(defn- add-node
  [{nds :nodes p :p} n]
  (->FuncGraph (conj (set nds) n) p))

(defn- add-link
  [{nds :nodes p :p} l]
  (->FuncGraph nds (some-fn p #{l})))

(defn- add-graph
  [{nds :nodes p :p :as g} g2]
  (if (instance? FuncGraph g2)
    (->FuncGraph (set/union nds (nodes g2)) (some-fn p (:p g2)))
    (g/default-add-graph g g2)))

(defn- intersection-graph
  [{nds :nodes p :p :as g} g2]
  (if (instance? FuncGraph g2)
    (->FuncGraph (set/intersection nds (nodes g2)) (every-pred p (:p g2)))
    (g/default-intersection-graph g g2)))

(def funcgraph-mixin
  {:nodes nodes
   :links links
   :adjency adjency
   :map-node map-node
   :filter-node filter-node
   :filter-link filter-link
   :reversed reversed
   :empty-graph empty-graph
   :add-link add-link
   :add-node add-node
   :add-graph add-graph
   :intersection-graph intersection-graph
   })

(extend FuncGraph g/GraphProtocol (merge g/default-graph-protocol-mixin funcgraph-mixin))
