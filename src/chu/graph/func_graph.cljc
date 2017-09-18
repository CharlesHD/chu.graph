(ns chu.graph.func-graph
  "Func Graph keep only the node list, a predicate over Links and a param function over links.
  Most of algorithmic costs are O(V^2)
  but memory usage is only O(V + P + F) where P is the predicate memory usage and F the param function one."
  (:require [chu.graph.protocol
             :as prot
             :refer
             [default-add-graph
              default-intersection-graph
              GraphProtocol]]
            [chu.link :as l]
            [chulper.core :as h]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(defrecord FuncGraph [nodes p f])

(def EMPTY (->FuncGraph (hash-set) (constantly false) (constantly {})))
;;spec
(s/def ::nodes :chu.graph/nodes)
(s/def ::p (s/fspec :args (s/cat :l :chu.link/link)))
(s/def ::f (s/fspec :args (s/cat :l :chu.link/link)
                    :ret :chu.link/params))
(s/def ::proto-func-graph (s/keys :req-un [::nodes ::p ::f]))
(s/def ::func-graph (s/with-gen
                      #(instance? FuncGraph %)
                      #(gen/fmap map->FuncGraph (s/gen ::proto-func-graph))))

(defn- xf
  [{p :p f :f}]
  (filter p)
  (map #(assoc % :params (f %))))

(defn- lks
  [{nds :nodes}]
  (for [x nds
        y nds]
    (l/make-link x y {})))

(defn- nodes
  [{nds :nodes}] nds)

(defn- links
  [g]
  (into (hash-set) (xf g) (lks g)))

(defn- adjency
  [{nds :nodes p :p f :f}]
  (fn [x]
    (reduce #(let [l (l/make-link x %2)]
               (if (p l)
                 (assoc %1 %2 (f l))
                 %1))
            {} nds)))

(defn- reversed
  [{nds :nodes p :p f :f}]
  (->FuncGraph nds (comp p l/flip) (comp f l/flip)))

(defn- map-node
  [{nds :nodes p :p fa :f} mg f]
  (let [f-inverse (zipmap nds (map f nds))]
    (->FuncGraph (vals f-inverse)
                 (comp p (partial h/map-vals f-inverse))
                 (comp fa (partial h/map-vals f-inverse)))))

(defn- filter-link
  [{nds :nodes p :p f :f} pred]
  (->FuncGraph nds (every-pred p pred) f))

(defn- filter-node
  [{nds :nodes p :p f :f} pred]
  (->FuncGraph (filter pred nds) p f))

(defn- empty-graph
  [g]
  EMPTY)

(defn- add-node
  [{nds :nodes p :p f :f} n]
  (->FuncGraph (conj (set nds) n) p f))

(defn- add-link
  [{nds :nodes p :p f :f} mg l]
  (->FuncGraph nds
               (some-fn p #{l})
               (fn [l2] (if (= l l2) (mg (f l2) l) (f l2)))))

(defn- add-graph
  [{nds :nodes p :p :as g} mg g2]
  (if (instance? FuncGraph g2)
    (->FuncGraph (set/union nds (nodes g2))
                 (some-fn p (:p g2))
                 (fn [l] (mg ((:f g) l)
                             ((:f g2) l))))
    (default-add-graph g mg g2)))

(defn- intersection-graph
  [{nds :nodes p :p :as g} mg g2]
  (if (instance? FuncGraph g2)
    (->FuncGraph (set/intersection nds (nodes g2))
                 (every-pred p (:p g2))
                 (fn [l] (mg ((:f g) l)
                             ((:f g2) l))))
    (default-intersection-graph mg g g2)))

(extend-type FuncGraph
  GraphProtocol
  ;; specific
  (nodes [g] (nodes g))
  (links [g] (links g))
  (adjency [g] (adjency g))
  (map-node [g mg f] (map-node g mg f))
  (filter-node [g pred] (filter-node g pred))
  (filter-link [g pred] (filter-link g pred))
  (reversed [g] (reversed g))
  (empty-graph [g] (empty-graph g))
  (add-node [g node] (add-node g node))
  (add-link [g mg link] (add-link g mg link))
  (add-graph [g mg g2] (add-graph g mg g2))
  (intersection-graph [g mg g2] (intersection-graph g mg g2))
  ;; generic
  (ancestry [g] (prot/default-ancestry g))
  (in-degrees [g] (prot/default-in-degrees g))
  (out-degrees [g] (prot/default-out-degrees g))
  (degrees [g] (prot/default-degrees g))
  (map-link [g f] (prot/default-map-link g f)))
