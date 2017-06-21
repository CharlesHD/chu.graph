(ns chu.graph.protocol
  (:require [chu.link :as l :refer [flip make-link]]
            [chulper.core :refer [map-vals]]
            [clojure.set :as set]))

(defprotocol GraphProtocol
  "That's a graph.
   It's specified by an ensemble of nodes and links which are ordered couple of such nodes"
  (nodes [g]
    "Set of node of the graph")
  (links [g]
    "List of links of the graph. A well formed graph has links wrapped in the Link record")
  (adjency [g]
    "The map where graph nodes are keys and vals are maps of nodes adjacent to the key node with links params as value.
    Exemple : a -> b(p1) ; a -> c(p2) ; b -> c(p3) => {a {b p1, c p2} b {c p3} c {}}")
  (ancestry [g]
    "The map where graph nodes are keys and vals are of ancestors of the key node with link param as value.
    Exemple : a -> b(p1); a -> c(p2); b -> c(p3) => {a #{} b {a p1} c {a p2, b p3}}")
  (reversed [g]
    "The same graph but with every links reversed : a -> b => b -> a")
  (map-link [g f]
    "Transform links params of the graph using `f` : a -> b(p) => a ->b(f a b p).
    `f` should take a link as parameter and return the new links param map.")
  (map-node [g merge-params f]
    "Transform nodes of the graph using `f` : a -> b => (f a) -> (f b).
   `merge-params` is used in case of transformation collision.
   Case where (f n1) = (f n2) = c is correctly handle :
   - n1 -> a(p1) ; n2 -> b(p2) => c -> a(p1) ; c -> b(p2)
   - a -> n1(p1) ; b -> n2(p2) => a -> c(p1) ; b -> c(p2)
   - a -> n1(p1) ; a -> n2(p2) => a -> c(merge-params p1 p2)
   - n1 -> a(p1) ; n2 -> a(p2) => c -> a(merge-params p1 p2)")
  (filter-node [g pred]
    "Make a graph where you keep only nodes verifying pred.")
  (filter-link [g pred]
    "Make a graph where you only keep links verifying pred.")
  (in-degrees [g]
    "return a map where nodes are keys and val is the in-degree of the keynode.")
  (out-degrees [g]
    "return a map where nodes are keys and val is the out-degree of the keynode.")
  (degrees [g]
    "return a map where nodes are keys and val is the total degree of the keynode.")
  (empty-graph [g]
    "The same graph but with no nodes, no links.")
  (add-node [g n]
    "Same graph with node n added.")
  (add-link [g merge-params l]
    "Same graph with link l added. Add nodes involved in the link if not present.
    If the link is already present `merge-params` resolve params conflict.")
  (add-graph [g merge-params g2]
    "Add the graph g2 to the graph g. It's the union of nodes and links of both graph.
    In case links are present in both graphs `merge-params` resolve params conflict.")
  (intersection-graph [g merge-params g2]
    "The graph containing only nodes and links presents in both graphs.
     `merge-params` resolve params conflict."))

(defn reduce-graph
  "Reduce through a graph.
  First reduce through nodes using `nf`.
  Then reduce through links using `lf`"
  [nf lf init g]
  (reduce lf (reduce nf init (nodes g)) (links g)))

(defn make-graph
  [g mg ns lks]
  (reduce #(add-link %1 mg %2)
          (reduce add-node (empty-graph g) ns)
          lks))

(defn adjency->links
  "Given the adjency of a graph, get you the links of the graph"
  [adjency]
  (for [[x adj] adjency
        [y p] adj]
    (make-link x y p)))

(defn default-add-graph
  "Add the graph g2 to the graph g. It's the union of nodes and links of both graph."
  [g merge-params g2]
  (reduce-graph
   add-node
   #(add-link %1 merge-params %2)
   g g2))

(defn default-intersection-graph
  "The graph containing only nodes and links presents in both graphs"
  [g1 merge-params g2]
  (let [e (empty-graph g1)
        l1 (links g1) l2 (links g2)
        nds (set/intersection (set (nodes g1)) (set (nodes g2)))
        lks (set/intersection (set l1) (set l2))]
    (reduce #(add-link %1 merge-params %2)
            (reduce add-node e nds)
            ;; you have to process twice the links in order to merge params
            (concat (filter lks l1) (filter lks l2)))))

(defn default-map-node
  [g merge-params f]
  (let [mf (memoize f)]
    (reduce-graph
     #(add-node %1 (mf %2))
     #(add-link %1 merge-params (l/make-link (mf (:from %2))
                                             (mf (:to %2))
                                             (:params %2)))
     (empty-graph g) g)))

(defn default-map-link
  "Change links params of `g` using `f`. See chu.link/update-params for more info.
  f as the signature (f link old-link-params) and returns new link params"
  [g f]
  (reduce #(add-link %1 merge %2)
          (reduce add-node (empty-graph g) (nodes g))
          (map (partial l/update-params f) (links g))))


(defn default-filter-link
  [g pred]
  (make-graph g merge (nodes g) (set (filter pred (links g)))))

;; Default Implementation of the Graph Protocol.
;; No default implementation for :
;; adjency, map-node, filter-node
(def default-graph-protocol-mixin
  {:nodes (fn [g] ((comp set keys adjency) g))
   :links (fn [g] ((comp adjency->links adjency) g))
   :reversed (fn [g] (reduce add-link
                             (reduce add-node
                                     (empty-graph g)
                                     (nodes g))
                             (map l/flip (links g))))
   :ancestry (fn [g] ((comp adjency reversed) g))
   :in-degrees (fn [g] (map-vals count (adjency g)))
   :out-degrees (fn [g] (map-vals count (ancestry g)))
   :degrees (fn [g] (merge-with + (in-degrees g) (out-degrees g)))
   :map-node default-map-node
   :map-link default-map-link
   :filter-link default-filter-link
   :add-graph default-add-graph
   :intersection-graph default-intersection-graph})
