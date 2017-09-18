(ns chu.graph.linked-graph
  "Linked Graph keeps only nodes and links in memory.
  Here is algorithmic cost of that structure :
  For V numbers of nodes, E numbers of links
  Memory usage : O(V + E)
  adjency : O(E)
  nodes : O(1)
  links : O(1)
  reversed : O(E)
  in-degrees : O(E + V)
  out-degrees : O(E + V)
  degrees : O(E + V)
  map-node, filter-node : O(E + V)
  add-node, add-link : O(1*)
  * : In fact it's something like log_32(n)"
  (:require [chu.graph.protocol :as prot :refer [GraphProtocol]]
            [chu.link :as l]
            [chu.graph :as g]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.set :as set]))

(defrecord LinkedGraph [nds lks])

;; spec
(defn- correct-linked-graph
  [{nds :nds lks :lks}]
  (set/subset? (reduce #(conj %1 (:from %2) (:to %2)) #{} lks) nds))

(s/def ::nds :chu.graph/nodes)
(s/def ::lks :chu.graph/links)
(defn- linked-graph-generator
  []
  (gen/bind (s/gen ::nds)
            (fn [nds]
              (gen/fmap
               #(->LinkedGraph nds %)
               (if (empty? nds)
                 (s/gen (s/and empty? set?))
                 (s/gen ::lks
                        {:chu.link/node (fn [] (s/gen nds))}))))))
(s/def ::linked-graph
  (s/with-gen
    (s/and #(instance? LinkedGraph %)
           #_correct-linked-graph)
    linked-graph-generator))

(s/fdef ->LinkedGraph
        :args (s/and (s/cat :nds ::nds :lks ::lks)
                     correct-linked-graph)
        :ret ::linked-graph)

(def EMPTY (->LinkedGraph #{} #{}))

(defn links->adjency
  "Return the map where graph nodes are keys and vals are sequence of nodes adjacent to the key node.
  Exemple : a -> b(p1); a -> c(p2); b -> c(p3) => {a {b p1, c p2} b {c p3} c {}}"
  [nodes links]
  (reduce (fn [m {x :from y :to p :params}]
            (update m x #(assoc % y p)))
          (zipmap nodes (repeat {})) links))

(defn- set-conj-link
  [mg s l]
  (if (some (partial l/loosely-equal l) s)
    (->> s
         (map #(if (l/loosely-equal % l)
                 (update % :params mg (:params l))
                 %))
         set)
    (conj s l)))

(extend-type LinkedGraph
  GraphProtocol
  ;; specific
  (nodes [g] (:nds g))
  (links [g] (:lks g))
  (adjency [g] (links->adjency (:nds g) (:lks g)))
  (reversed [g] (->LinkedGraph
                 (g/nodes g)
                 (set (map l/flip (g/links g)))))
  (empty-graph [g] EMPTY)
  (filter-node [g pred]
    (let [mpred (memoize pred)]
      (->LinkedGraph
       (set (filter mpred (g/nodes g)))
       (set (filter (fn [{fr :from to :to}] (and (mpred fr) (mpred to))) (g/links g))))))
  (filter-link [g pred] (prot/default-filter-link g pred))
  (add-node [g node]
    (update g :nds conj node))
  (add-link [g mg link]
    (-> g
        (g/add-node (:from link))
        (g/add-node (:to link))
        (update :lks (partial set-conj-link mg) link)))
  ;; generic
  (ancestry [g] (prot/default-ancestry g))
  (in-degrees [g] (prot/default-in-degrees g))
  (out-degrees [g] (prot/default-out-degrees g))
  (degrees [g] (prot/default-degrees g))
  (map-node [g mg f] (prot/default-map-node g mg f))
  (map-link [g f] (prot/default-map-link g f))
  (add-graph [g mg g2] (prot/default-add-graph g mg g2))
  (intersection-graph [g mg g2] (prot/default-intersection-graph g mg g2)))
