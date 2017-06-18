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
  (:require [clojure.set :as set]
            [chu.graph
             :refer
             [->Link
              add-node
              default-graph-protocol-mixin
              GraphProtocol
              flip-link
              links
              nodes]]))

(defn links->adjency
  "Return the map where graph nodes are keys and vals are sequence of nodes adjacent to the key node.
  Exemple : a -> b; a -> c; b -> c => {a #{b c} b #{c} c #{}}"
  [nodes links]
  (reduce (fn [m {x :from y :to}] (update m x #(conj % y)))
          (zipmap nodes (repeat #{})) links))

(defrecord LinkedGraph [nds lks])

(def EMPTY (->LinkedGraph #{} #{}))

(def linked-graph-mixin
  {:nodes (fn [g] (:nds g))
   :links (fn [g] (:lks g))
   :adjency (fn [g] (links->adjency (:nds g) (:lks g)))

   :reversed (fn [g]
               (->LinkedGraph
                (nodes g)
                (set (map flip-link (links g)))))

   :empty-graph (fn [_] EMPTY)

   :map-node (fn [g f]
               (->LinkedGraph
                (set (map f (nodes g)))
                (reduce (fn [lks {fr :from to :to}]
                          (conj lks (->Link (f fr) (f to))))
                          #{}           ;; use a set to ensure unicity if f is surjective.
                          (links g))))

   :filter-node (fn [g pred]
                  (let [mpred (memoize pred)]
                    (->LinkedGraph
                     (set (filter mpred (nodes g)))
                     (set (filter (fn [{fr :from to :to}] (and (mpred fr) (mpred to))) (links g))))))

   :filter-link (fn [g pred] (->LinkedGraph (nodes g) (filter pred (links g))))

   :add-node (fn [g n]
               (update g :nds conj n))

   :add-link (fn [g l]
               (-> g (add-node (:from l)) (add-node (:to l)) (update :lks conj l)))})

(extend LinkedGraph GraphProtocol (merge default-graph-protocol-mixin linked-graph-mixin))
