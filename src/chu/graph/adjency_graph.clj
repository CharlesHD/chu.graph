(ns chu.graph.adjency-graph
  "Adjency Graph keep only adjacence map.
  Here is algorithmic cost of that structure :
  For V numbers of nodes, E numbers of links
  Memory usage : O(V + E)
  adjency : O(1)
  nodes : O(1*)
  links : O(E)
  reversed : O(E)
  in-degrees : O(V)
  out-degrees : O(E + V)
  degrees : O(E + V)
  map-node, filter-node : O(E + V)
  add-node, add-link : O(1*)
  * : In fact it's something like log_32(n)"
  (:require [clojure.core.async :as a]
            [clojure.set :as set]
            [chu.graph
             :refer
             [->Link
              add-link
              add-node
              adjency
              default-graph-protocol-mixin
              GraphProtocol
              links
              nodes]]
            [chulper.core :refer [map-keys map-vals]]))

;; adjency, reversed, map-node, filter-node
(defrecord AdjencyGraph [adj])

(def EMPTY (->AdjencyGraph {}))

(defn- adj-map-node
  [g f]
  (let [mf (memoize f)]
    (as-> (adjency g) res
      (map-keys mf res set/union)
      (map-vals (partial reduce #(conj %1 (mf %2)) #{}) res)
      (->AdjencyGraph res))))

(defn- adj-filter-node
  [g pred]
  (let [mpred (memoize pred)
        adj (adjency g)]
    (->>
     (adjency g)
     (reduce-kv
      (fn [m k v] (if (mpred k)
                    (assoc m k (into #{} (filter mpred) v))
                    m)) {})
     (->AdjencyGraph))))

(defn- adj-filter-link
  [g pred]
  (let [gmake (a/chan (a/buffer (count (links g))))
        total (count (links g))
        adj (adjency g)]
    (doseq [i (nodes g)]
      (a/go (doseq [j (adj i)]
              (if (pred (->Link i j))
                (a/>! gmake (->Link i j))
                (a/>! gmake false)))))
    (loop [g (reduce add-node EMPTY (nodes g))
           treated 0]
      (if (= treated total)
        g
        (let [i (a/<!! gmake)
              ng (if i (add-link g i) g)]
          (recur ng (inc treated)))))))

(def adjency-graph-mixin
  {:adjency (fn [g] (:adj g))

   :empty-graph (fn [_] EMPTY)

   :map-node adj-map-node

   :filter-node adj-filter-node

   :filter-link adj-filter-link

   :add-node (fn [g n]
               (if (get-in g [:adj n])
                 g ;; node already exists, do nothing.
                 (assoc-in g [:adj n] #{})))

   :add-link (fn [g {fr :from to :to}]
               (-> (add-node g fr)
                   (add-node to)
                   (update-in [:adj fr] conj to)))

   :add-graph (fn [g g2]
                (let [adj2 (adjency g2)]
                  (update g :adj (partial merge-with set/union adj2))))})

(extend AdjencyGraph GraphProtocol (merge default-graph-protocol-mixin adjency-graph-mixin))
