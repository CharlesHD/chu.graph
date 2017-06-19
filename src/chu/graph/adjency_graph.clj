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
  (:require [chu.graph
             :refer
             [add-link
              add-node
              adjency
              default-graph-protocol-mixin
              empty-graph
              GraphProtocol
              links
              nodes]]
            [chulper.core :refer [map-keys map-vals]]
            [clojure.core.async :as a]
            [clojure.set :as set]
            [chu.link :as l]
            [chulper.core :as h]))

;; adjency, reversed, map-node, filter-node
(defrecord AdjencyGraph [adj])

(def EMPTY (->AdjencyGraph {}))

(defn- adj-filter-node
  [g pred]
  (let [mpred (memoize pred)
        adj (adjency g)
        keep #(select-keys % (filter pred (keys %)))]
    (->>
     (keep)
     (h/map-vals keep)
     (->AdjencyGraph))))

(defn- adj-filter-link
  [g pred]
  (let [gmake (a/chan (a/buffer (count (links g))))
        total (count (links g))
        adj (adjency g)]
    (doseq [i (nodes g)]
      (a/go (doseq [[j params] (adj i)]
              (if (pred (l/make-link i j params))
                (a/>! gmake (l/make-link i j params))
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

   :empty-graph (constantly EMPTY)

   :filter-node adj-filter-node

   :filter-link adj-filter-link

   :add-node (fn [g n]
               (if (get-in g [:adj n])
                 g ;; node already exists, do nothing.
                 (assoc-in g [:adj n] {})))

   :prot-add-link (fn [g {fr :from to :to :as l}]
               (-> (add-node g fr)
                   (add-node to)
                   (update-in [:adj fr] assoc to (l/params l))))
   })

(extend AdjencyGraph GraphProtocol (merge default-graph-protocol-mixin adjency-graph-mixin))
