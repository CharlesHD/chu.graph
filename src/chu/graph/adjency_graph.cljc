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
  (:require [chu.graph :as g]
            [chu.graph.protocol :as prot :refer [GraphProtocol]]
            [chu.link :as l]
            [chulper.core :as h]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [clojure.spec.gen.alpha :as gen]))

;; adjency, reversed, map-node, filter-node
(defrecord AdjencyGraph [adj])

(defn- correct-adjency
  [adj]
  (set/subset? (apply set/union (map (comp set keys)
                                     (vals adj)))
               (set (keys adj))))

(defn- adjency-generator
  []
  (gen/bind (s/gen (s/coll-of :chu.link/node :kind set? :max-count 10))
            (fn [nds]
              (if (empty? nds)
                (s/gen (s/and empty? map?))
                (gen/fmap
                 #(zipmap nds %)
                 (s/gen (s/coll-of (s/map-of nds :chu.link/params) :count (count nds))))))))
(s/def ::adj
  (s/with-gen
    (s/and (s/map-of :chu.link/node
                     (s/map-of :chu.link/node :chu.link/params))
           correct-adjency)
    adjency-generator))

(s/def ::adjency-graph
  (s/with-gen
    #(instance? AdjencyGraph %)
    #(gen/fmap ->AdjencyGraph (s/gen ::adj))))

(def EMPTY (->AdjencyGraph {}))

(defn- filter-node
  [g pred]
  (let [mpred (memoize pred)
        adj (g/adjency g)
        keep #(select-keys % (filter pred (keys %)))]
    (->> adj
         (keep)
         (h/map-vals keep)
         (->AdjencyGraph))))

#?(:clj
   (defn- filter-link
     [g pred]
     (let [gmake (a/chan (a/buffer (count (g/links g))))
           total (count (g/links g))
           adj (g/adjency g)]
       (doseq [i (g/nodes g)]
         (a/go (doseq [[j params] (adj i)]
                 (if (pred (l/make-link i j params))
                   (a/>! gmake (l/make-link i j params))
                   (a/>! gmake false)))))
       (loop [g (reduce g/add-node EMPTY (g/nodes g))
              treated 0]
         (if (= treated total)
           g
           (let [i (a/<!! gmake)
                 ng (if i (g/add-link g i) g)]
             (recur ng (inc treated))))))))

(defn- add-node
  [g n]
  (if (get-in g [:adj n])
    g ;; node already exists, do nothing.
    (assoc-in g [:adj n] {})))

(defn- add-link
  [g mg {fr :from to :to p :params}]
  (-> (g/add-node g fr)
      (g/add-node to)
      (#(if (get-in % [:adj fr to])
          (update-in % [:adj fr to] mg p)
          (assoc-in % [:adj fr to] p)))))

(extend-type AdjencyGraph
  GraphProtocol
  ;; specific
  (adjency [g] (:adj g))
  (empty-graph [g] EMPTY)
  (filter-node [g pred] (filter-node g pred))
  (filter-link [g pred] #?(:clj (filter-link g pred)
                           :cljs (prot/default-filter-link g pred)))
  (add-node [g node] (add-node g node))
  (add-link [g mg link] (add-link g mg link))
  ;; generic
  (nodes [g] (prot/default-nodes g))
  (links [g] (prot/default-links g))
  (reversed [g] (prot/default-reversed g))
  (ancestry [g] (prot/default-ancestry g))
  (in-degrees [g] (prot/default-in-degrees g))
  (out-degrees [g] (prot/default-out-degrees g))
  (degrees [g] (prot/default-degrees g))
  (map-node [g mg f] (prot/default-map-node g mg f))
  (map-link [g f] (prot/default-map-link g f))
  (add-graph [g mg g2] (prot/default-add-graph g mg g2))
  (intersection-graph [g mg g2] (prot/default-intersection-graph g mg g2)))
