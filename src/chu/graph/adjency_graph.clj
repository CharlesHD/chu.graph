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
            [chu.graph.protocol :refer [default-graph-protocol-mixin GraphProtocol]]
            [chu.link :as l]
            [chulper.core :as h]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [clojure.spec.gen.alpha :as gen]))

;; adjency, reversed, map-node, filter-node
(defrecord AdjencyGraph [adj])

(s/def ::adjency-graph
  (s/with-gen
    (s/and (s/map-of :chu.link/node
                     (s/map-of :chu.link/node :chu.link/params))
           #(set/subset? (set (keys %)) (apply set/union (map (comp set keys) (vals %)))))
    (fn []
      (gen/bind (s/gen (s/coll-of :chu.link/node :kind set? :max-count 20 :distinct true))
                (fn [nds]
                  (if (empty? nds)
                    (s/gen (s/and map? empty?))
                    (gen/fmap
                     #(zipmap nds %)
                     (s/gen (s/coll-of (s/map-of nds :chu.link/params) :max-count 20)))))))))

(def EMPTY (->AdjencyGraph {}))

(defn- filter-node
  [g pred]
  (let [mpred (memoize pred)
        adj (g/adjency g)
        keep #(select-keys % (filter pred (keys %)))]
    (->>
     (keep)
     (h/map-vals keep)
     (->AdjencyGraph))))

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
          (recur ng (inc treated)))))))

(def adjency-graph-mixin
  {:adjency (fn [g] (:adj g))

   :empty-graph (constantly EMPTY)

   :filter-node filter-node

   :filter-link filter-link

   :add-node (fn [g n]
               (if (get-in g [:adj n])
                 g ;; node already exists, do nothing.
                 (assoc-in g [:adj n] {})))

   :add-link (fn [g mg {fr :from to :to p :params}]
               (-> (g/add-node g fr)
                   (g/add-node to)
                   (#(if (get-in % [:adj fr to])
                       (update-in % [:adj fr to] mg p)
                       (assoc-in % [:adj fr to] p)))))
   })

(extend AdjencyGraph GraphProtocol (merge default-graph-protocol-mixin adjency-graph-mixin))
