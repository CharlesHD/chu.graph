(ns chu.graph.id-wrapper
  "An ID wrapper for any graph implementation.
   It keeps a mapping between graph nodes and integers and
   transform the graph structure in an integer graph"
  (:require [chu.graph :as g]
            [chu.graph.protocol :as prot :refer [GraphProtocol GraphWrapperProtocol]]
            [chu.link :as l]))

(defrecord IdWrapperGraph [bij g])
(defrecord Bijection [forward backward])
(defn- bij-assoc
  [bij a b]
  (-> bij
      (update :forward assoc a b)
      (update :backward assoc b a)))
(defn- bij-forward-dissoc
  [bij a]
  (let [b ((:forward bij) a)]
    (if b
      (-> bij
          (update :forward dissoc a)
          (update :backward dissoc b))
      bij)))
(defn- bij-backward-dissoc
  [bij b]
  (let [a ((:backward bij) b)]
    (if a
      (-> bij
          (update :forward dissoc a)
          (update :backward dissoc b))
      bij)))
(defn- bij-add
  [bij a]
  (if ((:forward bij) a)
    bij
    (let [m (reduce max -1 (keys (:backward bij)))]
      (bij-assoc bij a (inc m)))))


(defn nodes
  [{:keys [bij g]}]
  (set (map (:backward bij) (g/nodes g))))
(defn links
  [{:keys [bij g]}]
  (->> (g/links g)
       (map #(l/make-link ((:backward bij) (:from %))
                          ((:backward bij) (:to %))
                          (:params %)))
       set))
(defn adjency
  [{:keys [bij g] :as idg}]
  (g/adjency (g/map-node (:backward bij) g)))
(defn ancestry
  [{:keys [bij g]}]
  (g/ancestry (g/map-node (:backward bij) g)))
(defn reversed
  [{:keys [bij g]}]
  (->IdWrapperGraph bij (g/reversed g)))
(defn map-link
  [{:keys [bij g]} f]
  (let [bc (:backward bij)
        nf (fn [{:keys [from to params]}]
             (f (l/make-link (bc from) (bc to) params)))]
    (->IdWrapperGraph bij (g/map-link nf g))))
(defn map-node
  [{:keys [bij g] :as r} merge-params f]
  (let [bij2 (->Bijection {} {})
        bij2 (reduce bij-add bij2 (distinct (map f (nodes r))))]
    (->IdWrapperGraph
     bij2
     (g/map-node merge-params (comp (:forward bij2) f (:backward bij)) g))))
(defn filter-node
  [{:keys [bij g]} f]
  (let [cf (comp f (:backward bij))
        ng (g/filter-node cf g)
        nbij (reduce bij-forward-dissoc bij (remove f (keys (:forward bij))))]
    (->IdWrapperGraph nbij ng)))
(defn filter-link
  [{:keys [bij g]} f]
  (let [cf (comp f (fn [{:keys [from to params]}] (l/make-link ((:backward bij) from)
                                                               ((:backward bij) to)
                                                               params)))]
    (->IdWrapperGraph bij (g/filter-link cf g))))
(defn in-degrees
  [{:keys [bij g]}]
  (chulper.core/map-keys (:backward bij)
                         (g/in-degrees g)))
(defn out-degrees
  [{:keys [bij g]}]
  (chulper.core/map-keys (:backward bij)
                         (g/out-degrees g)))
(defn degrees
  [{:keys [bij g]}]
  (chulper.core/map-keys (:backward bij)
                         (g/degrees g)))
(defn empty-graph
  [_]
  (->IdWrapperGraph (->Bijection {} {}) chu.graph.adjency-graph/EMPTY))
(defn add-node
  [{:keys [bij g]} n]
  (let [nbij (bij-add bij n)
        ng (g/add-node g ((:forward nbij) n))]
    (->IdWrapperGraph nbij ng)))
(defn add-link
  [r merge-params {:keys [from to params]}]
  (let [{:keys [bij g]} (-> r (add-node from) (add-node to))]
    (->IdWrapperGraph bij
                      (g/add-link merge-params g
                                  (l/make-link ((:forward bij) from)
                                               ((:forward bij) to)
                                               params)))))

(defn wrap
  [g]
  (g/add-graph
   (->IdWrapperGraph
    (->Bijection {} {}) (g/empty-graph g))
   g))

(defn unwrap
  [{:keys [bij g]}]
  (g/map-node (:backward bij) g))

(def wrapper-mixin
  {:wrap (fn [_ g] (wrap g))
   :unwrap unwrap
   :inner :g})

(extend IdWrapperGraph GraphWrapperProtocol wrapper-mixin)

(def id-wrapper-graph-mixin
  {:nodes nodes
   :links links
   :adjency adjency
   :ancestry ancestry
   :reversed reversed
   :map-link map-link
   :map-node map-node
   :filter-node filter-node
   :filter-link filter-link
   :in-degrees in-degrees
   :out-degrees out-degrees
   :degrees degrees
   :empty-graph empty-graph
   :add-node add-node
   :add-link add-link})



(extend IdWrapperGraph GraphProtocol (merge prot/default-graph-protocol-mixin id-wrapper-graph-mixin))
