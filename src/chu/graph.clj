(ns chu.graph
  "Graph Protocol and API.
  Graph is a classic interface for complex linked data.
  It's composed of nodes (data) and links (relation between data)."
  (:require [chu.link :as l :refer [make-link]]
            [chu.graph.protocol :as prot]
            [chulper.core :refer [fixpoint map-vals]]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [chulper.core :as h]))

;; specs
(s/def ::node :chu.link/node)
(s/def ::nodes (s/coll-of ::node :distinct true :kind set?))
(s/def ::adj (s/fspec :args (s/cat :n ::node)
                      :ret (s/or :hit (s/map-of ::node :chu.link/params)
                                 :miss nil?)))
(s/def ::links (s/and (s/coll-of :chu.link/link :distinct true :kind set?)
                      #(or (empty? %)
                           (apply distinct? (map l/unparam %)))))
(s/def ::graph #(satisfies? prot/GraphProtocol %))
(s/def ::merge-params (s/fspec :args (s/+ :chu.link/params)
                               :ret :chu.link/params))

;; API Wrappers
(s/fdef nodes
        :args (s/cat :g ::graph)
        :ret ::nodes)
(defn nodes
  "Set of node of the graph"
  [g]
  (prot/nodes g))

(s/fdef links
        :args (s/cat :g ::graph)
        :ret ::links)
(defn links
  "List of links of the graph. A well formed graph has links wrapped in the Link record"
  [g]
  (prot/links g))

(s/fdef adjency
        :args (s/cat :g ::graph)
        :ret ::adj)
(defn adjency
  "The map where graph nodes are keys and vals are maps of nodes adjacent to the key node with links params as value.
   Exemple : a -> b(p1) ; a -> c(p2) ; b -> c(p3) => {a {b p1, c p2} b {c p3} c {}}"
  [g]
  (prot/adjency g))

(s/fdef ancestry
        :args (s/cat :g ::graph)
        :ret ::adj)
(defn ancestry
  "The map where graph nodes are keys and vals are of ancestors of the key node with link param as value.
   Exemple : a -> b(p1); a -> c(p2); b -> c(p3) => {a #{} b {a p1} c {a p2, b p3}}"
  [g]
  (prot/ancestry g))

(s/fdef reversed
        :args (s/cat :g ::graph)
        :ret ::graph
        :fn #(= (->> % :args :g links (map l/flip) set)
                (->> % :ret links)))
(defn reversed
  "The same graph but with every links reversed : a -> b => b -> a"
  [g]
  (prot/reversed g))

(declare map-link)
(s/fdef map-link
        :args (s/cat :f (s/fspec :args (s/cat :l :chu.link/link)
                                 :ret :chu.link/params)
                     :g ::graph)
        :ret ::graph
        :fn (s/and #(= (nodes (:ret %)) (nodes (:g (:args %))))
                   #(let [args (:args %)
                          mf (memoize (:f args))
                          g (:g args)
                          ret (map-link mf g)]
                      (= (links ret)
                         (set (map (partial l/update-params mf) (links g)))))))
(defn map-link
  "Transform links params of the graph using `f` : a -> b(p) => a ->b(f a->b(p)).
   `f` should take a link as parameter and return the new links param map."
  [f g]
  (prot/map-link g f))

(declare map-node)
(s/fdef map-node
        :args (s/cat :merge-params (s/? ::merge-params)
                     :f (s/fspec :args (s/cat :n ::node)
                                 :ret ::node)
                     :g ::graph)
        :ret ::graph
        :fn #(let [args (-> % :args)
                   f (:f args)
                   mf (memoize f)
                   g (:g args)
                   ret (map-node (or (:merge-params args) merge) mf g)]
               (and
                ;; nodes are the same modulo mapping
                (= (nodes ret)
                   (set (map mf (nodes g))))
                ;; links are still here modulo mapping
                (= (set (map l/unparam (links ret)))
                   (set (map (fn [{fr :from to :to}]
                               (l/make-link (mf fr) (mf to)))
                             (links g)))))))
(defn map-node
  "Transform nodes of the graph using `f` : a -> b => (f a) -> (f b).
   `merge-params` is used in case of transformation collision.
   Case where (f n1) = (f n2) = c is correctly handle :
   - n1 -> a(p1) ; n2 -> b(p2) => c -> a(p1) ; c -> b(p2)
   - a -> n1(p1) ; b -> n2(p2) => a -> c(p1) ; b -> c(p2)
   - a -> n1(p1) ; a -> n2(p2) => a -> c(merge-params p1 p2)
   - n1 -> a(p1) ; n2 -> a(p2) => c -> a(merge-params p1 p2)"
  ([merge-params f g] (prot/map-node g merge-params f))
  ([f g] (prot/map-node g merge f)))

(declare filter-node)
(s/fdef filter-node
        :args (s/cat :pred (s/fspec :args (s/cat :x ::node))
                     :g ::graph)
        :ret ::graph
        :fn #(let [args (:args %)
                   mpred (memoize (:pred args))
                   g (:g args)
                   ret (filter-node mpred g)]
               (and
                ;; equality of node set modulo filter
                (= (nodes ret)
                   (set (filter mpred (nodes g))))
                ;; equality of links
                (= (links ret)
                   (->> g links (filter (fn [{fr :from to :to}]
                                          (and (mpred fr) (mpred to))))
                        set)))))
(defn filter-node
  "Make a graph where you keep only nodes verifying pred."
  [pred g]
  (prot/filter-node g pred))

(declare filter-link)
(s/fdef filter-link
        :args (s/cat :pred (s/fspec :args (s/cat :x :chu.link/link))
                     :g ::graph)
        :ret ::graph
        :fn
        #(let [args (:args %)
               mpred (memoize (:pred args))
               g (:g args)
               ret (filter-link mpred g)]
           (and
            ;; equality of node set modulo filter
            (= (nodes ret)
               (nodes g))
            ;; equality of links
            (= (links ret)
               (->> g links (filter mpred) set)))))
(defn filter-link
  "Make a graph where you only keep links verifying pred."
  [pred g]
  (prot/filter-link g pred))

(s/fdef in-degrees
        :args (s/cat :g ::graph)
        :ret (s/map-of ::node number?))
(defn in-degrees
  "return a map where nodes are keys and val is the in-degree of the keynode."
  [g]
  (prot/in-degrees g))

(s/fdef out-degrees
        :args (s/cat :g ::graph)
        :ret (s/map-of ::node number?))
(defn out-degrees
  "return a map where nodes are keys and val is the out-degree of the keynode."
  [g]
  (prot/out-degrees g))

(s/fdef degrees
        :args (s/cat :g ::graph)
        :ret (s/map-of ::node number?)
        :fn #(= (:ret %) (merge-with +
                                     (in-degrees (-> % :args :g))
                                     (out-degrees (-> % :args :g)))))
(defn degrees
  "return a map where nodes are keys and val is the total degree of the keynode."
  [g]
  (prot/degrees g))

(s/fdef empty-graph
        :args (s/cat :g ::graph)
        :ret ::graph
        :fn (s/and #(empty? (nodes (:ret %)))
                   #(empty? (links (:ret %)))))
(defn empty-graph
  "The same graph but with no nodes, no links."
  [g]
  (prot/empty-graph g))

(s/fdef add-node
        :args (s/cat :g ::graph :n ::node)
        :ret ::graph
        :fn #(= (-> % :ret nodes)
                (-> % :args :g nodes (conj (-> % :args :n)))))
(defn add-node
  "Same graph with node n added."
  [g n]
  (prot/add-node g n))

(declare add-link)
(s/fdef add-link
        :args (s/cat :merge-params (s/? ::merge-params) :g ::graph :l :chu.link/link)
        :ret ::graph
        :fn #(let [args (:args %)
                   mg (memoize (get args :merge-params merge))
                   g (:g args)
                   l (:l args)
                   ret (add-link mg g l)]
               (and
                (= (set (map l/unparam (links ret)))
                   (->> (conj (links g) l)
                        (map l/unparam) set))
                (let [p (partial l/loosely-equal l)]
                  (if (some p (links g))
                    (let [l1 (first (filter p (links g)))
                          l3 (first (filter p (links ret)))]
                      (= (:params l3) (mg (:params l1) (:params l))))
                    true)))))
(defn add-link
  "Same graph with link l added. Add nodes involved in the link if not present.
   If the link is already present `merge-params` resolve params conflict."
  ([merge-params g l]
   (prot/add-link g merge-params l))
  ([g l]
   (prot/add-link g merge l)))

(declare add-graph)
(s/fdef add-graph
        :args (s/cat :merge-params (s/? ::merge-params) :g ::graph :g2 ::graph)
        :ret ::graph
        :fn #(let [args (:args %)
                   g (:g args)
                   g2 (:g2 args)
                   mg (memoize (get args :merge-params merge))
                   ret (add-graph mg g g2)]
               (and
                (= (nodes ret)
                   (set/union (nodes g) (nodes g2)))
                (every?
                 (fn [l]
                   (let [get-p (fn [g]
                                 (->> g links
                                      (filter (partial l/loosely-equal l))
                                      first :params))]
                     (= (:params l)
                        (if (and (get-p g) (get-p g2))
                          (mg (get-p g) (get-p g2))
                          (or (get-p g)
                              (get-p g2))))))
                 (links ret)))))
(defn add-graph
  "Add the graph g2 to the graph g. It's the union of nodes and links of both graph.
   In case links are present in both graphs `merge-params` resolve params conflict."
  ([merge-params g g2]
   (prot/add-graph g merge-params g2))
  ([g g2]
   (prot/add-graph g merge g2)))

(s/fdef intersection-graph
        :args (s/or :complete (s/cat :merge-params ::merge-params :g ::graph :g2 ::graph)
                    :quick (s/cat :g ::graph :g2 ::graph))
        :ret ::graph
        :fn #(if-let [args (-> % :args :complete)]
               (let [g (:g args)
                     g2 (:g2 args)
                     ret (:ret %)]
                 (and
                  (= (nodes ret)
                     (set/intersection (nodes g) (nodes g2)))
                  (every?
                   (fn [l]
                     (let [get-p (fn [g]
                                   (->> g links
                                        (filter (partial l/loosely-equal l))
                                        first :params))]
                       (and (get-p g) (get-p g2)
                            ((:merge-params args) (get-p g) (get-p g2)))))
                   (links ret))))
               true))
(defn intersection-graph
  "The graph containing only nodes and links presents in both graphs.
   `merge-params` resolve params conflict."
  ([merge-params g g2]
   (prot/intersection-graph g merge-params g2))
  ([g g2]
   (prot/intersection-graph g merge g2)))

;; API
(s/fdef reduce-graph
        :args (s/cat :nf (s/fspec :args (s/cat :m any? :n ::node)
                                  :ret any?)
                     :lf (s/fspec :args (s/cat :m any? :l :chu.link/link))
                     :init any?
                     :g ::graph)
        :ret any?)
(defn reduce-graph
  "Reduce through a graph.
  First reduce through nodes using `nf`.
  Then reduce through links using `lf`"
  [nf lf init g]
  (prot/reduce-graph nf lf init g))

(s/fdef make-graph
        :args (s/cat :g ::graph
                     :mg (s/? ::merge-params)
                     :ns (s/coll-of ::node)
                     :lks (s/coll-of :chu.link/link))
        :ret ::graph)
(defn make-graph
  "Construct a new graph like g but with ns as nodes and lks as links."
  ([g mg ns lks]
   (prot/make-graph g mg ns lks))
  ([g ns lks]
   (prot/make-graph g merge ns lks)))

(s/fdef remove-node
        :args (s/cat :pred (s/fspec :args (s/cat :x ::node))
                     :g ::graph)
        :ret ::graph)
(defn remove-node
  "Make a graph without nodes verifying pred."
  [pred g]
  (filter-node (complement pred) g))

(s/fdef remove-link
        :args (s/cat :pred (s/fspec :args (s/cat :x :chu.link/link))
                     :g ::graph)
        :ret ::graph)
(defn remove-link
  "Make a graph without links verifying pred."
  [pred g]
  (filter-link (complement pred) g))

(s/fdef undirect
        :args (s/cat :g ::graph)
        :ret ::graph)
(defn undirect
  "Add missing return link to `g` : If a->b(p) is a link then b->a(p) will be too."
  [g]
  (add-graph g (reversed g)))

(s/fdef line-graph
        :args (s/cat :g ::graph)
        :ret ::graph)
(defn line-graph
  "Line-graph of g. Every link of g is a node in the line-graph. Two line-graph nodes are linked
   If their corresponding links share a node in g.

  Exemple : a -> b; b -> c => (a, b) -> (b, c)"
  [g]
  (let [line-nodes (links g)
        line-lks (for [{f1 :from t1 :to :as l1} line-nodes
                       {f2 :from t2 :to :as l2} line-nodes
                       :when (and (not= l1 l2) (= t1 f2))]
                   (make-link l1 l2))]
    (make-graph g line-nodes line-lks)))

(s/fdef seed-graph
        :args (s/cat :g ::graph :seeds ::nodes :node-limit number?)
        :ret ::graph)
(defn seed-graph
  "construct a subgraph of g by growing around the seeds until there is node-limit nodes."
  [g seeds node-limit]
  (let [n (nodes g)
        adj (comp keys (adjency g))
        grow (fn [s] (apply set/union s (map adj s)))
        stop #(or (>= (count %) node-limit) (= (count %) (count n)))
        nodes? (->> (fixpoint grow seeds stop)
                    (take node-limit)
                    set)]
    (filter-node nodes? g)))

(s/fdef remove-unlinked-nodes
        :args (s/cat :g ::graph)
        :ret ::graph)
(defn remove-unlinked-nodes
  "go away you unlinked node !"
  [g]
  (let [deg (degrees g)]
    (remove-node #(zero? (deg %)) g)))

(defn- dfs-
  "Depth first search. Short form of the method passes through all the
  nodes of the graph even if it's disconnected .
  (nodes-fn graph) expected to return list of all the nodes in the graph.
  (child-fn graph node) expected to return list of all the nodes linked
   to the given node.
  Returns hash-map where nodes are associated with a pair :idx, :leader.
  :idx stores finishing index of the node traversal (post-order counter)
  :leader first finishing index of the current DFS."

  ([graph nodes-fn child-fn]
   (second
    (reduce ;; Start DFS from each node of the graph
     (fn [[idx result passed :as args] next-node]
       (if (not (passed next-node)) ;; Don't do DFS if node is marked
         (dfs- idx idx result passed graph next-node child-fn)
         args))
     [0 {} #{}] ;;Initial index, result, set of passed nodes
     (nodes-fn graph))))
  ([idx leader result passed graph node child-fn]
   (let [[idx result passed]
         (reduce (fn [[idx result passed :as args] child-node]
                   (if (not (passed child-node))
                     (dfs- idx leader result passed graph child-node child-fn)
                     args))
                 [idx result (conj passed node)]
                 (child-fn graph node))]
     [(inc idx)
      (assoc result node {:idx idx :leader leader})
      passed])))

(s/fdef dfs
        :args (s/cat :g ::graph)
        :ret (s/map-of ::node (s/map-of keyword? int?)))
(defn dfs
  "Depth first search. Short form of the method passes through all the
  nodes of the graph even if it's disconnected .
  Returns hash-map where nodes are associated with a pair :idx, :leader.
  :idx stores finishing index of the node traversal (post-order counter)
  :leader first finishing index of the current DFS."
  [graph]
  (let [adj (adjency graph)]
    (dfs- graph nodes (fn [_ n] (adj n)))))

(defn- pass-two
  "Calls DFS making sure that traversal is done in the reverse :idx order."
  [graph result child-fn]
  (let [nodes-fn
        (constantly (->> result
                         ;;Sort by :idx in reverse order
                         (sort-by (comp :idx second)) reverse
                         ;;Return only nodes
                         (map first)))]
    (dfs- graph nodes-fn child-fn)))

(defn- scc-
  "Finds strongly connected components of the given directed graph.
  Returns lists of nodes grouped into SCC.
  (nodes-fn graph) expected to return list of all the nodes in the graph.
  (incoming-fn graph node) expected to return all the nodes with
   transitions towards the given node.
  (outgoing-fn graph node) expected to return all the nodes with
   transitions from the given node."
  ([graph nodes-fn incoming-fn outgoing-fn]
   (let [result (dfs- graph nodes-fn incoming-fn)
         leaders-idx (pass-two graph result outgoing-fn)]
     (for [scc-group (vals (group-by (comp :leader second) leaders-idx))]
       (for [[node & _] scc-group] node)))))

(s/fdef scc
        :args (s/cat :graph ::graph)
        :ret (s/coll-of (s/coll-of ::node)))
(defn scc
  "Finds strongly connected components of the given directed graph.
  Returns lists of nodes grouped into SCC."
  [graph]
  (let [adj-map (adjency graph)
        adj (fn [g n] (adj-map n))
        radj-map (ancestry graph)
        radj (fn [g n] (radj-map n))]
    (scc- graph nodes radj adj)))

(defn- remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra-
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) ((partial map-vals (partial + d))))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(s/fdef dijkstra
        :args (s/cat :start ::node
                     :g ::graph
                     :f (s/? (s/fspec :args (s/cat :x :chu.link/params)
                                      :ret number?)))
        :ret (s/map-of ::node number?))
(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.
  Given a link a->b(p), (f p) should return the distance (non-negative) from a
  to b.
  In short form (without `f`) each link distance is equal to 1.
  Returns a map from nodes to their distance from start."
  ([start g f]
   (let [adj (adjency g)
         distadj (fn [n] (map-vals f (adj n)))]
     (dijkstra- start distadj)))
  ([start g]
   (dijkstra start g (constantly 1))))

(s/fdef components
        :args (s/cat :g ::graph)
        :ret (s/coll-of ::graph))
(defn components
  "Return strongly connected components of `g` as graph whereas scc returns them as list of nodes"
  [g]
  (map #(filter-node (set %) g) (scc g)))

(s/fdef weak-components
        :args (s/cat :g ::graph)
        :ret (s/coll-of ::graph))
(defn weak-components
  "Return weakly connected components of `g` as graph."
  [g]
  (map #(filter-node (set %) g) (scc (undirect g))))

(defn- f-score
  [beta p r]
  (if (= 0 p r)
    0
    (let [b2 (* beta beta)]
      (/ (* (inc b2) p r)
         (+ (* b2 p) r)))))

(s/fdef graph-coverage
        :args (s/cat :g1 ::graph :g2 ::graph)
        :ret map?)
(defn graph-coverage
  "Compute the link coverage of g2 by g1."
  [g1 g2]
  (let [lks1 (links g1)
        lks2 (links g2)
        corr (set/intersection lks1 lks2)
        nc (count corr)
        r (/ nc (max (count lks2) 1))
        p (/ nc (max (count lks1) 1))
        f #(f-score % p r)]
    {:recall r :precision p :f-score (f 1)}))

(s/fdef kronecker
        :args (s/cat :g1 ::graph :g2 ::graph)
        :ret ::graph)
(defn kronecker [g1 g2]
  (let [pcart (fn [coll1 coll2] (for [x coll1 y coll2] [x y]))
        a1 (adjency g1)
        a2 (adjency g2)
        nodes (pcart (nodes g1) (nodes g2))]
    (make-graph g1 nodes (for [[x y] nodes
                               x' (a1 x)
                               y' (a2 y)]
                           (make-link [x y] [x' y'])))))

(s/fdef iterate-kronecker
        :args (s/cat :g ::graph :n (s/and pos-int? #(< % 4)))
        :ret ::graph)
(defn iterate-kronecker [g n]
  (reduce kronecker (repeat n g)))

(s/fdef chain-graph
        :args (s/cat :g ::graph :coll (s/coll-of ::node))
        :ret ::graph)
(defn chain-graph
  "Transform a sequence into a chain graph where each next element is link by the previous one"
  [g coll]
  (cond
    (empty? coll) g
    (= 1 (count coll)) (add-node g (first coll))
    :else (recur (add-link g
                           (make-link (first coll) (second coll)))
                 (rest coll))))
