(ns chu.graph
  "Graph Protocol and API.
  Graph is a classic interface for complex linked data.
  It's composed of nodes (data) and links (relation between data)."
  (:require [chu.link :as l :refer [flip-link make-link]]
            [chu.graph.protocol :as prot]
            [chulper.core :refer [fixpoint map-vals]]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; API Wrappers
(defn nodes
  "Set of node of the graph"
  [g]
  (prot/nodes g))

(defn links
  "List of links of the graph. A well formed graph has links wrapped in the Link record"
  [g]
  (prot/links g))

(defn adjency
  "The map where graph nodes are keys and vals are maps of nodes adjacent to the key node with links params as value.
   Exemple : a -> b(p1) ; a -> c(p2) ; b -> c(p3) => {a {b p1, c p2} b {c p3} c {}}"
  [g]
  (prot/adjency g))

(defn ancestry
  "The map where graph nodes are keys and vals are of ancestors of the key node with link param as value.
   Exemple : a -> b(p1); a -> c(p2); b -> c(p3) => {a #{} b {a p1} c {a p2, b p3}}"
  [g]
  (prot/ancestry g))

(defn reversed
  "The same graph but with every links reversed : a -> b => b -> a"
  [g]
  (prot/reversed g))

(defn map-link
  "Transform links params of the graph using `f` : a -> b(p) => a ->b(f a b p).
   `f` should take a link as parameter and return the new links param map."
  [f g]
  (prot/map-link g f))

(defn map-node
  "Transform nodes of the graph using `f` : a -> b => (f a) -> (f b).
   `merge-params` is used in case of transformation collision.
   Case where (f n1) = (f n2) = c is correctly handle :
   - n1 -> a(p1) ; n2 -> b(p2) => c -> a(p1) ; c -> b(p2)
   - a -> n1(p1) ; b -> n2(p2) => a -> c(p1) ; b -> c(p2)
   - a -> n1(p1) ; a -> n2(p2) => a -> c(merge-params p1 p2)
   - n1 -> a(p1) ; n2 -> a(p2) => c -> a(merge-params p1 p2)"
  ([merge-params f g] (prot/map-node g merge-params f))
  ([f g] (map-node merge f g)))

(defn filter-node
  "Make a graph where you keep only nodes verifying pred."
  [pred g]
  (prot/filter-node pred g))

(defn filter-link
  "Make a graph where you only keep links verifying pred."
  [pred g]
  (prot/filter-link g pred))

(defn in-degrees
  "return a map where nodes are keys and val is the in-degree of the keynode."
  [g]
  (prot/in-degrees g))

(defn out-degrees
  "return a map where nodes are keys and val is the out-degree of the keynode."
  [g]
  (prot/out-degrees g))

(defn degrees
  "return a map where nodes are keys and val is the total degree of the keynode."
  [g]
  (prot/degrees g))

(defn empty-graph
  "The same graph but with no nodes, no links."
  [g]
  (prot/empty-graph g))

(defn add-node
  "Same graph with node n added."
  [g n]
  (prot/add-node g n))

(defn add-link
  "Same graph with link l added. Add nodes involved in the link if not present.
   If the link is already present `merge-params` resolve params conflict."
  ([merge-params g l]
   (prot/add-link g merge-params l))
  ([g l]
   (prot/add-link g merge l)))

(defn add-graph
  "Add the graph g2 to the graph g. It's the union of nodes and links of both graph.
   In case links are present in both graphs `merge-params` resolve params conflict."
  ([merge-params g g2]
   (prot/add-graph g merge-params g2))
  ([g g2]
   (prot/add-graph g merge g2)))

(defn intersection-graph
  "The graph containing only nodes and links presents in both graphs.
   `merge-params` resolve params conflict."
  ([merge-params g g2]
   (prot/intersection-graph g merge-params g2))
  ([g g2]
   (prot/intersection-graph g merge g2)))

;; API
(defn reduce-graph
  "Reduce through a graph.
  First reduce through nodes using `nf`.
  Then reduce through links using `lf`"
  [nf lf init g]
  (prot/reduce-graph nf lf init g))

(defn make-graph
  "Construct a new graph like g but with ns as nodes and lks as links."
  ([g mg ns lks]
   (reduce (partial add-link mg)
           (reduce add-node (empty-graph g) ns)
           lks))
  ([g ns lks]
   (make-graph g merge ns lks)))

(defn map-links
  "Change links params of `g` using `f`. See chu.link/update-params for more info.
  f as the signature (f link old-link-params) and returns new link params"
  [g f]
  (make-graph g merge (nodes g) (map #(l/update-params % f) (links g))))

(defn remove-node
  "Make a graph without the nodes verifying pred."
  [g pred]
  (filter-node g (complement pred)))

(defn remove-link
  "Make a graph without the links verifying pred."
  [g pred]
  (filter-link g (complement pred)))

(defn undirect
  "Add missing return link to `g` : If a->b is a link then b->a will be too."
  [g]
  (add-graph g (reversed g) merge))

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

(defn seed-graph
  "construct a subgraph of g by growing around the seeds until there is node-limit nodes."
  [g seeds node-limit]
  (let [n (nodes g)
        adj (adjency g)
        grow (fn [s] (distinct (reduce concat s (map adj s))))
        stop #(or (>= (count %) node-limit) (= (count %) (count n)))
        nodes? (->> (fixpoint grow seeds stop)
                    (take node-limit)
                    set)]
    (filter-node g nodes?)))

(defn remove-unlinked-nodes
  "go away you unlinked node !"
  [g]
  (let [deg (degrees g)]
    (remove-node g #(zero? (deg %)))))

(defn dfs
  "Depth first search. Short form of the method passes through all the
  nodes of the graph even if it's disconnected .
  (nodes-fn graph) expected to return list of all the nodes in the graph.
  (child-fn graph node) expected to return list of all the nodes linked
   to the given node.
  Returns hash-map where nodes are associated with a pair :idx, :leader.
  :idx stores finishing index of the node traversal (post-order counter)
  :leader first finishing index of the current DFS."
  ([graph]
   (let [adj (adjency graph)]
     (dfs graph nodes (fn [_ n] (adj n)))))
  ([graph nodes-fn child-fn]
   (second
    (reduce ;; Start DFS from each node of the graph
     (fn [[idx result passed :as args] next-node]
       (if (not (passed next-node)) ;; Don't do DFS if node is marked
         (dfs idx idx result passed graph next-node child-fn)
         args))
     [0 {} #{}] ;;Initial index, result, set of passed nodes
     (nodes-fn graph))))
  ([idx leader result passed graph node child-fn]
   (let [[idx result passed]
         (reduce (fn [[idx result passed :as args] child-node]
                   (if (not (passed child-node))
                     (dfs idx leader result passed graph child-node child-fn)
                     args))
                 [idx result (conj passed node)]
                 (child-fn graph node))]
     [(inc idx)
      (assoc result node {:idx idx :leader leader})
      passed])))

(defn- pass-two
  "Calls DFS making sure that traversal is done in the reverse :idx order."
  [graph result child-fn]
  (let [nodes-fn
        (constantly (->> result
                         ;;Sort by :idx in reverse order
                         (sort-by (comp :idx second)) reverse
                         ;;Return only nodes
                         (map first)))]
    (dfs graph nodes-fn child-fn)))

(defn scc
  "Finds strongly connected components of the given directed graph.
  Returns lists of nodes grouped into SCC.
  (nodes-fn graph) expected to return list of all the nodes in the graph.
  (incoming-fn graph node) expected to return all the nodes with
   transitions towards the given node.
  (outgoing-fn graph node) expected to return all the nodes with
   transitions from the given node."
  ([graph nodes-fn incoming-fn outgoing-fn]
   (let [result (dfs graph nodes-fn incoming-fn)
         leaders-idx (pass-two graph result outgoing-fn)]
     (for [scc-group (vals (group-by (comp :leader second) leaders-idx))]
       (for [[node & _] scc-group] node))))
  ([graph]
   (let [adj-map (adjency graph)
         adj (fn [g n] (adj-map n))
         radj-map (ancestry graph)
         radj (fn [g n] (radj-map n))]
     (scc graph nodes adj radj))))

(defn- remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
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

(defn g-dijkstra
  "like dijkstra but works on unweighted graph"
  [start g]
  (let [adj (adjency g)]
    (dijkstra start
              (fn [n] (zipmap (adj n) (repeat 1))))))

;; (defn mst
;;   "calculate a minimum spanning tree for graph. Weight is a function from graph link to number"
;;   [graph weight]
;;   (let [lks (links graph)
;;         nds (nodes graph)
;;         lks (sort-by weight < lks)
;;         parent (apply uf/union-find nds)
;;         tree (loop [[l & ls] lks
;;                     uf parent
;;                     tree '()]
;;                (if (nil? l) tree
;;                    (let [from (:from l)
;;                          to (:to l)
;;                          [uf0 a] (uf/get-canonical uf from)
;;                          [uf1 b] (uf/get-canonical uf0 to)]
;;                      (if (= a b)
;;                        (recur ls uf1 tree)
;;                        (recur ls (uf/union uf1 from to) (conj tree l))))))]
;;     (make-graph nds tree)))

;; (defn count-DAG-pathcover-numbre
;;   "Given a DAG count its pathcover number. Expects (links dag) to be ordered as
;;   the dag require."
;;   [dag]
;;   (let [lks (links dag)]
;;     (reduce
;;      (fn [m {fr :from to :to}]
;;        (cond-> m
;;          (not (get m fr)) (assoc fr {:fathers [fr] :source :yes :count 0})
;;          (not (get m to)) (assoc to {:fathers [fr] :count 0})
;;          )))))

(defn components
  "Return strongly connected components of `g` as graph whereas scc returns them as list of nodes"
  [g]
  (map #(filter-node g (set %)) (scc g)))

(defn weak-components
  "Return weakly connected components of `g` as graph."
  [g]
  (map #(filter-node g (set %)) (scc (undirect g))))

(defn- f-score
  [beta p r]
  (if (= 0 p r)
    0
    (let [b2 (* beta beta)]
      (/ (* (inc b2) p r)
         (+ (* b2 p) r)))))

(defn graph-coverage
  "Compute the link coverage of g2 by g1."
  [g1 g2]
  (let [lks1 (set (links g1))
        lks2 (set (links g2))
        corr (set/intersection lks1 lks2)
        nc (count corr)
        r (/ nc (max (count lks2) 1))
        p (/ nc (max (count lks1) 1))
        f #(f-score % p r)]
    {:recall r :precision p :f-score (f 1)}))

(defn kronecker [g1 g2]
  (let [pcart (fn [coll1 coll2] (for [x coll1 y coll2] [x y]))
        a1 (adjency g1)
        a2 (adjency g2)
        nodes (pcart (nodes g1) (nodes g2))]
    (make-graph g1 nodes (for [[x y] nodes
                               x' (a1 x)
                               y' (a2 y)]
                           (make-link [x y] [x' y'])))))

(defn iterate-kronecker [g n]
  (reduce kronecker (repeat n g)))

(defn chain-graph
  "Transform a sequence into a chain graph where each next element is link by the previous one"
  [g coll]
  (cond
    (empty? coll) g
    (= 1 (count coll)) (add-node g (first coll))
    :else (recur (add-link g
                           (make-link (first coll) (second coll))
                           merge)
                 (rest coll))))
