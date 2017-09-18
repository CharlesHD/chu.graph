(ns chu.link
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::params (s/map-of any? any? :max-count 50))
(s/def ::node (s/and any? (complement nil?)))
(s/def ::from ::node)
(s/def ::to ::node)
(s/def ::link (s/keys :req-un [::from ::to ::params]))

(s/fdef make-link
        :args (s/cat :from ::from :to ::to :params (s/? ::params))
        :ret ::link
        :fn #(let [args (:args %)
                   ret (:ret %)]
               (and (= (:from ret) (:from args))
                    (= (:to ret) (:to args))
                    (= (:params ret) (get args :params {})))))
(defn make-link
  "Link constructor"
  ([from to params]
   {:from from :to to :params params})
  ([from to]
   (make-link from to {})))

(s/fdef update-params
        :args (s/cat :f (s/fspec :args (s/cat :l ::link)
                                 :ret ::params)
                     :l ::link)
        :ret ::link)
(defn update-params
  "Change a link params using `f`. New links params is (f `l`)"
  [f l]
  (assoc l :params (f l)))

(s/fdef lift
        :args (s/cat :f (s/fspec :args (s/cat :from ::from :to ::to :params ::params)))
        :ret (s/fspec :args (s/cat :l ::link)))
(defn lift
  "takes a function `f` of three args and make a function that take a link and call `(f from to params)`"
  [f]
  (fn [l] (f (:from l) (:to l) (:params l))))

(s/fdef unlift
        :args (s/cat :f (s/fspec :args (s/cat :l ::link)))
        :ret (s/fspec :args (s/cat :from ::from :to ::to :params ::params)))
(defn unlift
  "takes a function operating on link and return a three args function (f from to params)"
  [f]
  (fn [from to params] (f (make-link from to params))))

(s/fdef lift2
        :args (s/cat :f (s/fspec :args (s/cat :from ::from :to ::to)))
        :ret (s/fspec :args (s/cat :l ::link)))
(defn lift2
  "takes a function `f` of two args and make a function that take a link and call `(f from to)` "
  [f]
  (fn [{fr :from to :to}] (f fr to)))

(s/fdef unlift2
        :args (s/cat :f (s/fspec :args (s/cat :l ::link)))
        :ret (s/fspec :args (s/cat :from ::from :to ::to)))
(defn unlift2
  "takes a function operating on link and return a two args function (f from to)"
  [f]
  (fn [from to] (f (make-link from to {}))))

(s/fdef flip
        :args (s/cat :l ::link)
        :ret ::link
        :fn (s/and #(= (-> % :args :l :from) (-> % :ret :to))
                   #(= (-> % :args :l :to) (-> % :ret :from))
                   #(= (-> % :args :l :params) (-> % :ret :params))))
(defn flip
  "Gimme a link, here it is flipped."
  [{from :from to :to :as l}]
  (make-link to from (:params l)))

(s/fdef loosely-equal
        :args (s/+ ::link)
        :ret boolean?)
(defn loosely-equal
  "link looe equality. Two links are loosely equals if they are equals in :from and :to"
  [& lks]
  (and (apply = (map :from lks))
       (apply = (map :to lks))))

(s/fdef unparam
        :args (s/cat :l ::link)
        :ret ::link
        :fn (s/and #(loosely-equal (-> % :args :l) (:ret %))
                   #(= {} (-> % :ret :params))))
(defn unparam
  "same link with no params"
  [l]
  (assoc l :params {}))

(s/fdef in-domain?
        :args (s/cat :dom set? :l ::link)
        :ret boolean?)
(defn in-domain?
  "Tests if link `l` is in the domain `dom`."
  [dom l]
  (and (contains? dom (:from l))
       (contains? dom (:to l))))
