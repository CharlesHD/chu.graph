(ns chu.link
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(s/def ::params map?)
(s/def ::meta-params
  (s/with-gen
    #(s/valid? (s/keys :req [::params]) (meta %))
    (fn [] (gen/bind (gen/map (gen/any) (gen/any))
                     (fn [v] (gen/fmap #(vary-meta % assoc ::params v) (gen/map (gen/any) (gen/any)))) ))))
(s/def ::node (s/and any? (complement nil?)))
(s/def ::from ::node)
(s/def ::to ::node)
(s/def ::proto-link (s/keys :req-un [::from ::to]))
(s/def ::link
  (s/with-gen
    (s/and ::proto-link
           ::meta-params)
    (fn [] (gen/bind (s/gen map?)
                     (fn [v] (gen/fmap
                              #(vary-meta % assoc ::params v)
                              (s/gen (s/keys :req-un [::from ::to]))))))))

(defrecord Link [from to])


(s/fdef assoc-params
        :args (s/cat :l ::proto-link :new-params ::params)
        :ret ::link
        :fn (s/and #(let [x (-> % :ret meta ::params)
                          y (-> % :args :new-params)]
                        (= x y))
                   #(= (:ret %) (-> % :args :l))))
(defn assoc-params
  "Associate parameters to a link"
  [l new-params]
  (vary-meta l assoc ::params new-params))

(s/fdef make-link
        :args (s/or :complete (s/cat :from ::from :to ::to :params ::params)
                    :quick (s/cat :from ::from :to ::to))
        :ret ::link
        :fn #(if (= :complete (first (:args %)))
               (and (= (-> % :ret :from) (-> % :args second :from))
                    (= (-> % :ret :to) (-> % :args second :to))
                    (= (-> % :ret meta ::params) (-> % :args second :params)))
               true))
(defn make-link
  "Link constructor"
  ([from to params]
   (assoc-params (->Link from to) params))
  ([from to]
   (make-link from to {})))

(s/fdef params
        :args (s/cat :l ::link)
        :ret ::params)
(defn params
  "parameters of the link."
  [l]
  (::params (meta l)))

(s/fdef update-params
        :args (s/cat :l ::link
                     :f (s/fspec :args (s/cat :l ::link :old-params ::params)
                                 :ret ::params))
        :ret ::link)
(defn update-params
  "Change a link params using `f`. New links params is (f `l` old-params)"
  [l f]
  (vary-meta l update ::params (partial f l)))

(s/fdef lift
        :args (s/cat :f (s/fspec :args (s/cat :from ::from :to ::to :params ::params)))
        :ret (s/fspec :args (s/cat :l ::link)))
(defn lift
  "takes a function `f` of three args and make a function that take a link and call `(f from to params)`"
  [f]
  (fn [l] (f (:from l) (:to l) (params l))))

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

(s/fdef flip-link
        :args (s/cat :l ::link)
        :ret ::link
        :fn (s/and #(= (-> % :args :l :from) (-> % :ret :to))
                   #(= (-> % :args :l :to) (-> % :ret :from))
                   #(= (-> % :args :l params) (-> % :ret params))))
(defn flip-link
  "Gimme a link, here it is flipped."
  [{from :from to :to :as l}]
  (make-link to from (params l)))

(defn flat-link
  "flatten a link and it's metadata"
  [l]
  (assoc l :p (params l)))
