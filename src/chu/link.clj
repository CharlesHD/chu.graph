(ns chu.link)

(defrecord Link [from to])

(defn assoc-params
  "Associate parameters to a link"
  [l new-params]
  (vary-meta l assoc ::params new-params))

(defn make-link
  "Link constructor"
  ([from to params]
   (assoc-params (->Link from to) params))
  ([from to]
   (make-link from to {})))

(defn params
  "parameters of the link."
  [l]
  (::params (meta l)))

(defn update-params
  "Change a link params using `f`. New links params is (f `l` old-params)"
  [l f]
  (vary-meta l update ::params (partial f l)))

(defn lift
  "takes a function `f` of three args and make a function that take a link and call `(f from to params)`"
  [f]
  (fn [l] (f (:from l) (:to l) (params l))))

(defn unlift
  "takes a function operating on link and return a three args function (f from to params)"
  [f]
  (fn [from to params] (f (make-link from to params))))

(defn lift2
  "takes a function `f` of two args and make a function that take a link and call `(f from to)` "
  [f]
  (fn [{fr :from to :to}] (f fr to)))

(defn unlift2
  "takes a function operating on link and return a two args function (f from to)"
  [f]
  (fn [from to] (f (make-link from to {}))))

(defn flip-link
  "Gimme a link, here it is flipped."
  [{from :from to :to :as l}]
  (make-link to from (params l)))
