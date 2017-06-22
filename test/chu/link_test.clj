(ns chu.link-test
  (:require [chu.link :as sut]
            [chu.test :refer [defspec-test]]))

(def opts {:clojure.spec.test.check/opts {:num-tests 20 :max-size 50}})

(defspec-test make-link
  `sut/make-link
  opts)

(defspec-test update-params
  `sut/update-params
  opts)

(defspec-test lift
  `sut/lift
  opts)

(defspec-test lift2
  `sut/lift2
  opts)

(defspec-test unlift
  `sut/unlift
  opts)

(defspec-test lift2
  `sut/unlift2
  opts)

(defspec-test flip
  `sut/flip
  opts)

(defspec-test loosely-equal
  `sut/loosely-equal
  opts)

(defspec-test unparam
  `sut/unparam
  opts)

(defspec-test in-domain?
  `sut/in-domain?
  opts)
