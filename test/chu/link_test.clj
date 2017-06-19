(ns chu.link-test
  (:require [chu.link :as sut]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :as t]
            [clojure.test.check :as tc]))


(chu.test/defspec-test
  link
  (stest/enumerate-namespace 'chu.link)
  {:clojure.spec.test.check/opts {:num-tests 50}})
