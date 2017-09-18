(ns chu.test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check :as tc]))

;; just here to keep the tc dependency
(defn- dum [] (tc/quick-check))
(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   (when t/*load-tests*
     `(def
        ~(vary-meta
          name assoc
          :test `(fn []
                   (let [check-results# (stest/check ~sym-or-syms ~opts)
                         checks-passed?# (every? nil? (map :failure check-results#))]
                     (if checks-passed?#
                       (t/do-report {:type    :pass
                                     :message (str "Generative tests pass for "
                                                   (str/join ", " (map :sym check-results#)))})
                       (doseq [failed-check# (filter :failure check-results#)
                               :let [r# (stest/abbrev-result failed-check#)
                                     failure# (:failure r#)]]
                         (t/do-report
                          {:type     :fail
                           :message  (with-out-str (clojure.spec.alpha/explain-out failure#))
                           :expected (->> r# :spec rest (apply hash-map) :ret)
                           :actual   (if (instance? Throwable failure#)
                                       failure#
                                       (:clojure.spec.test.alpha/val failure#))})))
                     checks-passed?#)))
        (fn [] (t/test-var (var ~name)))))))
