(ns missing.locks-test
  (:require [missing.locks :as locks]
            [clojure.test :refer :all])
  (:import (java.util UUID)))


(deftest on-value-test
  (testing "When I lock on a value, another thread will wait until I'm done."
    (let [thing-to-mutate        (atom #{})
          bad-mutation-scheduled (atom false)
          value-to-lock-on       (str (UUID/randomUUID))
          different-value        (str (UUID/randomUUID))]
      (locks/locking value-to-lock-on
        (future
          (swap! bad-mutation-scheduled not)
          (locks/locking value-to-lock-on (swap! thing-to-mutate conj :bad)))
        (future
          (locks/locking different-value (swap! thing-to-mutate conj :okay)))
        (Thread/sleep 1000)
        (is @bad-mutation-scheduled)
        (is (contains? @thing-to-mutate :okay))
        (is (false? (contains? @thing-to-mutate :bad)))
        (swap! thing-to-mutate conj :good)
        (is (contains? @thing-to-mutate :good)))
      (Thread/sleep 1000)
      (is (contains? @thing-to-mutate :good))
      (is (contains? @thing-to-mutate :okay))
      (is (contains? @thing-to-mutate :bad)))))
