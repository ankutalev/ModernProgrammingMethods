(ns nsufit.task3_tests
  (:require [clojure.test :refer :all]
            [nsufit.task31 :refer :all]
            [nsufit.task32 :refer :all]
            ))

(deftest block-test
  (testing "Test paralell filter with different n equal normal filter"
    (let [ test-range (range 1 2000)]
      (doall(map (fn [n] (is (= (reduce + (filter even? test-range)) (reduce + (my-parallel-filter n even? test-range))))) (range 1 100))))
    )
  )


(deftest lazy-test
  (testing "Test paralell-lazy-filter with different n equal normal filter on endless seq"
      (doall(map (fn [n] (is (= (reduce + (take 500 (filter even? naturals))) (reduce + (take 500 (my-parallel-filter n even? naturals)))))) (range 1 100))))
  )

(deftest lazy-test-fail
  (testing "Test paralell-filter fails on lazy sec"
     (is (thrown? StackOverflowError (my-parallel-filter 1 even? naturals))))
  )