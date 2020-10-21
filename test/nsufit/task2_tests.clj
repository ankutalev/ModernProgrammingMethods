(ns nsufit.task2_tests
  (:require [clojure.test :refer :all]
            [nsufit.task21 :refer :all]
            [nsufit.task22 :refer :all]
            ))
(run-tests 'nsufit.task2_tests)

; for h =0.01
(def epsilon 0.02)

(deftest linear-test
  (testing "Integral x  is x^2 / 2"
    (let [val (get-integral identity), sq (fn [x] (/ (* x x) 2))]
      (doall (map (fn [x] (is (< (Math/abs(double(- (val x) (sq x)))) epsilon))) (range 0 20))))
    )
  )

(deftest sin-test
  (testing "Integral sin is -cos"
    (let [val (get-integral (fn [x] (Math/sin x))) ]
      (doall (map (fn [x] (is (< (Math/abs(double(- (val x) (- 1 (Math/cos x))))) epsilon))) (range 0 20))))
    )
  )

(deftest exp-test
  (testing "Integral e^x is e^x"
    (let [pow (fn [x] (Math/exp x)), val (get-integral pow)]
      (doall (map (fn [x] (is (< (Math/abs(double(- (val x)  (dec(pow x))))) epsilon))) (range 0 4))))
    )
  )

(deftest linear-test-partial
  (testing "Integral x  is x^2 / 2"
    (let [val (get-integral-partial identity), sq (fn [x] (/ (* x x) 2))]
      (doall (map (fn [x] (is (< (Math/abs(double(- (val x) (sq x)))) epsilon))) (range 0 20))))
    )
  )

(deftest sin-test-partial
  (testing "Integral sin is -cos"
    (let [val (get-integral-partial (fn [x] (Math/sin x))) ]
      (doall (map (fn [x] (is (< (Math/abs(double(- (val x) (- 1 (Math/cos x))))) epsilon))) (range 0 20))))
    )
  )

(deftest exp-test-partial
  (testing "Integral e^x is e^x"
    (let [pow (fn [x] (Math/exp x)), val (get-integral-partial pow)]
      (doall (map (fn [x] (is (< (Math/abs(double(- (val x)  (dec(pow x))))) epsilon))) (range 0 4))))
    )
  )

