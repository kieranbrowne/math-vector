(ns math-vector.core-test
  (:require [clojure.test :refer :all]
            [math-vector.core :refer :all]))


(deftest shape-test
  (testing "Shape of a scalar"
    (is (= [] (shape 6)))
    (is (= [] (shape -20.0)))
    (is (= [] (shape 0))))
  (testing "Shape of a vector"
    (is (= [0] (shape [])))
    (is (= [1] (shape [-20.0])))
    (is (= [3] (shape [5 0 -2]))))
  (testing "Shape of a matrix"
    (is (= [1 0] (shape [[]])))
    (is (= [1 1] (shape [[1]])))
    (is (= [3 0] (shape [[] [] []])))
    (is (= [3 1] (shape [[2] [1] [7]]))))
  (testing "Shape of a tensor"
    (is (= [3 1 0] (shape [[[]] [[]] [[]]])))
    (is (= [1 1 1] (shape [[[2.4]]])))
    (is (= [1 1 1 1] (shape [[[[2.4]]]])))
    (is (= [2 1 1 1] (shape [[[[2.4]]] [[[2.4]]]])))
  ))


(deftest mul-test
  (testing "Mul two scalars"
    (is (= 42 (mul 6 7)))
    (is (= 42.0 (mul 6.0 7)))
    (is (= 42.0 (mul 7.0 6.0))))
  (testing "Mul scalar by vector"
    (is (= [2 4 6 8] (mul 2 [1 2 3 4])))
    (is (= [2.0 4 -6.0 8] (mul 2 [1.0 2 -3.0 4])))
    )
  (testing "Mul vector by scalar"
    (is (= [2 4 6 8] (mul [1 2 3 4] 2)))
    (is (= [2.0 4 -6.0 8] (mul [1.0 2 -3.0 4] 2)))
    )
  (testing "Matrix-vector multiplication"
    ;; rotate vector -90deg
    (is (= [-1 1] (mul [[0 1] [-1 0]] [1 1])))
    (is (= [-7 -2] (mul [[0 1] [-1 0]] [-2 7])))
    ;; shear transformation
    (is (= [2 1] (mul [[1 0] [1 1]] [1 1])))
    (is (= [4 2] (mul [[1 0] [1 1]] [2 2])))
    ))

(deftest add-test
  (testing "Add two scalars"
    (is (= 13 (add 6 7)))
    (is (= 13.0 (add 6.0 7)))
    (is (= 13.0 (add 7.0 6.0))))
  (testing "Add scalar to vector"
    (is (= [3 4 5 6] (add 2 [1 2 3 4])))
    (is (= [3.0 4 -1.0 6] (add 2 [1.0 2 -3.0 4])))
    )
  (testing "Add vector to scalar"
    (is (= [3 4 5 6] (add [1 2 3 4] 2)))
    (is (= [3.0 4 -1.0 6] (add [1.0 2 -3.0 4] 2)))
    ))


(deftest normalise-test
  (testing "Normalize vertical or horizontal vectors"
    (is (= [1.0] (normalize [1])))
    (is (= [1.0] (normalize [9])))
    (is (= [-1.0] (normalize [-200])))
    (is (= [1.0 0.0] (normalize [1 0])))
    (is (= [1.0 0.0] (normalize [11 0])))
    (is (= [0.0 1.0] (normalize [0 1])))
    (is (= [0.0 -1.0] (normalize [0 -20.2])))
    (is (= [0.0 -1.0 0.0] (normalize [0 -20.2 0])))
    (is (= [0.0 0.0 1.0] (normalize [0 0 10000])))
    (is (= [0.0 0.0 1.0] (normalize [0 0 1e-8])))
    )
  (testing "Normalize complex vectors"
    (is (= (map float [0.70710677 0.70710677]) (map float (normalize [1 1]))))
    (is (= (map float [-0.70710677 -0.70710677])
           (map float (normalize [-1 -1]))))
    (is (=
         (map float [0.57735026 -0.57735026 0.57735026])
         (map float (normalize [1 -1 1]))))
    ))


(deftest size-test
  (testing "Find size of vectors"
    (is (= 1.0 (size [1])))
    (is (= 9.0 (size [9])))
    (is (= 200.0 (size [-200])))
    (is (= 1.0 (size [1 0])))
    (is (= 11.0 (size [11 0])))
    (is (= 1.0 (size [0 1])))
    (is (= 20.2 (size [0 -20.2])))
    (is (= 20.2 (size [0 -20.2 0])))
    (is (= 10000.0 (size [0 0 10000])))
    (is (= 1e-8 (size [0 0 1e-8])))
    )
  (testing "Size complex vectors"
    (is (= (float 1.4142135) (float (size [1 1]))))
    (is (= (float 1.4142135) (float (size [-1 -1]))))
    (is (= (float 1.7320508) (float (size [1 -1 1]))))
    ))



(deftest matmul-test
  (testing "Matrix Multiplication"
    (is (= [[2 1] [0 -2]]
           (matmul [[0 1] [2 0]] [[1 1] [-2 0]])))))
