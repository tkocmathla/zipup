(ns zipup.core-test
  (:require
    [clojure.test :refer :all]
    [clojure.zip :as z]
    [zipup.core :as zu]))

(deftest top?-test
  (is (true? (zu/top? (z/vector-zip []))))
  (is (true? (zu/top? (-> (z/vector-zip [:a]) z/down z/up))))
  (is (false? (zu/top? (-> (z/vector-zip [:a]) z/down))))
  (is (false? (zu/top? (-> (z/vector-zip [:a :b]) z/down z/right))))
  (is (false? (zu/top? (-> (z/vector-zip [:a [:b]]) z/down z/right z/down)))))

(deftest root-loc-test
  (is (= [] (-> (z/vector-zip []) zu/root-loc z/node)))
  (is (= [:a] (-> (z/vector-zip [:a]) zu/root-loc z/node)))
  (is (= [:a] (-> (z/vector-zip [:a]) z/down zu/root-loc z/node)))
  (is (= [nil :b] (-> (z/vector-zip [nil :b]) z/down z/right zu/root-loc z/node)))
  (is (= [:a [:b] :c] (-> (z/vector-zip [:a [:b] :c]) z/down z/right z/down zu/root-loc z/node))))

(deftest move-nth-test
  (is (= :b (z/node (zu/move-nth (comp z/right z/down) (z/vector-zip [:a :b]) 1))))
  (is (= :c (z/node (zu/move-nth (comp z/right z/down) (z/vector-zip [:a [:b :c]]) 2)))))

(deftest copy-zip-test
  (testing "append"
    (is (= [:a [:b :c [[:e] :d]] :d]
           (-> (z/vector-zip [:a [:b :c [[:e] :d]]])
               (zu/next-nth 8)
               (zu/copy-zip (comp z/left z/up z/up))
               z/root))))
  (testing "replace"
    (is (= [:d [:b :c [[:e] :d]]]
           (-> (z/vector-zip [:a [:b :c [[:e] :d]]])
               (zu/next-nth 8)
               (zu/copy-zip (comp z/left z/up z/up) :replace? true)
               z/root)))))

(deftest cut-zip-test
  (is (= [:a [[[:e] :d]]]
         (-> (z/vector-zip [:a [:b :c [[:e] :d]]])
             (zu/next-nth 5)
             (zu/cut-zip z/left z/left)
             z/root))))

(deftest map-zip-test
  (is (= [2 :a 3 [4]]
         (->> (z/vector-zip [1 :a 2 [3]]) 
              (zu/map-zip #(cond-> % (number? %) inc))
              z/node)))
  (is (= ["1" ":a" "2" ["3"]]
         (->> (z/vector-zip [1 :a 2 [3]])
              (zu/map-zip str)
              z/node))))
