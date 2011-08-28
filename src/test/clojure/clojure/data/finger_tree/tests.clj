;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Tests for finger tree collections."
      :author "Chris Houser"}
  clojure.data.finger-tree.tests
  (:use [clojure.test :only [deftest is are]]
        [clojure.data.finger-tree
         :only [finger-tree meter conjl ft-concat ft-split-at split-tree
                opfn idElem measure measured to-tree getMeter
                double-list counted-double-list counted-sorted-set]])
  (:import (clojure.data.finger_tree CountedSortedSet CountedDoubleList)))

(deftest Conj-Seq-Queue
  (let [len 100]
    (are [x] (and (= (range len) x) (= x (range len)) (= x x)) 
      (rseq (reduce conjl (double-list) (range len)))
      (seq  (reduce conj  (double-list) (range len))))))

(deftest Conj-Seq-Stack
  (let [len 100, r (range (dec len) -1 -1)]
    (are [x] (and (= x r) (= r x) (= x x)) 
      (rseq (reduce conj  (double-list) (range len)))
      (seq  (reduce conjl (double-list) (range len))))))
    
(deftest Conj-Seq-Mixed
  (doseq [m (range 2 7)]
    (loop [ft (double-list), vc [], i (int 0)]
      (when (< i 40)
        (is (= ft vc))
        (is (= vc ft))
        (is (= ft ft))
        (if (zero? (rem i m))
          (recur (conjl ft i) (vec (cons i vc)) (inc i))
          (recur (conj  ft i) (conj vc i)       (inc i)))))))

(deftest Concat
  (doseq [a-len (range 25), b-len (range 25)]
    (let [a-s (map #(symbol (str % 'a)) (range a-len))
          b-s (map #(symbol (str % 'b)) (range b-len))
          a (apply double-list a-s)
          b (apply double-list b-s)
          s (concat a-s b-s)
          ft (ft-concat a b)]
      (is (= s ft))
      (is (= ft s))
      (is (= ft ft)))))

(defn test-split-at [expected-vec counted-tree tree-type]
  (dotimes [n (count expected-vec)]
    (let [[l m r] (ft-split-at counted-tree n)]
      (is (instance? tree-type l))
      (is (instance? tree-type r))
      (is (= (nth expected-vec n) m))
      (is (= n (count l)))
      (is (= (- (count expected-vec) n 1) (count r)))
      (is (= (seq (subvec expected-vec 0 n)) (seq l)))
      (is (= (seq (subvec expected-vec (inc n))) (seq r)))))
  
  (let [[l m r] (ft-split-at counted-tree -1)]
    (is (instance? tree-type l))
    (is (instance? tree-type r))
    (is (nil? m))
    (is (zero? (count l)))
    (is (= (count expected-vec) (count r)))
    (is (empty? l))
    (is (= (seq expected-vec) (seq r)))
    (is (= r expected-vec))
    (is (= r r)))

  (let [len (count expected-vec)
        [l m r] (ft-split-at counted-tree len)]
    (is (instance? tree-type l))
    (is (instance? tree-type r))
    (is (nil? m))
    (is (= len (count l)))
    (is (zero? (count r)))
    (is (= (seq expected-vec) (seq l)))
    (is (= l expected-vec))
    (is (= l l))
    (is (empty? r))))

(deftest CDLSplit
  (let [basevec (vec (map #(format "x%02d" %) (range 50)))]
    (dotimes [len (count basevec)]
      (let [lenvec (subvec basevec 0 len)]
        (test-split-at lenvec (apply counted-double-list lenvec)
                       CountedDoubleList)))))

(deftest CDLAssoc
  (doseq [len (range 50), n (range (inc len))]
    (let [v (assoc (vec (range len)) n :x)
          cdl (assoc (apply counted-double-list (range len)) n :x)]
      (is (= v cdl))
      (is (= cdl v))
      (is (= cdl cdl))
      (doseq [i (range len)]
        (is (= (nth v i) (nth cdl i)))
        (is (= (get v i) (get cdl i))))
      (doseq [i [-1 len]]
        (is (= (nth v i :nf) (nth cdl i :nf)))
        (is (= (get v i :nf) (get cdl i :nf)))))))

(deftest CDLAssocCons
  (doseq [len (range 50)]
    (let [v (vec (cons :x (range len)))
          cdl(assoc (apply counted-double-list (range len)) -1 :x)]
    (is (= v cdl))
    (is (= cdl v))
    (is (= cdl cdl)))))

(deftest CDLAssocFail
  (doseq [len (range 50), n [-2 (inc len)]]
    (is (thrown? Exception
                 (assoc (apply counted-double-list (range len)) n :x)))))

; XXX continue here
(deftest CSSConjDisj
  (let [values (vec (concat (range 50) [4.5 10.5 45.5 30.5]))]
    (dotimes [len (count values)]
      (let [pset (apply sorted-set (take len values))
            base (apply counted-sorted-set (take len values))] ; cons
        (is (= len (count base)))                    ; counted
        (dotimes [n len]
          (is (= pset (conj base (values n))))       ; exclusive set, next
          (is (= (nth (seq pset) n) (nth base n)))   ; indexed lookup
          (is (= (values n) (get base (values n))))) ; set lookup
        (reduce (fn [[pset base] value]              ; disj
                  (is (= pset base))
                  (is (= base pset))
                  (is (= (count pset) (count base)))
                  [(disj pset value) (disj base value)])
                [pset base] (take len values))))))

(deftest CSSSplitAt
  (let [basevec (vec (map #(format "x%02d" %) (range 50)))]
    (dotimes [len (count basevec)]
      (let [lenvec (subvec basevec 0 len)]
        (test-split-at lenvec (apply counted-sorted-set lenvec)
                       CountedSortedSet)))))

(deftest CSSPeekPop
  (let [basevec (vec (map #(format "x%02d" %) (range 50)))]
    (loop [v basevec, t (apply counted-sorted-set basevec)]
      (is (= (peek v) (peek t)))
      (is (= (seq v) (seq t)))
      (when (seq v)
        (recur (pop v) (pop t))))))

; for CSS: subseq, rsubseq

(defrecord Len-Meter [^int len])
(def measure-len (constantly (Len-Meter. 1)))
(def len-meter (meter measure-len
                      (Len-Meter. 0)
                      #(Len-Meter. (+ (:len %1) (:len %2)))))

(defrecord String-Meter [string])
(defn ^:static measure-str [node] (String-Meter. (str node)))
(def string-meter (meter measure-str
                         (String-Meter. "")
                         #(String-Meter. (str (:string %1) (:string %2)))))


(defrecord Len-String-Meter [len string])

(def len-string-meter
  (let [len-op (opfn len-meter)
        string-op (opfn string-meter)]
    (meter
      (fn [o]
        (Len-String-Meter. (:len (measure len-meter o))
                           (:string (measure string-meter o))))
      (Len-String-Meter. (:len (idElem len-meter))
                         (:string (idElem string-meter)))
      (fn [a b] (Len-String-Meter.
                  (:len (len-op a b))
                  (:string (string-op a b)))))))

(deftest Annotate-One-Direction
  (let [measure-fns len-string-meter]
    (let [len 100]
      (are [x] (= x (Len-String-Meter. len (apply str (range len))))
        (measured (reduce conj  (finger-tree measure-fns) (range len))))
      (are [x] (= x (Len-String-Meter. len (apply str (reverse (range len)))))
        (measured (reduce conjl (finger-tree measure-fns) (range len)))))))
      
(deftest Annotate-Mixed-Conj
  (let [measure-fns len-string-meter]
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree measure-fns), vc [], i (int 0)]
        (when (< i 40)
          (is (= (measured ft) (Len-String-Meter. (count vc) (apply str vc))))
          (if (zero? (rem i m))
            (recur (conjl ft i) (vec (cons i vc)) (inc i))
            (recur (conj  ft i) (conj vc i)       (inc i))))))))

(deftest Ann-Conj-Seq-Queue
  (let [len 100]
    (are [x] (= (map identity x) (range len))
      (rseq (reduce conjl (counted-double-list) (range len)))
      (seq  (reduce conj  (counted-double-list) (range len))))))

(deftest Counted-Test
  (let [xs (map #(str "x" %) (range 1000))
        cdl (apply counted-double-list xs)]
    (is (= (concat [nil] xs [nil]) (map #(get cdl %) (range -1 1001))))))

(deftest Annotate-Concat
  (let [measure-fns len-string-meter]
    (doseq [a-len (range 25), b-len (range 25)]
      (let [a-s (map #(symbol (str % 'a)) (range a-len))
            b-s (map #(symbol (str % 'b)) (range b-len))
            a (apply finger-tree measure-fns a-s)
            b (apply finger-tree measure-fns b-s)]
        (is (= (Len-String-Meter.
                 (+ (count a-s) (count b-s))
                 (apply str (concat a-s b-s)))
               (measured (ft-concat a b))))))))

(deftest Split
  (let [make-item (fn [i] (symbol (str i 'a)))]
    (doseq [len (range 10)
            :let [tree (to-tree len-string-meter (map make-item (range len)))]
            split-i (range len)]
      (is (= [len split-i (make-item split-i)]
             [len split-i (second (split-tree tree #(< split-i (:len %))))])))))

(defrecord Right-Meter [right])
(defn measure-right [x] (Right-Meter. x))
(def zero-right (Right-Meter. nil))
(def right-meter
  (meter measure-right
         zero-right
         #(if (:right %2) %2 %1)))

(defn insert-where [tree pred value]
  (if (empty? tree)
    (conj tree value)
    (let [[l x r] (split-tree tree pred)
          [a b] (if (pred (measure (getMeter tree) x)) [value x] [x value])]
      (ft-concat (conj l a) (conjl r b)))))


(deftest Sorted-Set
  (let [r (java.util.Random. 42)]
    (reduce (fn [[t s] i]
              (let [t2 (insert-where t
                                     #(when-let [r (:right %)] (< i r))
                                     i)
                    s (conj s i)]
                (is (every? true? (map = s t2)))
                [t2 s]))
            [(finger-tree right-meter) (sorted-set)]
            (take 2 (repeatedly #(.nextInt r))))))

(deftest Remove-From-Empty-Trees
  (is (= () (pop (double-list))))
  (is (= () (rest (double-list))))
  (is (= () (pop (counted-double-list))))
  (is (= () (rest (counted-double-list))))
  (is (= #{} (pop (counted-sorted-set))))
  (is (= #{} (rest (counted-sorted-set))))
  (is (= #{} (disj (counted-sorted-set) :foo))))

(deftest Get-Empty-Trees
  (is (nil? (first (double-list))))
  (is (nil? (peek (double-list))))
  (is (nil? (get (double-list) :anything)))
  (is (nil? (first (counted-double-list))))
  (is (nil? (peek (counted-double-list))))
  (is (nil? (get (counted-double-list) 0)))
  (is (nil? (first (counted-sorted-set))))
  (is (nil? (peek (counted-sorted-set))))
  (is (nil? (get (counted-sorted-set) :foo))))

(deftest Get-Not-Found
  (is (= :notfound (get (double-list) :anything :notfound)))
  (is (= :notfound (get (counted-double-list) 0 :notfound)))
  (is (= :notfound (get (counted-sorted-set) :foo :notfound))))

(deftest Unequal-Lists
  (doseq [[a b] [[[] [1]] [[1] []] [[1] [2]] [[1 2] [2 1]]]
          ctor [double-list counted-double-list]]
    (let [aobj (apply ctor a)]
      (is (not= aobj b))
      (is (not= b aobj))
      (doseq [afn [#(apply hash-set %) #(zipmap % %)]]
        (is (not= aobj (afn a)))
        (is (not= (afn a) aobj))))))

(deftest Unequal-Sets
  (doseq [[a b] [[[] [1]] [[1] []] [[1] [2]] [[1 2] [2 1]]]]
    (let [aobj (apply counted-sorted-set a)]
      (is (not= aobj b))
      (is (not= b aobj))
      (is (not= aobj (zipmap a a)))
      (is (not= (zipmap a a) aobj)))))

(deftest Meta
  (doseq [data [[] [3 2 1] (range 50)]
          ctor [double-list counted-double-list counted-sorted-set]]
    (let [mdata {:foo :bar}
          coll (with-meta (apply ctor data) mdata)]
      (is (= mdata (meta coll)))
      (is (= coll (with-meta coll nil))))))
