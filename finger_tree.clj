;(ns clojure.contrib.finger-tree)

(use 'clojure.test)


(defn finger-tree [l m r]
  [l m r])

(defn single [x]
  [[x] nil []])

(defn single? [l m r]
  (and (< (count l) 2) (empty? m) (empty? r)))


(defn ft-seq [t]
  (when-let [[l m r] t]
    (lazy-cat l (apply concat (ft-seq m)) r)))

(defn ft-rseq [t]
  (when-let [[l m r] t]
    (lazy-cat (reverse r)
              (mapcat reverse (ft-rseq m))
              (reverse l))))

(defn conjl [t a]
  (if-let [[l m r] t]
    (if (< (count l) 4)
      (if (single? l m r)
        (finger-tree [a] nil l)
        (finger-tree (vec (cons a l)) m r))
      (let [[b c d e] l]
        (finger-tree [a b] (conjl m [c d e]) r)))
    (single a)))

(defn conjr [t a]
  (if-let [[l m r] t]
    (if (< (count r) 4)
      (if (single? l m r)
        (finger-tree l nil [a])
        (finger-tree l m (conj r a)))
      (let [[e d c b] r]
        (finger-tree l (conjr m [e d c]) [b a])))
    (single a)))

(defn nodes [[a b c & xs :as s]]
  (assert (> (count s) 1))
  (condp = (count s)
    2 [[a b]]
    3 [[a b c]]
    4 [[a b] [c (first xs)]]
    (cons [a b c] (nodes xs))))

(defn app3 [[l1 m1 r1 :as t1] ts [l2 m2 r2 :as t2]]
  (cond
    (empty? t1) (reduce conjl t2 (reverse ts))
    (empty? t2) (reduce conjr t1 ts)
    (single? l1 m1 r1) (conjl (reduce conjl t2 (reverse ts)) (l1 0))
    (single? l2 m2 r2) (conjr (reduce conjr t1 ts) (l2 0))
    :else (finger-tree l1 (app3 m1 (nodes (concat r1 ts l2)) m2) r2)))

(defn ft-concat [t1 t2]
  (app3 t1 nil t2))

(deftest Conj-Seq
  (let [len 100]
    (are [x] (= x (range len))
      (ft-rseq (reduce conjl nil (range len)))
      (ft-seq  (reduce conjr nil (range len))))
    (are [x] (= x (range (dec len) -1 -1))
      (ft-rseq (reduce conjr nil (range len)))
      (ft-seq  (reduce conjl nil (range len))))
    
    (doseq [m (range 2 7)]
      (loop [ft nil, vc [], i (int 0)]
        (if-not (< i 40)
          (is (= (ft-seq ft) (seq vc)))
          (if (zero? (rem i m))
            (recur (conjl ft i) (vec (cons i vc)) (inc i))
            (recur (conjr ft i) (conj vc i)       (inc i))))))))

(deftest Concat
  (doseq [a-len (range 25), b-len (range 25)]
    (let [a-s (map #(symbol (str % 'a)) (range a-len))
          b-s (map #(symbol (str % 'b)) (range b-len))
          a (reduce conjr nil a-s)
          b (reduce conjr nil b-s)]
      (is (= (seq (concat a-s b-s)) (ft-seq (ft-concat a b)))))))
