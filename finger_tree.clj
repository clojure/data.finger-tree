;(ns clojure.contrib.finger-tree)

(use 'clojure.test)

(defn node [cache-fns & xs]
  (vec xs))


(defn deep-tree [l m r & [cache-fns]]
  [l m r cache-fns])

(defn single [[_ _ _ cache-fns] x]
  [(node cache-fns x) nil [] cache-fns])

(defn single? [l m r]
  (and (< (count l) 2) (empty? m) (empty? r)))

(defn ft-empty? [[l m r]]
  (and (empty? l) (empty? m) (empty? r)))


(defn ft-seq [t]
  (when-not (ft-empty? t)
    (let [[l m r] t]
      (lazy-cat l (apply concat (ft-seq m)) r))))

(defn ft-rseq [t]
  (when-not (ft-empty? t)
    (let [[l m r] t]
      (lazy-cat (reverse r)
                (mapcat reverse (ft-rseq m))
                (reverse l)))))

(defn conjl [t a]
  (if-not (ft-empty? t)
    (let [[l m r cache-fns] t]
      (if (< (count l) 4)
        (if (single? l m r)
          (deep-tree (node cache-fns a) nil l)
          (deep-tree (apply node cache-fns (cons a l)) m r))
        (let [[b c d e] l]
          (deep-tree [a b] (conjl m (node cache-fns c d e)) r))))
    (single t a)))

(defn conjr [t a]
  (if-not (ft-empty? t)
    (let [[l m r cache-fns] t]
      (if (< (count r) 4)
        (if (single? l m r)
          (deep-tree l nil (node cache-fns a))
          (deep-tree l m (apply node cache-fns (conj r a))))
        (let [[e d c b] r]
          (deep-tree l (conjr m (node cache-fns e d c)) (node cache-fns b a)))))
    (single t a)))

(defn finger-tree [cache-fns & xs]
  (reduce conjr (deep-tree [] nil [] cache-fns) xs))

(defn- nodes [cache-fns [a b c & xs :as s]]
  (assert (> (count s) 1))
  (condp = (count s)
    2 [(node cache-fns a b)]
    3 [(node cache-fns a b c)]
    4 [(node cache-fns a b) (node cache-fns c (first xs))]
    (cons (node cache-fns a b c) (nodes cache-fns xs))))

(defn- app3 [[l1 m1 r1 cache-fns :as t1] ts [l2 m2 r2 :as t2]]
  (cond
    (ft-empty? t1) (reduce conjl t2 (reverse ts))
    (ft-empty? t2) (reduce conjr t1 ts)
    (single? l1 m1 r1) (conjl (reduce conjl t2 (reverse ts)) (l1 0))
    (single? l2 m2 r2) (conjr (reduce conjr t1 ts) (l2 0))
    :else (deep-tree l1 (app3 m1 (nodes cache-fns (concat r1 ts l2)) m2) r2
                     cache-fns)))

(defn ft-concat [t1 t2]
  (assert (= (t1 3) (t2 3))) ; cache-fns must be the same
  (app3 t1 nil t2))

(deftest Conj-Seq
  (let [len 100]
    (are [x] (= x (range len))
      (ft-rseq (reduce conjl (finger-tree nil) (range len)))
      (ft-seq  (reduce conjr (finger-tree nil) (range len))))
    (are [x] (= x (range (dec len) -1 -1))
      (ft-rseq (reduce conjr (finger-tree nil) (range len)))
      (ft-seq  (reduce conjl (finger-tree nil) (range len))))
    
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree nil), vc [], i (int 0)]
        (if-not (< i 40)
          (is (= (ft-seq ft) (seq vc)))
          (if (zero? (rem i m))
            (recur (conjl ft i) (vec (cons i vc)) (inc i))
            (recur (conjr ft i) (conj vc i)       (inc i))))))))

(deftest Concat
  (doseq [a-len (range 25), b-len (range 25)]
    (let [a-s (map #(symbol (str % 'a)) (range a-len))
          b-s (map #(symbol (str % 'b)) (range b-len))
          a (apply finger-tree nil a-s)
          b (apply finger-tree nil b-s)]
      (is (= (seq (concat a-s b-s)) (ft-seq (ft-concat a b)))))))
