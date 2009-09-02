;(ns clojure.contrib.finger-tree)

(use 'clojure.test)

(gen-interface
  :name clojure.lang.IFingerTreeNode
  :methods [[consLeft  [Object] clojure.lang.IFingerTreeNode]
            [consRight [Object] clojure.lang.IFingerTreeNode]
            [measure [] Object]
            [measureFns [] clojure.lang.IPersistentCollection]])

(gen-interface
  :name clojure.lang.IMeasureFn
  :methods [[iden [] Object]
            [measure [Object] Object]
            [reduce [Object Object] Object]])

(gen-interface
  :name clojure.lang.IPrintable
  :methods [[print [java.io.Writer] Object]])

(import '(clojure.lang Indexed Seqable IFingerTreeNode IPrintable))

(set! *warn-on-reflection* true)

(defn mes* [cache-fns & xs]
  (into cache-fns
        (if (instance? IFingerTreeNode (first xs))
          (let [mes-maps (map #(.measure %) xs)]
            (for [[k [mes red]] cache-fns]
              [k (reduce red (map k mes-maps))]))
          (for [[k [mes red]] cache-fns]
            [k (reduce red (map mes xs))]))))

(defn red* [cache-fns v1 v2]
  (zipmap (keys cache-fns)
          (for [[k [mes red]] cache-fns]
            (red (k v1) (k v2)))))

(defn iden* [cache-fns & xs]
  (into cache-fns (map #(nth %2 2) cache-fns)))


(defmethod print-method IPrintable [x w] (.print #^IPrintable x w))

(defn #^IFingerTreeNode digit [measure-fns & xs]
  (assert (<= 1 (count xs) 4))
  (let [xs-vec (vec xs)]
    (new [IFingerTreeNode Indexed Seqable IPrintable] this
      (consLeft  [x] (apply digit measure-fns x xs-vec))
      (consRight [x] (apply digit measure-fns (conj xs-vec x)))
      (measure   []  (apply mes* measure-fns xs-vec))
      (measureFns[]  measure-fns)
      (nth       [i] (nth xs-vec i))
      (count     []  (count xs-vec))
      (seq       []  (seq xs-vec))
      (toString  []  (str xs-vec))
      (print     [w] (.write w (str "#<digit " this ">"))))))

(defn #^IFingerTreeNode node2 [measure-fns a b]
  (let [mval (mes* measure-fns a b)]
    (new [IFingerTreeNode Indexed Seqable IPrintable] this
      (measure   []  mval)
      (measureFns[]  measure-fns)
      (nth       [i] (if (zero? i) a b))
      (count     []  (int 2))
      (seq       []  (list a b))
      (toString  []  (str [a b] mval))
      (print     [w] (.write w (str "#<node2 " this ">"))))))

(defn #^IFingerTreeNode node3 [measure-fns a b c]
  (let [mval (mes* measure-fns a b c)]
    (new [IFingerTreeNode Indexed Seqable IPrintable] this
      (measure   []  mval)
      (measureFns[]  measure-fns)
      (nth       [i] (condp == i (int 0) a (int 1) b c))
      (count     []  (int 3))
      (seq       []  (list a b c))
      (toString  []  (str [a b c] mval))
      (print     [w] (.write w (str "#<node3 " this ">"))))))

(declare single deep)

(defn empty-ft [measure-fns]
  (new [IFingerTreeNode Indexed Seqable IPrintable] this
    (consLeft  [a] (single measure-fns a))
    (consRight [b] (single measure-fns b))
    (measure   []  (iden* measure-fns))
    (measureFns[]  measure-fns)
    (count     []  0)
    (seq       []  nil)
    (toString  []  (keys measure-fns))
    (print     [w] (.write w (str "#<empty " this ">")))))

(defn single [measure-fns x]
  (new [IFingerTreeNode Indexed Seqable IPrintable] this
    (consLeft  [a] (deep (digit measure-fns a) nil (digit measure-fns x)))
    (consRight [b] (deep (digit measure-fns x) nil (digit measure-fns b)))
    (measure   []  (if (instance? IFingerTreeNode x)
                     (.measure x)
                     (mes* measure-fns x)))
    (nth       [i] x)
    (count     []  1)
    (seq       []  (cons x nil))
    (toString  []  (str x (.measure #^IFingerTreeNode this)))
    (print     [w]
      (binding [*out* w]
        (print "#<single ")
        (pr x)
        (pr (.measure #^IFingerTreeNode this))
        (print ">")))))

(defn delayed-ft [tree-ref mval]
  (new [IFingerTreeNode Seqable IPrintable] this
    (consLeft  [a] (.consLeft #^IFingerTreeNode @tree-ref a))
    (consRight [b] (.consRight #^IFingerTreeNode @tree-ref b))
    (measure   []  mval)
    (seq       []  (.seq #^Seqable @tree-ref))
    (toString  []  (.toString #^Object @tree-ref))
    (print     [w]
      (binding [*out* w]
        (print "#<delay ")
        (pr @tree-ref)
        (print ">")))))

(defmacro delay-ft [tree-expr mval]
  `(delayed-ft (delay ~tree-expr) ~mval))

(defn deep [#^IFingerTreeNode pre, #^IFingerTreeNode m, #^IFingerTreeNode suf]
  (assert (= (.measureFns pre) (.measureFns suf)))
  (let [measure-fns (.measureFns pre)
        mval (if m
               (mes* measure-fns pre m suf)
               (mes* measure-fns pre suf))]
    (new [IFingerTreeNode Seqable IPrintable] this
      (consLeft  [a] (if (< (count pre) 4)
                       (deep (.consLeft pre a) m suf)
                       (let [[b c d e] pre
                             n (node3 measure-fns c d e)]
                         ;(prn :n n)
                         (deep (digit measure-fns a b)
                               (if m
                                 (delay-ft (.consLeft m n)
                                           (mes* measure-fns n m))
                                 (single measure-fns n))
                                 ;(delay-ft (.consLeft (empty-ft measure-fns) n)
                                 ;          (.measure n)))
                               suf))))
      (consRight [b]) ;TBD
      (measure   []  mval)
      (seq       []) ;TBD
      (toString  []  "deep-finger-tree")
      (print     [w]
        (binding [*out* w]
          (print "#<deep ")
          (pr pre m suf)
          (print " ")
          (pr (.measure #^IFingerTreeNode this))
          (print ">"))))))

(set! *warn-on-reflection* false)
;=====

(defn deep-tree [l m r cache-fns m-vals]
  [l m r cache-fns m-vals])

(defn tree-vals [[l m r cache-fns m-vals]]
  (into cache-fns
        (for [[k [mes red]] cache-fns]
          [k (reduce red (concat (when-not (empty? l) [(k (.measure l))])
                                 (when m-vals [(k m-vals)])
                                 (when-not (empty? r) [(k (.measure r))])))])))

(defn single? [l m r]
  (and (< (count l) 2) (nil? m) (empty? r)))

(defn ft-empty? [[l m r]]
  (and (empty? l) (nil? m) (empty? r)))


(defn ft-seq [t]
  (when-not (ft-empty? t)
    (let [[l m r] t]
      (lazy-cat l (apply concat (ft-seq (and m @m))) r))))

(defn ft-rseq [t]
  (when-not (ft-empty? t)
    (let [[l m r] t]
      (lazy-cat (reverse r)
                (mapcat reverse (ft-rseq (and m @m)))
                (reverse l)))))

(defn conjl [t a]
  (if-not (ft-empty? t)
    (let [[l m r cache-fns m-vals] t]
      (if (< (count l) 4)
        (if (single? l m r)
          (deep-tree (digit cache-fns a) nil l (t 3) nil)
          (deep-tree (.consLeft l a) m r (t 3) m-vals))
        (let [[b c d e] l
              n (digit cache-fns c d e)]
          (deep-tree (digit cache-fns a b)
                     (delay (conjl (and m @m) n))
                     r
                     (t 3)
                     (if m-vals
                       (red* cache-fns (.measure n) m-vals)
                       (.measure n))))))
    (single t a)))

(defn conjr [t a]
  (if-not (ft-empty? t)
    (let [[l m r cache-fns m-vals] t]
      (if (< (count r) 4)
        (if (single? l m r)
          (deep-tree l nil (digit cache-fns a) (t 3) nil)
          (deep-tree l m (.consRight r a) (t 3) m-vals))
        (let [[e d c b] r
              n (digit cache-fns e d c)]
          (deep-tree l
                     (delay (conjr (and m @m) n))
                     (digit cache-fns b a)
                     (t 3)
                     (if m-vals
                       (red* cache-fns m-vals (.measure n))
                       (.measure n))))))
    (single t a)))

(defn finger-tree [cache-fns & xs]
  (reduce conjr (deep-tree nil nil nil cache-fns nil) xs))


(defn- nodes [cache-fns [a b c & xs :as s]]
  (assert (> (count s) 1))
  (condp = (count s)
    2 [(digit cache-fns a b)]
    3 [(digit cache-fns a b c)]
    4 [(digit cache-fns a b) (digit cache-fns c (first xs))]
    (cons (digit cache-fns a b c) (nodes cache-fns xs))))

(defn- app3 [[l1 m1 r1 cache-fns m1-vals :as t1] ts [l2 m2 r2 _ m2-vals :as t2]]
  (cond
    (ft-empty? t1) (reduce conjl t2 (reverse ts))
    (ft-empty? t2) (reduce conjr t1 ts)
    (single? l1 m1 r1) (conjl (reduce conjl t2 (reverse ts)) (nth l1 0))
    (single? l2 m2 r2) (conjr (reduce conjr t1 ts) (nth l2 0))
    :else (let [n-s (nodes cache-fns (concat r1 ts l2))]
            (deep-tree l1
                       (delay (app3 (and m1 @m1) n-s (and m2 @m2)))
                       r2
                       cache-fns
                       (into cache-fns
                             (for [[k [mes red]] cache-fns]
                               [k (reduce red (concat
                                                (when m1-vals [(k m1-vals)])
                                                (map #(k (.measure %)) n-s)
                                                (when m2-vals [(k m2-vals)])))]))))))

(defn ft-concat [t1 t2]
  (assert (= (t1 3) (t2 3))) ; cache-fns must be the same
  (app3 t1 nil t2))

(defn- split-digit
  "The last arg is a simple collection.  Returns [coll item coll]"
  [cache-fns pred acc [a & as]]
  (if-not as
    [nil a nil]
    (let [next-acc (red* cache-fns acc (mes* cache-fns a))]
      (if (pred next-acc)
        [nil a as]
        (let [[l x r] (split-digit cache-fns pred next-acc as)]
          [(cons a l) x r])))))

;(defn- split-tree [pred acc [l m r cache-fns m-vals]]
;  (let [vpr (red* cache-fns acc (.measure l))
;        vm  (red* cache-fns vpr m-vals)]
;    (cond
;      (single? l m r) [(finger-tree cache-fns) (l 0) (finger-tree cache-fns)]
;      (pred vpr) (let [[sl sx sr] (split-digit pred acc l)]
;                   [(apply finger-tree cache-fns sl)
;                    sx
;                    (deep-tree sr m r cache-fns m-vals)]) ; deepl
;      (pred vm) 


(deftest Conj-Seq-Queue
  (let [len 100]
    (are [x] (= x (range len))
      (ft-rseq (reduce conjl (finger-tree nil) (range len)))
      (ft-seq  (reduce conjr (finger-tree nil) (range len))))))

(deftest Conj-Seq-Stack
  (let [len 100]
    (are [x] (= x (range (dec len) -1 -1))
      (ft-rseq (reduce conjr (finger-tree nil) (range len)))
      (ft-seq  (reduce conjl (finger-tree nil) (range len))))))
    
(deftest Conj-Seq-Mixed
  (doseq [m (range 2 7)]
    (loop [ft (finger-tree nil), vc [], i (int 0)]
      (when (< i 40)
        (is (= (ft-seq ft) (seq vc)))
        (if (zero? (rem i m))
          (recur (conjl ft i) (vec (cons i vc)) (inc i))
          (recur (conjr ft i) (conj vc i)       (inc i)))))))

(deftest Concat
  (doseq [a-len (range 25), b-len (range 25)]
    (let [a-s (map #(symbol (str % 'a)) (range a-len))
          b-s (map #(symbol (str % 'b)) (range b-len))
          a (apply finger-tree nil a-s)
          b (apply finger-tree nil b-s)]
      (is (= (seq (concat a-s b-s)) (ft-seq (ft-concat a b)))))))

(deftest Annotate-One-Direction
  (let [cache-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (let [len 100]
      (are [x] (= x {:size len :str (apply str (range len))})
        (tree-vals (reduce conjr (finger-tree cache-fns) (range len))))
      (are [x] (= x {:size len :str (apply str (reverse (range len)))})
        (tree-vals (reduce conjl (finger-tree cache-fns) (range len)))))))
      
(deftest Annotate-Mixed-Conj
  (let [cache-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree cache-fns), vc [], i (int 0)]
        (when (< i 40)
          (is (= (tree-vals ft) {:size (count vc) :str (apply str vc)}))
          (if (zero? (rem i m))
            (recur (conjl ft i) (vec (cons i vc)) (inc i))
            (recur (conjr ft i) (conj vc i)       (inc i))))))))

(deftest Annotate-Concat
  (let [cache-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [a-len (range 25), b-len (range 25)]
      (let [a-s (map #(symbol (str % 'a)) (range a-len))
            b-s (map #(symbol (str % 'b)) (range b-len))
            a (apply finger-tree cache-fns a-s)
            b (apply finger-tree cache-fns b-s)]
        (is (= {:size (+ (count a-s) (count b-s))
                :str (apply str (concat a-s b-s))}
               (tree-vals (ft-concat a b))))))))
