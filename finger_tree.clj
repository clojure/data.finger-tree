(ns clojure.lang.fingertree)
(comment ; TODO:

- test performance
- add pre-packaged ctor for vector-like obj, perhaps also priority queue
- implement java.util.Collection
- implement equals
- implement IMeta
- implement IChunkedSeq?
- fix clojure core.clj to call consLeft/consRight
- replace copy/pasted code with macros
- test deque complexity
- confirm recursion is bounded, though perhaps O(log n) growth is slow enough
)

;(set! *warn-on-reflection* true)

(defprotocol DoubleSeq
  (consl [s a] "Append a to the left-hand side of s")
  (conjr [s b] "Append b to the right-hand side of s"))

(defprotocol Measured
  (measure [o] "Return the measured value of o")
  (measureFns [o] "Return the functions used to compute measure of o"))

(defprotocol Splittable
  (split [o pred iden] "Return [pre m post] where pre and post are trees"))

(defprotocol Tree
  (app3 [t1 ts t2] "Append ts and (possibly deep) t2 to tree t1")
  (app3deep [t2 ts t1] "Append ts and t2 to deep tree t1")
  (measureMore [o] "Return the measure of o not including the leftmost item")
  (measurePop [o] "Return the measure of o not including the rightmost item"))


(use 'clojure.test)
(import '(clojure.lang Seqable ISeq IPersistentStack IPersistentCollection
                       Reversible Indexed))
(declare newEmptyTree newSingleTree newDeepTree digit deep)

(defmacro ^:private defdigit [& items]
  (let [i (gensym "i_")
        p (gensym "p_")
        o (gensym "o_")
        typename (symbol (str "Digit" (count items)))
        this-items (map #(list (keyword %) o) items)]
   `(deftype ~typename [~@items ~'measure-fns ~'measure-ref]
      Seqable
        (seq [_] (list ~@items))
      Indexed
        (count [_] ~(count items)) ; not needed?
        (nth [_ ~i notfound#]
          (cond ~@(mapcat (fn [sym n] [`(== ~i (int ~n)) sym])
                          items
                          (range (count items)))
            :else notfound#))
      IPersistentCollection
        (cons [_ x]) ; TBD
        (empty [_]) ; TBD ; not needed?
        (equiv [_ x]) ; TBD
      ISeq
        (first     [_] ~(first items))
        (more      [_] ~(if (> (count items) 1)
                          `(digit ~'measure-fns ~@(next items))
                          `(newEmptyTree ~'measure-fns)))
        (next      [_] ~(when (> (count items) 1)
                          `(digit ~'measure-fns ~@(next items))))
      IPersistentStack
        (peek      [_] ~(last items))
        (pop       [_] ~(if (> (count items) 1)
                          `(digit ~'measure-fns ~@(drop-last items))
                          `(newEmptyTree ~'measure-fns)))
      DoubleSeq
        (consl [_ x#] (digit ~'measure-fns x# ~@items))
        (conjr [_ x#] (digit ~'measure-fns ~@items x#))
      Measured
        (measure [_] @~'measure-ref)
        (measureFns [_] ~'measure-fns) ; not needed?
      Splittable
        (split [_ ~p ~i]
          ~(letfn [(step [ips [ix & ixs]]
                      (if (empty? ixs)
                        [(when ips `(digit ~'measure-fns ~@ips))
                         ix
                         nil]
                        `(let [~i (red* ~'measure-fns
                                        ~i
                                        (mes* ~'measure-fns ~ix))]
                           (if (~p ~i)
                             [~(when ips
                                 `(digit ~'measure-fns ~@ips))
                              ~ix
                              (digit ~'measure-fns ~@ixs)]
                             ~(step (concat ips [ix]) ixs)))))]
             (step nil items))))))

(defmacro ^:private make-digit [measure-fns & items]
  (let [typename (symbol (str "Digit" (count items)))]
    `(let [mfns# ~measure-fns]
       (new ~typename ~@items mfns# (delay (~'mes* mfns# ~@items))))))

(defn iden* [measure-fns]
    (into measure-fns (for [[k [_ _ iden]] measure-fns] [k iden])))

(defn mes* [measure-fns & xs]
  (into measure-fns
        (if (and (first xs) (satisfies? Measured (first xs)))
          (let [mes-maps (map #(if %
                                 (measure %)
                                 (iden* measure-fns))
                              xs)]
            (for [[k [mes red]] measure-fns]
              [k (reduce red (map k mes-maps))]))
          (for [[k [mes red]] measure-fns]
            [k (reduce red (map mes xs))]))))

(defn red* [measure-fns v1 v2]
  (zipmap (keys measure-fns)
          (for [[k [mes red]] measure-fns]
            (red (k v1) (k v2)))))

(defdigit a)
(defdigit a b)
(defdigit a b c)
(defdigit a b c d)

(defn digit
  ([measure-fns a]       (make-digit measure-fns a))
  ([measure-fns a b]     (make-digit measure-fns a b))
  ([measure-fns a b c]   (make-digit measure-fns a b c))
  ([measure-fns a b c d] (make-digit measure-fns a b c d)))

(defn nodes [mfns xs]
  (let [v (vec xs), c (count v)]
    (seq
      (loop [i (int 0), nds []]
        (condp == (- c i)
          (int 2) (-> nds (conj (digit mfns (v i) (v (+ (int 1) i)))))
          (int 3) (-> nds (conj (digit mfns (v i) (v (+ (int 1) i))
                                       (v (+ (int 2) i)))))
          (int 4) (-> nds (conj (digit mfns (v i) (v (+ (int 1) i))))
                    (conj (digit mfns (v (+ (int 2) i))
                                 (v (+ (int 3) i)))))
          (recur (+ (int 3) i)
                 (-> nds
                   (conj (digit mfns (v i) (v (+ (int 1) i))
                                (v (+ (int 2) i)))))))))))

(deftype EmptyTree [measure-fns]
  Seqable
    (seq [_] nil)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_] 0) ; not needed?
    (empty [this] this)
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] nil)
    (more [this] this)
    (next [_] nil)
  IPersistentStack
    (peek [_] nil)
    (pop [this] this)
  Reversible
    (rseq [_] nil)
  DoubleSeq
    (consl [_ a] (newSingleTree measure-fns a))
    (conjr [_ b] (newSingleTree measure-fns b))
  Measured
    (measure [_] (iden* measure-fns))
    (measureFns [_] measure-fns) ; not needed?
;  Splittable
;    (split [pred iden]) ; TBD -- not needed??
  Tree
    (app3 [_ ts t2] (reduce consl t2 (reverse ts)))
    (app3deep [_ ts t1] (reduce conjr t1 ts))
    (measureMore [_] (iden* measure-fns))
    (measurePop [_] (iden* measure-fns)))

(defn newEmptyTree [measure-fns]
  (EmptyTree. measure-fns))

(deftype SingleTree [measure-fns x]
  Seqable
    (seq [this] this)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_]) ; not needed?
    (empty [_] (EmptyTree. measure-fns)) ; not needed?
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] x)
    (more [_] (EmptyTree. measure-fns))
    (next [_] nil)
  IPersistentStack
    (peek [_] x)
    (pop [_] (EmptyTree. measure-fns))
  Reversible
    (rseq [_] (list x)) ; not this because tree ops can't be reversed
  DoubleSeq
    (consl [_ a] (deep (digit measure-fns a)
                       (EmptyTree. measure-fns)
                       (digit measure-fns x)))
    (conjr [_ b] (deep (digit measure-fns x)
                       (EmptyTree. measure-fns)
                       (digit measure-fns b)))
  Measured
    (measure [_] (mes* measure-fns x))
    (measureFns [_] measure-fns) ; not needed?
  Splittable
    (split [this pred iden] (let [e (empty this)] [e x e]))
  Tree
    (app3 [this ts t2] (consl (app3 (empty this) ts t2) x))
    (app3deep [_ ts t1] (conjr (reduce conjr t1 ts) x))
    (measureMore [_] (iden* measure-fns))
    (measurePop [_] (iden* measure-fns)))

(defn newSingleTree [measure-fns x]
  (SingleTree. measure-fns x))

(deftype DelayedTree [tree-ref mval]
  Seqable
    (seq [this] this)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_]) ; not needed?
    (empty [_] (empty @tree-ref))
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] (first @tree-ref))
    (more [_] (rest @tree-ref))
    (next [_] (next @tree-ref))
  IPersistentStack
    (peek [_] (peek @tree-ref))
    (pop [_] (pop @tree-ref))
  Reversible
    (rseq [_] (rseq @tree-ref)) ; not this because tree ops can't be reversed
  DoubleSeq
    (consl [_ a] (consl @tree-ref a))
    (conjr [_ b] (conjr @tree-ref b))
  Measured
    (measure [_] mval)
    (measureFns [_] (measureFns @tree-ref)) ; not needed?
  Splittable
    (split [_ pred iden] (split @tree-ref pred iden))
  Tree
    (app3 [_ ts t2] (app3 @tree-ref ts t2))
    (app3deep [_ ts t1] (app3deep @tree-ref ts t1))
    (measureMore [_] (measureMore @tree-ref))
    (measurePop [_] (measurePop @tree-ref)))

(defmacro ^:private delay-ft [tree-expr mval]
  `(DelayedTree. (delay ~tree-expr) ~mval))
  ;`(let [v# ~mval] (assert v#) ~tree-expr))
  ;`(delayed-ft (delay (do (print "\nforce ") ~tree-expr)) ~mval))

(defn to-tree [measure-fns coll]
  (reduce conjr (EmptyTree. measure-fns) coll))

(defn deep-left [pre m suf]
  (cond
    (seq pre) (deep pre m suf)
    (empty? (first m)) (to-tree (measureFns suf) suf)
    :else (deep (first m)
                (delay-ft (rest m) (measureMore m))
                suf)))

(defn deep-right [pre m suf]
  (cond
    (seq suf) (deep pre m suf)
    (empty? (peek m)) (to-tree (measureFns pre) pre)
    :else (deep pre
                (delay-ft (pop m) (measurePop m))
                (peek m))))

(defn deep [pre m suf]
  (let [measure-fns (measureFns pre)]
    (newDeepTree measure-fns pre m suf
                 (delay (if (seq m)
                          (mes* measure-fns pre m suf)
                          (mes* measure-fns pre suf))))))

(deftype DeepTree [measure-fns pre mid suf mval]
  Seqable
    (seq [this] this)
  IPersistentCollection
    (cons [_ x]) ; TBD
    (count [_]) ; not needed?
    (empty [_] (empty pre))
    (equiv [_ x]) ; TBD
  ISeq
    (first [_] (first pre))
    (more [_] (deep-left (rest pre) mid suf))
    (next [this] (seq (rest this)))
  IPersistentStack
    (peek [_] (peek suf))
    (pop [_] (deep-right pre mid (pop suf)))
  Reversible
    (rseq [this] (lazy-seq (cons (peek this) (rseq (pop this)))))
  DoubleSeq
    (consl [_ a] (if (< (count pre) 4)
                   (deep (consl pre a) mid suf)
                   (let [[b c d e] pre
                         n (digit measure-fns c d e)]
                     (deep (digit measure-fns a b) (consl mid n) suf))))
    (conjr [_ a] (if (< (count suf) 4)
                   (deep pre mid (conjr suf a))
                   (let [[e d c b] suf
                         n (digit measure-fns e d c)]
                     (deep pre (conjr mid n) (digit measure-fns b a)))))
  Measured
    (measure [_] @mval)
    (measureFns [_] (measureFns pre)) ; not needed?
  Splittable
    (split [_ pred iden]
      (let [measure-fns measure-fns
            vpr (red* measure-fns iden (measure pre))]
        (if (pred vpr)
          (let [[sl sx sr] (split pre pred iden)]
            [(to-tree measure-fns sl) sx (deep-left sr mid suf)])
          (let [vm (red* measure-fns vpr (measure mid))]
            (if (pred vm)
              (let [[ml xs mr] (split mid pred vpr)
                    [sl sx sr]
                    (split
                      (apply digit measure-fns xs)
                      pred
                      (red* measure-fns vpr (mes* measure-fns ml)))]
                [(deep-right pre ml sl) sx (deep-left sr mr suf)])
              (let [[sl sx sr] (split suf pred vm)]
                [(deep-right pre mid sl)
                  sx
                  (to-tree measure-fns sr)]))))))
  Tree
    (app3 [this ts t2] (app3deep t2 ts this))
    (app3deep [_ ts t1]
      (deep (.pre ^DeepTree t1)
            (app3 (.mid ^DeepTree t1)
                  (nodes measure-fns (concat (.suf ^DeepTree t1) ts pre))
                  mid)
            suf))
    (measureMore [this] (mes* measure-fns (next pre) mid suf))
    (measurePop  [this] (mes* measure-fns pre mid (pop suf))))

(defn newDeepTree [measure-fns pre mid suf mval]
  (DeepTree. measure-fns pre mid suf mval))

(defn finger-tree [measure-fns & xs]
  (to-tree measure-fns xs))

(defn split-tree [t p]
  (split t p (iden* (measureFns t))))

(defn ft-concat [t1 t2]
  (assert (= (measureFns t1) (measureFns t2))) ;measure-fns must be the same
  (app3 t1 nil t2))

;;=== tests ===

(deftest Conj-Seq-Queue
  (let [len 100]
    (are [x] (= (map identity x) (range len))
      (rseq (reduce consl (finger-tree nil) (range len)))
      (seq  (reduce conjr (finger-tree nil) (range len))))))

(deftest Conj-Seq-Stack
  (let [len 100]
    (are [x] (= (map identity x) (range (dec len) -1 -1))
      (rseq (reduce conjr (finger-tree nil) (range len)))
      (seq  (reduce consl (finger-tree nil) (range len))))))
    
(deftest Conj-Seq-Mixed
  (doseq [m (range 2 7)]
    (loop [ft (finger-tree nil), vc [], i (int 0)]
      (when (< i 40)
        (is (= (seq (map identity ft)) (seq vc)))
        (if (zero? (rem i m))
          (recur (consl ft i) (vec (cons i vc)) (inc i))
          (recur (conjr ft i) (conj vc i)       (inc i)))))))

(deftest Concat
  (doseq [a-len (range 25), b-len (range 25)]
    (let [a-s (map #(symbol (str % 'a)) (range a-len))
          b-s (map #(symbol (str % 'b)) (range b-len))
          a (apply finger-tree nil a-s)
          b (apply finger-tree nil b-s)]
      (is (= (seq (concat a-s b-s)) (seq (map identity (ft-concat a b))))))))

(deftest Annotate-One-Direction
  (let [measure-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (let [len 100]
      (are [x] (= x {:size len :str (apply str (range len))})
        (measure (reduce conjr (finger-tree measure-fns) (range len))))
      (are [x] (= x {:size len :str (apply str (reverse (range len)))})
        (measure (reduce consl (finger-tree measure-fns) (range len)))))))
      
(deftest Annotate-Mixed-Conj
  (let [measure-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree measure-fns), vc [], i (int 0)]
        (when (< i 40)
          (is (= (measure ft) {:size (count vc) :str (apply str vc)}))
          (if (zero? (rem i m))
            (recur (consl ft i) (vec (cons i vc)) (inc i))
            (recur (conjr ft i) (conj vc i)       (inc i))))))))

(deftest Annotate-Concat
  (let [measure-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [a-len (range 25), b-len (range 25)]
      (let [a-s (map #(symbol (str % 'a)) (range a-len))
            b-s (map #(symbol (str % 'b)) (range b-len))
            a (apply finger-tree measure-fns a-s)
            b (apply finger-tree measure-fns b-s)]
        (is (= {:size (+ (count a-s) (count b-s))
                :str (apply str (concat a-s b-s))}
               (measure (ft-concat a b))))))))

(deftest Split
  (let [mfns {:size [(constantly 1) + 0] :str [str str ""]}
        make-item (fn [i] (symbol (str i 'a)))]
    (doseq [len (range 100)
            :let [tree (to-tree mfns (map make-item (range len)))]
            split-i (range len)]
      (is (= (make-item split-i)
             (nth (split-tree tree #(< split-i (:size %))) 1))))))
