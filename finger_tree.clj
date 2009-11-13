;(ns clojure.lang.fingertree)
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
(import '(clojure.lang ISeq IPersistentStack Reversible Indexed))
(declare EmptyTree SingleTree DeepTree digit deep)

(defmacro #^{:private true} defdigit [& items]
  (let [i (gensym "i_")
        p (gensym "p_")
        o (gensym "o_")
        typename (symbol (str "Digit" (count items)))
        this-items (map #(list (keyword %) o) items)]
   `(do
      (deftype ~typename [~@items ~'measure-fns ~'measure-ref]
               [ISeq Indexed IPersistentStack]
        ;Seqable:
          (.seq [] (list ~@items))
        ;Indexed
          (.count [] ~(count items)) ; not needed?
          (.nth [~i] (cond ~@(mapcat (fn [sym n] [`(== ~i (int ~n)) sym])
                                     items
                                     (range (count items)))))
        ;IPersistentCollection:
          (.cons [x]) ; TBD
          (.empty []) ; TBD ; not needed?
          (.equiv [x]) ; TBD
        ;ISeq:
          (.first      [] ~(first items))
          (.more       [] ~(if (> (count items) 1)
                            `(digit ~'measure-fns ~@(next items))
                            `(EmptyTree ~'measure-fns)))
          (.next       [] ~(when (> (count items) 1)
                            `(digit ~'measure-fns ~@(next items))))
        ;IPersistentStack
          (.peek       [] ~(last items))
          (.pop        [] ~(if (> (count items) 1)
                            `(digit ~'measure-fns ~@(drop-last items))
                            `(EmptyTree ~'measure-fns))))
      (extend ~(keyword (str *ns*) (str typename))
        ~'DoubleSeq
          {:consl (fn [~o x#] (digit (:measure-fns ~o) x# ~@this-items))
           :conjr (fn [~o x#] (digit (:measure-fns ~o) ~@this-items x#))}
        ~'Measured
          {:measure (fn [o#] @(:measure-ref o#))
           :measureFns (fn [o#] (:measure-fns o#))} ; not needed?
        ~'Splittable
          {:split (fn [~o ~p ~i]
                    ~(letfn [(step [ips [ix & ixs]]
                               (if (empty? ixs)
                                 [(when ips `(digit (:measure-fns ~o) ~@ips))
                                  ix
                                  nil]
                                 `(let [~i (red* (:measure-fns ~o)
                                                 ~i
                                                 (mes* (:measure-fns ~o) ~ix))]
                                    (if (~p ~i)
                                      [~(when ips
                                          `(digit (:measure-fns ~o) ~@ips))
                                       ~ix
                                       (digit (:measure-fns ~o) ~@ixs)]
                                      ~(step (concat ips [ix]) ixs)))))]
                       (step nil this-items)))}))))

(defmacro #^{:private true} make-digit [measure-fns & items]
  (let [typename (symbol (str "Digit" (count items)))]
    `(let [mfns# ~measure-fns]
       (~typename ~@items mfns# (delay (~'mes* mfns# ~@items))))))

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

(deftype EmptyTree [measure-fns] [ISeq IPersistentStack Reversible]
  ;Seqable:
    (.seq [] nil)
  ;IPersistentCollection:
    (.cons [x]) ; TBD
    (.count [] 0) ; not needed?
    (.empty [] this)
    (.equiv [x]) ; TBD
  ;ISeq:
    (.first [] nil)
    (.more [] this)
    (.next [] nil)
  ;IPersistentStack:
    (.peek [] nil)
    (.pop [] this)
  ;Reversible
    (.rseq [] nil))

(extend ::EmptyTree 
  DoubleSeq
    {:consl (fn [s a] (SingleTree (:measure-fns s) a))
     :conjr (fn [s b] (SingleTree (:measure-fns s) b))}
  Measured
    {:measure (fn [o] (iden* (:measure-fns o)))
     :measureFns (fn [o] (:measure-fns o))} ; not needed?
;  Splittable
;    {:split (fn [o pred iden])} ; TBD -- not needed??
  Tree
    {:app3 (fn [t1 ts t2] (reduce consl t2 (reverse ts)))
     :app3deep (fn [t2 ts t1] (reduce conjr t1 ts))
     :measureMore (fn [o] (iden* (:measure-fns o)))
     :measurePop (fn [o] (iden* (:measure-fns o)))})

(deftype SingleTree [measure-fns x] [ISeq IPersistentStack Reversible]
  ;Seqable:
    (.seq [] this)
  ;IPersistentCollection:
    (.cons [x]) ; TBD
    (.count []) ; not needed?
    (.empty [] (EmptyTree measure-fns)) ; not needed?
    (.equiv [x]) ; TBD
  ;ISeq:
    (.first [] x)
    (.more [] (EmptyTree measure-fns))
    (.next [] nil)
  ;IPersistentStack:
    (.peek [] x)
    (.pop [] (EmptyTree measure-fns))
  ;Reversible
    (.rseq [] (list x))) ; not this because tree ops can't be reversed

(extend ::SingleTree 
  DoubleSeq
    {:consl (fn [s a] (deep (digit (:measure-fns s) a)
                            (EmptyTree (:measure-fns s))
                            (digit (:measure-fns s) (:x s))))
     :conjr (fn [s b] (deep (digit (:measure-fns s) (:x s))
                            (EmptyTree (:measure-fns s))
                            (digit (:measure-fns s) b)))}
  Measured
    {:measure (fn [o] (mes* (:measure-fns o) (:x o)))
     :measureFns (fn [o] (:measure-fns o))} ; not needed?
  Splittable
    {:split (fn [o pred iden] (let [e (empty o)] [e (:x o) e]))}
  Tree
    {:app3 (fn [t1 ts t2] (consl (app3 (empty t1) ts t2) (:x t1)))
     :app3deep (fn [t2 ts t1] (conjr (reduce conjr t1 ts) (:x t2)))
     :measureMore (fn [o] (iden* (:measure-fns o)))
     :measurePop (fn [o] (iden* (:measure-fns o)))})

(deftype DelayedTree [tree-ref mval] [ISeq IPersistentStack Reversible]
  ;Seqable:
    (.seq [] this)
  ;IPersistentCollection:
    (.cons [x]) ; TBD
    (.count []) ; not needed?
    (.empty [] (empty @tree-ref))
    (.equiv [x]) ; TBD
  ;ISeq:
    (.first [] (first @tree-ref))
    (.more [] (rest @tree-ref))
    (.next [] (next @tree-ref))
  ;IPersistentStack:
    (.peek [] (peek @tree-ref))
    (.pop [] (pop @tree-ref))
  ;Reversible
    (.rseq [] (rseq @tree-ref))) ; not this because tree ops can't be reversed

(extend ::DelayedTree
  DoubleSeq
    {:consl (fn [s a] (consl @(:tree-ref s) a))
     :conjr (fn [s b] (conjr @(:tree-ref s) b))}
  Measured
    {:measure (fn [o] (:mval o))
     :measureFns (fn [o] (measureFns @(:tree-ref o)))} ; not needed?
  Splittable
    {:split (fn [o pred iden] (split @(:tree-ref o) pred iden))}
  Tree
    {:app3 (fn [t1 ts t2] (app3 @(:tree-ref t1) ts t2))
     :app3deep (fn [t2 ts t1] (app3deep @(:tree-ref t2) ts t1))
     :measureMore (fn [o] (measureMore @(:tree-ref o)))
     :measurePop (fn [o] (measurePop @(:tree-ref o)))})

(defmacro #^{:private true} delay-ft [tree-expr mval]
  `(DelayedTree (delay ~tree-expr) ~mval))
  ;`(let [v# ~mval] (assert v#) ~tree-expr))
  ;`(delayed-ft (delay (do (print "\nforce ") ~tree-expr)) ~mval))

(defn to-tree [measure-fns coll]
    (reduce conjr (EmptyTree measure-fns) coll))

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
  (let [measure-fns (:measure-fns pre)]
    (DeepTree measure-fns pre m suf
              (delay (if (seq m)
                       (mes* measure-fns pre m suf)
                       (mes* measure-fns pre suf))))))

(deftype DeepTree [measure-fns pre mid suf mval]
  [ISeq IPersistentStack Reversible]
  ;Seqable:
    (.seq [] this)
  ;IPersistentCollection:
    (.cons [x]) ; TBD
    (.count []) ; not needed?
    (.empty [] (empty pre))
    (.equiv [x]) ; TBD
  ;ISeq:
    (.first [] (first pre))
    (.more [] (deep-left (rest pre) mid suf))
    (.next [] (seq (rest this)))
  ;IPersistentStack:
    (.peek [] (peek suf))
    (.pop [] (deep-right pre mid (pop suf)))
  ;Reversible
    (.rseq [] (lazy-seq (cons (peek this) (rseq (pop this))))))

(extend ::DeepTree
  DoubleSeq
    {:consl (fn [s a] (if (< (count (:pre s)) 4)
                        (deep (consl (:pre s) a) (:mid s) (:suf s))
                        (let [[b c d e] (:pre s)
                              n (digit (:measure-fns s) c d e)]
                          (deep (digit (:measure-fns s) a b)
                                (consl (:mid s) n)
                                (:suf s)))))
     :conjr (fn [s a] (if (< (count (:suf s)) 4)
                        (deep (:pre s) (:mid s) (conjr (:suf s) a))
                        (let [[e d c b] (:suf s)
                              n (digit (:measure-fns s) e d c)]
                          (deep (:pre s)
                                (conjr (:mid s) n)
                                (digit (:measure-fns s) b a)))))}
  Measured
    {:measure (fn [o] @(:mval o))
     :measureFns (fn [o] (:measure-fns (:pre o)))} ; not needed?
  Splittable
    {:split (fn [o pred iden]
              (let [pre (:pre o)
                    mid (:mid o)
                    suf (:suf o)
                    measure-fns (:measure-fns o)
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
                         (to-tree measure-fns sr)]))))))}
  Tree
    {:app3 (fn [t1 ts t2] (app3deep t2 ts t1))
     :app3deep (fn [t2 ts t1]
                 (deep (:pre t1)
                       (app3 (:mid t1)
                             (nodes (:measure-fns t2)
                                    (concat (:suf t1) ts (:pre t2)))
                             (:mid t2))
                       (:suf t2)))
     :measureMore (fn [o] (mes* (:measure-fns o)
                                (next (:pre o)) (:mid o) (:suf o)))
     :measurePop  (fn [o] (mes* (:measure-fns o)
                                (:pre o) (:mid o) (pop (:suf o))))})

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
