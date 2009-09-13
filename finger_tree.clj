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

(use 'clojure.test)

(gen-interface
  :name clojure.lang.IDoubleSeq
  :extends [clojure.lang.IPersistentCollection
            clojure.lang.ISeq
            clojure.lang.IPersistentStack
            clojure.lang.Reversible]
  :methods [[consLeft  [Object] clojure.lang.IDoubleSeq]
            [consRight [Object] clojure.lang.IDoubleSeq]])

(gen-interface
  :name clojure.lang.fingertree.Measured
  :methods [[measure [] Object]
            [measureFns [] clojure.lang.IPersistentCollection]])

(gen-interface
  :name clojure.lang.fingertree.ISplit
  :extends [clojure.lang.fingertree.Measured]
  :methods [[split [clojure.lang.IFn Object] clojure.lang.IPersistentVector]])

(gen-interface
  :name clojure.lang.fingertree.IDigit
  :extends [clojure.lang.IDoubleSeq
            clojure.lang.Indexed
            clojure.lang.fingertree.ISplit])

(gen-interface
  :name clojure.lang.fingertree.INode
  :extends [clojure.lang.fingertree.Measured clojure.lang.Seqable])

(gen-interface
  :name clojure.lang.fingertree.ITree
  :extends [clojure.lang.IDoubleSeq
            clojure.lang.fingertree.ISplit]
  :methods [[app3 [clojure.lang.ISeq clojure.lang.fingertree.ITree]
                  clojure.lang.fingertree.ITree]
            [app3deep [clojure.lang.ISeq clojure.lang.fingertree.IDeepTree]
                      clojure.lang.fingertree.ITree]
            [measureMore [] Object]
            [measurePop [] Object]])

(gen-interface
  :name clojure.lang.fingertree.IDeepTree
  :extends [clojure.lang.IPersistentCollection
            clojure.lang.fingertree.ITree]
  :methods [[pre [] clojure.lang.fingertree.IDigit]
            [mid [] clojure.lang.fingertree.ITree]
            [suf [] clojure.lang.fingertree.IDigit]])


;(gen-interface
;  :name clojure.lang.IMeasureFn
;  :methods [[iden [] Object]
;            [measure [Object] Object]
;            [reduce [Object Object] Object]])

(gen-interface
  :name clojure.lang.IPrintable
  :methods [[print [java.io.Writer] Object]])

(import '(clojure.lang ISeq IDoubleSeq IPersistentCollection IPrintable)
        '(clojure.lang.fingertree Measured ISplit IDigit INode ITree IDeepTree))

;(defmethod print-method IPrintable [x w] (.print #^IPrintable x w))
;(prefer-method print-method IPrintable ISeq)

(defmacro #^{:private true} delay-ft [tree-expr mval]
  `(~'delayed-ft (delay ~tree-expr) ~mval))
  ;`(delayed-ft (delay (do (print "\nforce ") ~tree-expr)) ~mval))

(letfn
 [(iden* [measure-fns]
    (into measure-fns (for [[k [_ _ iden]] measure-fns] [k iden])))

  (mes* [measure-fns & xs]
    (into measure-fns
          (if (instance? Measured (first xs))
            (let [mes-maps (map #(if %
                                  (.measure #^Measured %)
                                  (iden* measure-fns))
                                xs)]
              (for [[k [mes red]] measure-fns]
                [k (reduce red (map k mes-maps))]))
            (for [[k [mes red]] measure-fns]
              [k (reduce red (map mes xs))]))))

  (red* [measure-fns v1 v2]
    (zipmap (keys measure-fns)
            (for [[k [mes red]] measure-fns]
              (red (k v1) (k v2)))))

  (consl [t a] (.consLeft  #^IDoubleSeq t a))
  (conjr [t a] (.consRight #^IDoubleSeq t a))

  (node
    ([measure-fns a b]
      (let [mval (mes* measure-fns a b)
            lst (list a b)]
        (new [INode IPrintable] this
          (measure    []  mval)
          (measureFns []  measure-fns)
          (seq        []  lst)
          (toString   []  (str lst mval))
          (print      [w] (.write w (str "#<node " this ">"))))))
    ([measure-fns a b c]
      (let [mval (mes* measure-fns a b c)
            lst (list a b c)]
        (new [INode IPrintable] this
          (measure    []  mval)
          (measureFns []  measure-fns)
          (seq        []  lst)
          (toString   []  (str lst mval))
          (print      [w] (.write w (str "#<node " this ">")))))))

  (digit [measure-fns & xs]
    (assert (<= 1 (count xs) 4))
    (let [xs-vec (vec xs)]
      (new [IDigit IPrintable] this
        (consLeft  [x] (apply digit measure-fns x xs-vec))
        (consRight [x] (apply digit measure-fns (conj xs-vec x)))
        (measure   []  (apply mes* measure-fns xs-vec))
        (measureFns[]  measure-fns)
        (split     [p i] (loop [i i, l [], [x & xs] xs-vec]
                          (let [i* (red* measure-fns i (mes* measure-fns x))]
                            (if (p i*)
                              [(when (seq l) (apply digit measure-fns l))
                               x
                               (when xs (apply digit measure-fns xs))]
                              (recur i* (conj l x) xs)))))
        (nth       [i] (nth xs-vec i))
        (count     []  (count xs-vec))
        (seq       []  (seq xs-vec))
        (first     []  (nth xs-vec 0))
        (more      []  (if (> (count xs-vec) 1)
                        (apply digit measure-fns (next xs-vec))
                        (empty-ft measure-fns)))
        (next      []  (when (> (count xs-vec) 1)
                        (apply digit measure-fns (next xs-vec))))
        (peek      []  (peek xs-vec))
        (pop       []  (if (> (count xs-vec) 1)
                        (apply digit measure-fns (pop xs-vec))
                        (empty-ft measure-fns)))
        (toString  []  (str xs-vec))
        (print     [w] (.write w (str "#<digit " this ">"))))))

  (nodes
    ([mfns a b]          (list (node mfns a b)))
    ([mfns a b c]        (list (node mfns a b c)))
    ([mfns a b c d]      (list (node mfns a b) (node mfns c d)))
    ([mfns a b c d & xs] (lazy-seq  ; lazy to avoid stack overflow
                          (cons (node mfns a b c) (apply nodes mfns d xs)))))

  (empty-ft [measure-fns]
    (new [ISeq ITree IPrintable] this
      (consLeft    [a] (single measure-fns a))
      (consRight   [b] (single measure-fns b))
      (app3        [ts t2] (reduce consl t2 (reverse ts)))
      (app3deep    [ts t1] (reduce conjr t1 ts))
      (measure     [] (iden* measure-fns))
      (measureFns  [] measure-fns)
      (measureMore [] (iden* measure-fns))
      (measurePop  [] (iden* measure-fns))
      (count       [] 0)
      (seq         [] nil)
      (rseq        [] nil)
      (first       [] nil)
      (more        [] this)
      (next        [] nil)
      (peek        [] nil)
      (pop         [] this)
      (toString    [] (str (keys measure-fns)))
      (print       [w] (.write w (str "#<empty " this ">")))))

  (single [measure-fns x]
    (new [ITree IPrintable] this
      (consLeft  [a] (deep (digit measure-fns a)
                          (empty-ft measure-fns)
                          (digit measure-fns x)))
      (consRight [b] (deep (digit measure-fns x)
                          (empty-ft measure-fns)
                          (digit measure-fns b)))
      (app3     [ts t2] (consl (.app3 #^ITree (empty-ft measure-fns) ts t2) x))
      (app3deep [ts t1] (conjr (reduce conjr t1 ts) x))
      (measure     [] (mes* measure-fns x))
      (measureFns  [] measure-fns)
      (measureMore [] (iden* measure-fns))
      (measurePop  [] (iden* measure-fns))
      (split       [p i] (let [e (empty-ft measure-fns)] [e x e]))
      (seq         [] this)
      (rseq        [] (list x))
      (first       [] x)
      (more        [] (empty-ft measure-fns))
      (next        [] nil)
      (peek        [] x)
      (pop         [] (empty-ft measure-fns))
      (toString    [] (str x " " (.measure #^ITree this)))
      (print [w]
        (binding [*out* w]
          (print "#<single ")
          (pr x)
          (pr (.measure #^ITree this))
          (print ">")))))

  (delayed-ft [tree-ref mval]
    (new [ITree IPrintable] this
      (consLeft    [a] (.consLeft  #^ITree @tree-ref a))
      (consRight   [b] (.consRight #^ITree @tree-ref b))
      (app3        [ts t2] (.app3  #^ITree @tree-ref ts t2))
      (app3deep    [ts t1] (.app3deep #^ITree @tree-ref ts t1))
      (measure     []  mval)
      (measureFns  []  (.measureFns  #^ITree @tree-ref))
      (measureMore []  (.measureMore #^ITree @tree-ref))
      (measurePop  []  (.measurePop  #^ITree @tree-ref))
      (split       [p i] (.split #^ITree @tree-ref p i))
      (seq         []  this)
      (rseq        []  this)
      (first       []  (.first #^ITree @tree-ref))
      (more        []  (.more  #^ITree @tree-ref))
      (next        []  (.next  #^ITree @tree-ref))
      (peek        []  (.peek  #^ITree @tree-ref))
      (pop         []  (.pop   #^ITree @tree-ref))
      (toString    []  (.toString #^ITree @tree-ref))
      (print [w]
        (binding [*out* w]
          (print "#<delay ")
          (pr @tree-ref)
          (print ">")))))

  (to-tree [measure-fns coll]
    (reduce conjr (empty-ft measure-fns) coll))

  (deep-left [pre, #^ITree m, #^IDigit suf]
    (cond
      (seq pre) (deep pre m suf)
      (empty? (.first m)) (to-tree (.measureFns suf) suf)
      :else (deep (apply digit (.measureFns m) (.first m))
                  (delay-ft (.more m) (.measureMore m))
                  suf)))

  (deep-right [#^IDigit pre, #^ITree m, suf]
    (cond
      (seq suf) (deep pre m suf)
      (empty? (.peek m)) (to-tree (.measureFns pre) pre)
      :else (deep pre
                  (delay-ft (.pop m) (.measurePop m))
                  (apply digit (.measureFns m) (.peek m)))))

  (deep [#^IDigit pre, #^ITree m, #^IDigit suf]
    ;(print "\ndeep ")
    (assert (= (.measureFns pre) (.measureFns suf)))
    (let [measure-fns (.measureFns pre)
          mval (if (.seq m)
                (mes* measure-fns pre m suf)
                (mes* measure-fns pre suf))]
      (new [IDeepTree IPrintable] this
        (pre [] pre)
        (mid [] m)
        (suf [] suf)
        (consLeft  [a] (if (< (count pre) 4)
                        (deep (.consLeft pre a) m suf)
                        (let [[b c d e] pre
                              n (node measure-fns c d e)]
                          (deep (digit measure-fns a b) (.consLeft m n) suf))))
        (consRight [a] (if (< (count suf) 4)
                        (deep pre m (conjr suf a))
                        (let [[e d c b] suf
                              n (node measure-fns e d c)]
                          (deep pre (conjr m n) (digit measure-fns b a)))))
        (measureMore [] (mes* measure-fns (next pre) m suf))
        (measurePop  [] (mes* measure-fns pre m (pop suf)))
        (app3      [ts t2] (.app3deep t2 ts this))
        (app3deep  [ts t1] (let [t2 #^IDeepTree this]
                            (deep (.pre t1)
                                  (.app3 (.mid t1)
                                          (apply nodes measure-fns
                                                (concat (.suf t1) ts (.pre t2)))
                                          (.mid t2))
                                  (.suf t2))))
        (measure     [] mval)
        (measureFns  [] measure-fns)
        (split [p i]
          (let [vpr (red* measure-fns i (.measure pre))
                vm  (red* measure-fns vpr (.measure m))]
            (cond
              (p vpr) (let [[sl sx sr] (.split pre p i)]
                        [(to-tree measure-fns sl) sx (deep-left sr m suf)])
              (p vm) (let [[ml xs mr] (.split m p vpr)
                          [sl sx sr]
                            (.split
                              #^IDigit (apply digit measure-fns xs)
                              p
                              (red* measure-fns vpr (mes* measure-fns ml)))]
                      [(deep-right pre ml sl) sx (deep-left sr mr suf)])
              :else (let [[sl sx sr] (.split suf p vm)]
                      [(deep-right pre m sl) sx (to-tree measure-fns sr)]))))
        (seq       []  this)
        (rseq      []  (lazy-seq (cons (.peek #^IDeepTree this)
                                      (rseq (.pop #^IDeepTree this)))))
        (first     []  (.first pre))
        (more      []  (deep-left (.more pre) m suf))
        (next      []  (.seq (.more #^IDeepTree this)))
        (peek      []  (.peek suf))
        (pop       []  (deep-right pre m (.pop suf)))
        (toString  []  "deep-finger-tree")
        (print     [w]
          (binding [*out* w]
            (print "#<deep ")
            (pr pre m suf)
            (print " ")
            (pr (.measure #^IDeepTree this))
            (print ">"))))))]

 (defn #^ITree finger-tree [measure-fns & xs]
   (to-tree measure-fns xs))

 (defn split-tree [#^ISplit t, p]
   (.split t p (iden* (.measureFns t))))

 (defn #^ITree ft-concat [#^ITree t1, #^ITree t2]
   (assert (= (.measureFns t1) (.measureFns t2))) ;measure-fns must be the same
   (.app3 t1 nil t2))

 (def consl consl)
 (def conjr conjr)
 (def to-tree to-tree)
 (def empty-ft empty-ft))

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
        (.measure (reduce conjr (finger-tree measure-fns) (range len))))
      (are [x] (= x {:size len :str (apply str (reverse (range len)))})
        (.measure (reduce consl (finger-tree measure-fns) (range len)))))))
      
(deftest Annotate-Mixed-Conj
  (let [measure-fns {:size [(constantly 1) + 0] :str [str str ""]}]
    (doseq [m (range 2 7)]
      (loop [ft (finger-tree measure-fns), vc [], i (int 0)]
        (when (< i 40)
          (is (= (.measure ft) {:size (count vc) :str (apply str vc)}))
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
               (.measure (ft-concat a b))))))))

(deftest Split
  (let [mfns {:size [(constantly 1) + 0] :str [str str ""]}
        make-item (fn [i] (symbol (str i 'a)))]
    (doseq [len (range 100)
            :let [tree (to-tree mfns (map make-item (range len)))]
            split-i (range len)]
      (is (= (make-item split-i)
             (nth (split-tree tree #(< split-i (:size %))) 1))))))
