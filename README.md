# Finger Tree Quickstart

You'll need git and maven, then execute the following at a shell prompt to fetch finger trees and all its dependencies (including a recent snapshot of Clojure itself) and start a REPL:

    git clone git://github.com/clojure/data.finger-tree.git fingertree
    cd fingertree
    mvn clojure:repl

Then load up the finger-tree lib and start playing with it:

    (use 'clojure.data.finger-tree)
    (apply double-list '[a b c d e f g h i j k l m n o p q])

# Examples

The finger-tree lib actually includes several collections built on top of [Hize and Paterson's finger trees][1].  Here are some examples of each of them:

## double-list

The double-list is a sequential collection that provides constant-time access to both the left and right ends:

    (def dl (double-list 4 5 6 7))

    dl
    ;=> (4 5 6 7)

    [(first dl) (rest dl)]
    ;=> [4 (5 6 7)]

    (consl dl 'x)
    ;=> (x 4 5 6 7)

    [(pop dl) (peek dl)]
    ;=> [(4 5 6) 7]

    (conjr dl 'x)
    ;=> (4 5 6 7 x)

## counted-double-list

This provides all the features of double-list plus constant-time `count` and log-n `nth`:

    (def cdl
      (apply counted-double-list '[a b c d e f g h i j k l m]))

    (nth cdl 5)
    ;=> f

    (assoc cdl 5 'XX)
    ;=> (a b c d e XX g h i j k l m)

    (def parts
      (let [[left _ right] (ft-split-at cdl 5)]
        {:left left, :right right}))

    parts
    ;=> {:left (a b c d e), :right (g h i j k l m)}

    (ft-concat (conjr (:left parts) 'XX) (:right parts))
    ;=> (a b c d e XX g h i j k l m)

    (ft-concat (:left parts) (:right parts))
    ;=> (a b c d e g h i j k l m)
    ;             ^-- missing f

    (ft-concat (into (:left parts) '[X Y Z]) (:right parts))
    ;=> (a b c d e X Y Z g h i j k l m)

## counted-sorted-set

This is like counted-double-list, but does not support `consl` or `conjr`.  Instead, `conj` is used to insert items in sorted order.

    (def css (apply counted-sorted-set
                    '[m j i e d a f k b c f g h l]))
    css
    ;=> (a b c d e f g h i j k l m)

    (get css 'e)      ; O(log(n))
    ;=> e

    (get css 'ee)     ; O(log(n))
    ;=> nil

    (count css)       ; O(1)
    ;=> 13

    (nth css 5)       ; O(log(n))
    ;=> f

[1]: http://www.soi.city.ac.uk/~ross/papers/FingerTree.html
