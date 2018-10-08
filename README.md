# Finger Trees

Finger trees are a versatile family of fully persistent collections.  This library includes everything you need to make your own, as well a few ready-to-use collection types:

- **double-list** is a sequential collection that provides constant-time access to both the left and right ends.

- **counted-double-list** provides all the features of double-list plus constant-time `count` and log-n `nth`.

- **counted-sorted-set** is sorted set that also provides log-n `nth`

There are examples of all these later in the README.

# Finger Tree Quickstart

## project.clj

If you use leiningen or cake, add this to the `:dependencies` in your `project.clj`:

    [org.clojure/data.finger-tree "0.0.3"]

## pom.xml

If you use maven, add this to the `<dependencies>` in your `pom.xml`:

    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>data.finger-tree</artifactId>
      <version>0.0.3</version>
    </dependency>

You'll need git and maven, then execute the following at a shell prompt to fetch finger trees and all its dependencies (including a recent snapshot of Clojure itself) and start a REPL:

## use/require

Regardless of how you fetch the dependency, to use a finger-tree
function in your project you'll need to add something like this to
your `ns` declaration:

    (:use [clojure.data.finger-tree :only [double-list]])

# Talk

Thanks to heroku for hosting the [slides for my Clojure Conj talk][1] about this library.  The rather raw sources and enormous PDF of the slides are at [github][2].

# Examples

The finger-tree lib actually includes several collections built on top
of [Ralf Hinze and Ross Paterson's finger trees][3].  Here are some
examples of each of them:

## double-list

The double-list is a sequential collection that provides constant-time
access to both the left and right ends:

    (def dl (double-list 4 5 6 7))

    dl
    ;=> (4 5 6 7)

    [(first dl) (rest dl)]
    ;=> [4 (5 6 7)]

    (conjl dl 'x)
    ;=> (x 4 5 6 7)

    [(pop dl) (peek dl)]
    ;=> [(4 5 6) 7]

    (conj dl 'x)
    ;=> (4 5 6 7 x)

## counted-double-list

This provides all the features of double-list plus constant-time
`count` and log-n `nth`:

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

    (ft-concat (conj (:left parts) 'XX) (:right parts))
    ;=> (a b c d e XX g h i j k l m)

    (ft-concat (:left parts) (:right parts))
    ;=> (a b c d e g h i j k l m)
    ;             ^-- missing f

    (ft-concat (into (:left parts) '[X Y Z]) (:right parts))
    ;=> (a b c d e X Y Z g h i j k l m)

## counted-sorted-set

This is like counted-double-list, but does not support `conjl`.  Instead, `conj` is used to insert items in sorted order.

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

## Build-your-own finger tree

    (def empty-cost-tree (finger-tree (meter :cost 0 +)))

    (def ct (conj empty-cost-tree
                  {:id :h, :cost 5} {:id :i, :cost 1}
                  {:id :j, :cost 2} {:id :k, :cost 3}
                  {:id :l, :cost 4}))

    (measured ct)
    ;=> 15

    (next (split-tree ct #(> % 7)))
    ;=> ({:cost 2, :id :j}
         ({:cost 3, :id :k} {:cost 4, :id :l}))

    (next (split-tree (rest ct) #(> % 7)))
    ;=> ({:cost 4, :id :l} ())

[1]: http://talk-finger-tree.heroku.com/
[2]: http://github.com/Chouser/talk-finger-tree
[3]: http://www.soi.city.ac.uk/~ross/papers/FingerTree.html
