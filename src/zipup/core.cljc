(ns zipup.core
  (:require [clojure.zip :as zip]))

(def leaf?
  "Returns true if the node at loc is a leaf node"
  (complement zip/branch?))

(defn top?
  "Returns true if the node at loc is the root node"
  [loc]
  (nil? (loc 1)))

(defn root-loc
  "Like clojure.zip/root, but returns the loc of the root node"
  [loc]
  (if (top? loc)
    loc
    (let [l (zip/up loc)]
      (if l (recur l) loc))))

(defn move-nth
  "Applies move-fn to loc n times"
  [move-fn loc n]
  (some->>
    loc
    (iterate move-fn)
    (take (inc n))
    last))

(def prev-nth
  "Applies clojure.zip/prev to loc n times. See [[move-nth]]."
  (partial move-nth zip/prev))

(def next-nth
  "Applies clojure.zip/next to loc n times. See [[move-nth]]."
  (partial move-nth zip/next))

(def left-nth
  "Applies clojure.zip/left to loc n times. See [[move-nth]]."
  (partial move-nth zip/left))

(def right-nth
  "Applies clojure.zip/right to loc n times. See [[move-nth]]."
  (partial move-nth zip/right))

(def up-nth
  "Applies clojure.zip/up to loc n times. See [[move-nth]]."
  (partial move-nth zip/up))

(def down-nth
  "Applies clojure.zip/down to loc n times. See [[move-nth]]."
  (partial move-nth zip/down))

(defn copy-zip
  "Copies the node at loc to the location specified by move-fn, appending the
   copy as a child of the destination node. If replace? is truthy, replaces the
   destination node instead."
	[loc move-fn & {:keys [replace?]}]
	(let [node (zip/node loc)]
		(if replace?
			(-> loc move-fn (zip/replace node))
			(-> loc move-fn zip/up (zip/append-child node)))))

(defn cut-zip
  "Removes nodes visited by applying move-fns to loc"
  [loc & move-fns]
  (->> (reduce (fn [l mf] (zip/replace (mf l) :zipup/cut)) loc move-fns)
       root-loc
       (iterate (fn [l] (if (= :zipup/cut (zip/node l)) (zip/remove l) (zip/next l))))
       (take-while (complement zip/end?))
       last))

(defn map-zip
  "Applies f to every leaf node in the tree rooted at loc"
  [f loc]
  (if (zip/end? loc)
    (root-loc loc)
    (recur f (zip/next (cond-> loc (leaf? loc) (zip/edit f))))))
