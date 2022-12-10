(ns advent-of-code.2022
  (:require [clojure.set :as set]
            [clojure.string :as str]

            [advent-of-code.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 1

(comment
  ; Part A
  (->> (io/inputs "2022_1.txt")
       (partition-by #(= % :new-elf))
       (filter #(not= % [:new-elf]))
       (map #(apply + %))
       (sort (comp - compare))
       first)

  ; Part B
  (->> (io/inputs "2022_1.txt")
       (partition-by #(= % :new-elf))
       (filter #(not= % [:new-elf]))
       (map #(apply + %))
       (sort (comp - compare))
       (take 3)
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 2

(def decode
  {'A :rock
   'B :paper
   'C :scissors
   'X :rock
   'Y :paper
   'Z :scissors})

(def decode-b
  {['A 'X] [:rock :scissors]
   ['A 'Y] [:rock :rock]
   ['A 'Z] [:rock :paper]

   ['B 'X] [:paper :rock]
   ['B 'Y] [:paper :paper]
   ['B 'Z] [:paper :scissors]

   ['C 'X] [:scissors :paper]
   ['C 'Y] [:scissors :scissors]
   ['C 'Z] [:scissors :rock]})

(def scores
  {:rock 1
   :paper 2
   :scissors 3

   [:rock :rock] 3
   [:rock :paper] 6
   [:rock :scissors] 0

   [:paper :rock] 0
   [:paper :paper] 3
   [:paper :scissors] 6

   [:scissors :rock] 6
   [:scissors :paper] 0
   [:scissors :scissors] 3})

(comment
  ; Part A
  (->> (io/inputs "2022_2.txt")
       (map decode)
       (partition 2)
       (map (fn [[elf you :as round]]
              (+ (scores you) (scores round))))
       (apply +))

  ; Part B
  (->> (io/inputs "2022_2.txt")
       (partition 2)
       (map decode-b)
       (map (fn [[elf you :as round]]
              (+ (scores you) (scores round))))
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 2 - arithmetic

(defn round-score [elf-num you-num]
  (-> you-num inc (- elf-num) (mod 3) (* 3)))

(defn score-a [[elf you]]
  (let [elf-num (-> elf name first byte (- (byte \A)))
        you-num (-> you name first byte (- (byte \X)))]
    (+ you-num 1 (round-score elf-num you-num))))

(defn score-b [[elf you]]
  (let [elf-num (-> elf name first byte (- (byte \A)))
        you-num (-> you name first byte (- (byte \Y)) (+ elf-num) (mod 3))]
    (+ you-num 1 (round-score elf-num you-num))))

(comment
  ; Part A
  (->> (io/inputs "2022_2.txt")
       (partition 2)
       (map score-a)
       (apply +))

  ; Part B
  (->> (io/inputs "2022_2.txt")
       (partition 2)
       (map score-b)
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 3

(defn common-letter [letters]
  (let [half (/ (count letters) 2)]
    (first
      (set/intersection (set (take half letters))
                        (set (drop half letters))))))

(defn common-badge [[a b c]]
  (first (set/intersection a b c)))

(defn priority [letter]
  (let [letter-value (byte letter)]
    (if (>= letter-value (byte \a))
      (inc (- letter-value (byte \a)))
      (+ (- letter-value (byte \A)) 27))))

(comment
  ; Part A
  (->> (io/inputs-text "2022_3.txt")
       str/split-lines
       (map (comp priority common-letter seq))
       (apply +))

  ; Part B
  (->> (io/inputs-text "2022_3.txt")
       str/split-lines
       (map (comp set seq))
       (partition 3)
       (map (comp priority common-badge))
       (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 4

(defn parse-line [line]
  (let [[begin-a end-a begin-b end-b]
        (->> line
             (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
             rest
             (map #(Integer/parseInt %)))]
    [(set (range begin-a (inc end-a)))
     (set (range begin-b (inc end-b)))]))

(defn either-subset? [[a b]]
  (or (set/subset? a b)
      (set/subset? b a)))

(defn intersecting? [[a b]]
  (seq (set/intersection a b)))

(comment
  ; Part A
  (->> (io/inputs-text "2022_4.txt")
       str/split-lines
       (map (comp either-subset? parse-line))
       (filter identity)
       count)

  ; Part B
  (->> (io/inputs-text "2022_4.txt")
       str/split-lines
       (map (comp intersecting? parse-line))
       (filter identity)
       count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 5

(defn parse-state-line [line]
  (->> line
       (str \space)
       (re-seq #" *\[[A-Z]\]")
       (map (fn [token]
              (let [[_ spaces crate] (re-find #"( *)\[([A-Z])\]" token)]
                [(-> spaces count dec (/ 4) inc) crate])))
       (reduce (fn [[last-column layer] [columns-left crate]]
                 (let [next-column (+ last-column columns-left)]
                   [next-column (conj layer [next-column crate])]))
               [0 []])
       second))

(defn build-state [state-desc]
  (let [layers (->> (str/split-lines state-desc)
                    reverse
                    rest
                    (map parse-state-line))
        add (fnil conj '())]
    (reduce (fn [state [stack-num crate]]
              (update state stack-num add crate))
            {}
            (apply concat layers))))

;; Added to replace the above two inspired by Peter McLean's
;; python implementation, but without special indexing.
(defn peter-build-state [state-desc]
  (let [stacks (->> (str/split-lines state-desc)
                    (apply interleave)
                    (partition-by #(Character/isUpperCase %))
                    (partition 2)
                    (map second))]
    (zipmap (range 1 (inc (count stacks)))
            stacks)))

(defn parse-instruction [line]
  (->> line
       (re-find #"move (\d+) from (\d) to (\d)")
       rest
       (map #(Integer/parseInt %))))

(defn execute-instruction [state [src dest]]
  (let [crate (first (get state src))]
    (-> state
        (update src #(drop 1 %))
        (update dest conj crate))))

(defn execute-instruction-b [state [cnt src dest]]
  (let [crates (take cnt (get state src))]
    (-> state
        (update src #(drop cnt %))
        (update dest #(concat crates %)))))

(comment
  ; Part A
  (let [[state-desc instructions] (-> (io/inputs-text "2022_5.txt")
                                      (str/split #"\n\n"))
        state (peter-build-state state-desc)]
    (->> (str/split-lines instructions)
         (map parse-instruction)
         (mapcat (fn [[cnt src dest]]
                   (repeat cnt [src dest])))
         (reduce execute-instruction state)
         sort
         (map (comp first second))
         (apply str)))

  ; Part B
  (let [[state-desc instructions] (-> (io/inputs-text "2022_5.txt")
                                      (str/split #"\n\n"))
        state (peter-build-state state-desc)]
    (->> (str/split-lines instructions)
         (map parse-instruction)
         (reduce execute-instruction-b state)
         sort
         (map (comp first second))
         (apply str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 6

(defn find-distinct [n input]
  (->> input
       seq
       (partition n 1)
       (take-while (fn [window]
                     (not= (count (set window)) n)))
       count
       (+ n)))

(comment
  ; Part A
  (find-distinct 4 (io/inputs-text "2022_6.txt"))

  ; Part B
  (find-distinct 14 (io/inputs-text "2022_6.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 7

(defn build-sizes [lines]
  (let [add (fnil + 0)]
    (loop [sizes {} pwd [] [head & tail] lines]
      (case (or (not head) (subs head 0 4))
        "$ cd" (let [pwd' (if (= (subs head 5) "..")
                            (pop pwd)
                            (conj pwd (subs head 5)))]
                 (recur sizes pwd' tail))
        "$ ls" (let [contents (take-while #(not= (first %) \$) tail)
                     files-size (->> contents
                                     (filter #(not= (subs % 0 3) "dir"))
                                     (map (comp #(Long/parseLong %)
                                                first
                                                #(str/split % #" ")))
                                     (apply +))
                     sizes' (->> (range 1 (inc (count pwd)))
                                 (map #(subvec pwd 0 %))
                                 (reduce (fn [s path] (update s path add files-size))
                                         sizes))
                     tail' (drop (count contents) tail)]
                 (recur sizes' pwd tail'))
        true sizes))))

(comment
  ;Part A
  (->> (io/inputs-text "2022_7.txt")
       str/split-lines
       build-sizes
       vals
       (filter #(<= % 100000))
       (apply +))

  ; Part B
  (let [sizes (->> (io/inputs-text "2022_7.txt")
                   str/split-lines
                   build-sizes)
        free (- 70000000 (get sizes ["/"]))
        to_delete (- 30000000 free)]
    (->> sizes
         vals
         (filter #(>= % to_delete))
         (apply min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 8

(defn reflect [xs] (map reverse xs))
(defn transpose [xs] (apply map vector xs))

(defn visible [row]
  (->> row
       (reductions (fn [{:keys [mx]} height]
                     {:mx (max height mx)
                      :vis (< mx height)})
                   {:mx Byte/MIN_VALUE})
       (drop 1)
       (map :vis)))

(defn score-2d [row]
  (letfn [(score-1d [tree trees]
            (reduce (fn [cnt next-tree]
                      (if (< next-tree tree)
                        (inc cnt)
                        (reduced (inc cnt))))
                    0 trees))]
    (loop [left '() [tree & right] row scores []]
      (if tree
        (recur (conj left tree)
               right
               (conj scores (* (score-1d tree left)
                               (score-1d tree right))))
        scores))))

(comment
  ; Part A
  (let [forest (->> (io/inputs-text "2022_8.txt")
                    str/split-lines
                    (map #(map byte %)))
        left (->> forest (map visible) flatten)
        right (->> forest reflect (map visible) reflect flatten)
        top (->> forest transpose (map visible) transpose flatten)
        bottom (->> forest transpose reflect (map visible) reflect transpose flatten)]
    (->> (map (fn [l r t b] (or l r t b)) left right top bottom)
         (filter identity)
         count))

  ; Part B
  (let [forest (->> (io/inputs-text "2022_8.txt")
                    str/split-lines
                    (map #(map byte %)))
        rowwise (->> forest (map score-2d) flatten)
        columnwise (->> forest transpose (map score-2d) transpose flatten)]
    (->> (map * rowwise columnwise)
         (apply max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 9

(defn move [[x y] direction]
  (case direction
    U [x (inc y)]
    D [x (dec y)]
    L [(dec x) y]
    R [(inc x) y]))

(defn tail-moves [[x1 y1] [x2 y2]]
  (case [(- x2 x1) (- y2 y1)]
    [0 2]                     ['U]
    ([1 2] [2 1] [2 2])       ['U 'R]
    [2 0]                     ['R]
    ([2 -1] [1 -2] [2 -2])    ['D 'R]
    [0 -2]                    ['D]
    ([-2 -1] [-1 -2] [-2 -2]) ['D 'L]
    [-2 0]                    ['L]
    ([-2 1] [-1 2] [-2 2])    ['U 'L]
                              []))

(defn step-old [[head tail] direction]
  (let [head' (move head direction)
        tail' (->> (tail-moves tail head')
                   (reduce move tail))]
    [head' tail']))

(defn step [[head & knots] direction]
  (let [head' (move head direction)]
    (loop [head' head' [tail & knots'] knots moved [head']]
      (if tail
        (let [tail' (->> (tail-moves tail head')
                         (reduce move tail))]
          (recur tail' knots' (conj moved tail')))
        moved))))

(defn position-count [n]
  (->> (io/inputs "2022_9.txt")
       (partition 2)
       (mapcat (fn [[direction cnt]]
                 (repeat cnt direction)))
       (reductions step (repeat n [0 0]))
       (map last)
       set
       count))

(comment
  ; Part A
  (position-count 2)

  ; Part B
  (position-count 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 10

(defn register-states [tokens]
  (loop [state 1 states [1] [head & tail] tokens]
    (case head
      noop (recur state (conj states state) tail)
      addx (let [state' (+ state (first tail))]
             (recur state' (into states [state state']) (rest tail)))
      nil states)))

(defn draw [col center]
  (if (< 1 (abs (- center col)))
    "." "#"))

(comment
  ; Part A
  (->> (io/inputs "2022_10.txt")
       register-states
       (drop 19)
       (partition 40 40 nil)
       (map first)
       (take 6)
       (map * (range 20 240 40))
       (apply +))

  ; Part B
  (->> (io/inputs "2022_10.txt")
       register-states
       (partition 40)
       (map #(map draw (range 0 40) %))
       (map #(apply str %))
       (str/join "\n")
       print))
