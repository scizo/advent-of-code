(ns advent-of-code.2022
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as set]
            [clojure.string :as str]

            [advent-of-code.io :as io]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defmacro fn-> [& body]
  `(fn [arg#] (-> arg# ~@body)))

(defmacro fn->> [& body]
  `(fn [arg#] (->> arg# ~@body)))

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
    ([1 2] [2 2] [2 1])       ['U 'R]
    [2 0]                     ['R]
    ([2 -1] [2 -2] [1 -2])    ['D 'R]
    [0 -2]                    ['D]
    ([-1 -2] [-2 -2] [-2 -1]) ['D 'L]
    [-2 0]                    ['L]
    ([-2 1] [-2 2] [-1 2])    ['U 'L]
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
  (= (position-count 2) 6271)

  ; Part B
  (= (position-count 10) 2458))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 11

(def monkey-pattern
  #"\n*Monkey (\d+):\n  Starting items: (\d+(?:, \d+)*)\n  Operation: new = old ([\+\*]) (\d+|old)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d+)\n    If false: throw to monkey (\d+)")

(defn monkey [items inspect test]
  (let [[test t f] test]
    {:items items
     :inspect inspect
     :next-monkey #(if (zero? (mod % test)) t f)
     :test test
     :inspected 0}))

(defn inspect-fn [operator operand]
  (eval `(fn [~'old] (~(symbol operator) ~'old ~(read-string operand)))))

(def monkeys
  (->> (io/inputs-text "2022_11.txt")
       (re-seq monkey-pattern)
       (mapv (fn [[_ m-num items operator operand divisor true-m false-m]]
               (monkey (io/inputs-from-string items)
                       (inspect-fn operator operand)
                       [(read-string divisor)
                        (read-string true-m)
                        (read-string false-m)])))))

(defn execute-monkey [worry-reducer monkeys monkey-num]
  (let [{:keys [items inspect next-monkey] :as monkey} (get monkeys monkey-num)
        monkey' (-> monkey
                    (assoc :items [])
                    (update :inspected + (count items)))]
    (reduce (fn [ms item]
              (let [worry (-> item inspect worry-reducer)]
                (update-in ms [(next-monkey worry) :items] conj worry)))
            (assoc monkeys monkey-num monkey')
            items)))

(comment
  ; Part A
  (->> (range 0 8)
       (repeat 20)
       flatten
       (reduce (partial execute-monkey #(quot % 3)) monkeys)
       (sort-by :inspected (comp - compare))
       (take 2)
       (map :inspected)
       (apply *))

  ; Part B
  (let [worry-reducer (apply * (map :test monkeys))]
    (->> (range 0 8)
         (repeat 10000)
         flatten
         (reduce (partial execute-monkey #(mod % worry-reducer)) monkeys)
         (sort-by :inspected (comp - compare))
         (take 2)
         (map :inspected)
         (apply *))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 12

(def max-value Long/MAX_VALUE)

(defn distance [[y1 x1] [y2 x2]]
  (+ (abs (- y2 y1)) (abs (- x2 x1))))

(defn next-moves [grid [y x :as location]]
  (let [current (get-in grid location)
        valid-elevation? (fn [n] (<= (- (or n max-value) current) 1))]
    (->> [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]
         (filter #(valid-elevation? (get-in grid %))))))

(defn a* [grid start end]
  (loop [[scores to-explore] [{start 0} (pm/priority-map start [(distance start end) 0])]]
    (let [[current _] (peek to-explore)
          current-score (scores current)]
      (cond
        (= current end) current-score
        (empty? to-explore) max-value
        :else
        (recur
          (reduce (fn [[s e] n]
                    (let [new-score (inc current-score)]
                      (if (< new-score (scores n max-value))
                        [(assoc s n new-score) (assoc e n [(+ new-score (distance n end)) (- new-score)])]
                        [s e])))
                  [scores (pop to-explore)]
                  (next-moves grid current)))))))

(defn coord->index [[y x]]
  (+ (* y 61) x))

(defn reversed-edges [grid]
  (let [m (count grid)
        n (count (first grid))]
    (for [y (range m) x (range n) [y' x'] (next-moves grid [y x])]
      [(coord->index [y' x']) (coord->index [y x])])))

(defn bellman-ford [edges source]
  (let [node-cnt (-> edges flatten set count)]
    (loop [dist (assoc (vec (repeat node-cnt (dec max-value))) source 0)
           [[u v] & tail] (apply concat (repeat (dec node-cnt) edges))]
      (if u
        (let [dist-u (dist u) dist-v (dist v)]
          (if (< (inc dist-u) dist-v)
            (recur (assoc dist v (inc dist-u)) tail)
            (recur dist tail)))
        dist))))

(comment
  ; Part A
  (let [grid (->> (io/inputs-text "2022_12.txt")
                  str/split-lines
                  (mapv (comp vec #(map byte %))))]
    (a* grid [20 0] [20 36]))

  ; Part B
  (let [grid (->> (io/inputs-text "2022_12.txt")
                  str/split-lines
                  (mapv (comp vec #(map byte %))))]
    (->> (for [y (range 0 41) x (range 0 61)
               :when (= (get-in grid [y x]) 97)]
           [y x])
         (map #(a* grid % [20 36]))
         (apply min)))

  ; Part A & B bellman-ford
  (let [grid (->> (io/inputs-text "2022_12.txt")
                  str/split-lines
                  (mapv (comp vec #(map byte %))))
        shortest (-> grid reversed-edges (bellman-ford (coord->index [20 36])))
        as (for [y (range 0 41) x (range 0 61)
                 :when (= (get-in grid [y x]) 97)]
             (coord->index [y x]))]
    [(shortest (coord->index [20 0]))
     (->> as (map shortest) (apply min))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 13

(defn coerce-list [o]
  (if (instance? clojure.lang.Sequential o) o [o]))

(defn check [[l & lrest] [r & rrest]]
  (cond
    (and (integer? l) (integer? r))
    (cond
      (< l r) -1
      (= l r) (check lrest rrest)
      :else 1)

    (and (nil? l) (not (nil? r)))
    -1

    (and (nil? l) (nil? r))
    0

    (and (not (nil? l)) (nil? r))
    1

    :else
    (let [as-list (check (coerce-list l) (coerce-list r))]
      (if (zero? as-list)
        (check lrest rrest)
        as-list))))

(comment
  ; Part A
  (->> (io/inputs "2022_13.txt")
       (partition 2)
       (map-indexed (fn [i [l r]]
                      (if (neg? (check l r)) (inc i) 0)))
       (apply +))

  ; Part B
  (->> (io/inputs "2022_13.txt")
       (concat [[[2]] [[6]]])
       (sort check)
       (map-indexed (fn [i p] [(inc i) p]))
       (filter (fn [[_ p]] (#{[[2]] [[6]]} p)))
       (map first)
       (apply *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 14

(defn next-sand [m max-y [x y :as coord]]
  (let [steps [[0 1] [-1 1] [1 1]]
        [x' y' :as coord'] (first (for [[dx dy] steps
                                        :let [x' (+ x dx) y' (+ y dy)]
                                        :when (not (get m [x' y']))]
                                    [x' y']))]
    (cond
      (not coord') coord
      (= y' max-y) coord'
      :else (recur m max-y coord'))))

(defn dimensions [m]
  (let [xs (map first (keys m))
        min-x (apply min xs)
        max-x (apply max xs)
        max-y (->> (keys m) (map second) (apply max))]
    [min-x max-x 0 max-y]))

(defn draw [m]
  (let [[min-x max-x min-y max-y] (dimensions m)]
    (->> (for [y (range (inc max-y))]
           (map #(case (get m [% y])
                   :rock "#"
                   :sand "o"
                   nil ".")
                (range min-x (inc max-x))))
         (map #(apply str %))
         (str/join "\n")
         (format "%d\n%s" min-x))))

(defn norm [[x1 y1] [x2 y2]]
  [(if (= x1 x2) 0 (/ (- x2 x1) (abs (- x2 x1))))
   (if (= y1 y2) 0 (/ (- y2 y1) (abs (- y2 y1))))])

(defn add-row [m [a b & tail]]
  (if b
    (let [[dx dy] (norm a b)
          rocks (->> (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) a)
                     (take-while #(not= % b)))]
      (recur (reduce (fn [m' coord]
                       (assoc m' coord :rock))
                     m (conj rocks b))
             (conj tail b)))
    m))

(defn parse-scan [input-name]
  (->> (io/inputs-by-line input-name)
       (map (fn->> (filter integer?) (partition 2)))
       (reduce add-row {})))

(comment
  ; Part A
  (let [state (parse-scan "2022_14.txt")
        [min-x max-x min-y max-y] (dimensions state)]
    (loop [m state num-added 0]
      (let [[x y :as coord] (next-sand m max-y [500 0])
            m' (assoc m coord :sand)]
        (if (= y max-y)
          num-added
          (recur m' (inc num-added))))))

  ; Part B
  (let [scan (parse-scan "2022_14.txt")
        [min-x max-x min-y max-y] (dimensions scan)
        state (add-row scan [[(- min-x (* 2 max-y)) (+ max-y 2)]
                               [(+ max-x (* 2 max-y)) (+ max-y 2)]])]
    (loop [m state num-added 0]
      (let [[x y :as coord] (next-sand m (+ max-y 2) [500 0])
            m' (assoc m coord :sand)]
        (if (= coord [500 0])
          (inc num-added)
          (recur m' (inc num-added)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2022 Day 15

(def sensor-pattern
  #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn touching? [[mn-a mx-a] [mn-b mx-b]]
  (and (<= mn-a (inc mx-b)) (<= mn-b (inc mx-a))))

;(defn combine [rngs]
;  (loop [checked [] [head & tail] rngs]
;    (if (seq tail)
;      (let [touching (group-by #(touching? head %) tail)]
;        (if (seq (touching true))
;          (recur)
;          (recur (conj checked head) tail)
;          ))
;      )
;    (if (and (<= mn-a (inc mx-a)) (<= mn-b (inc mx-a)))
;      (recur checked (conj tail [(min mn-a mn-b) (max mx-a mx-b)])))))

(defn range-remove [rngs [mn-d mx-d]]
  (mapcat (fn [[mn mx]]
            (if (and (<= mn mx-d) (<= mn-d mx))
              (filter identity [(when (< mn mn-d) [mn (dec mn-d)])
                                (when (< mx-d mx) [(inc mx-d) mx])])
              [[mn mx]]))
          rngs))

(defn into! [c s] (reduce conj! c s))

(comment
  ; Part A
  (time
    (let [sensor-beacons (->> (io/inputs-text "2022_15.txt")
                              (re-seq sensor-pattern)
                              (map (fn->> (drop 1) (map read-string) (partition 2))))
          searched (->> sensor-beacons
                        (keep (fn [[[sx sy :as s] [bx by :as b]]]
                                (let [sensor-range (distance s b)
                                      dist (abs (- 2000000 sy))
                                      extra (- sensor-range dist)]
                                  (when (<= 0 extra)
                                    (set (range (- sx extra) (+ sx extra 1)))))))
                        (reduce into #{})
                    )
          beacons (->> sensor-beacons
                       (keep (fn [[_ [bx by]]]
                               (when (= by 2000000) bx)))
                       set)]
      (count (set/difference searched beacons))))

  ; Part B
  (time
    (let [min-value 0
          max-value 4000000
          sensor-beacons (->> (io/inputs-text "2022_15.txt")
                              (re-seq sensor-pattern)
                              (map (fn->> (drop 1) (map read-string) (partition 2))))
          r2 (vec (repeat (inc max-value) [[min-value max-value]]))
          searched (->> sensor-beacons
                        (mapcat (fn [[[sx sy :as s] [bx by :as b]]]
                                  (let [sensor-range (distance s b)]
                                    (->> (for [dy (range (- sensor-range) (inc sensor-range))
                                               :let [y' (+ sy dy)]
                                               :when (<= min-value y' max-value)]
                                           [y' [(max min-value (- sx (- sensor-range (abs dy))))
                                                (min max-value (+ sx (- sensor-range (abs dy))))]]))))))
          [[y [[x _]]]] (->> searched
                             (reduce (fn [r2 [y rng]]
                                       (update r2 y range-remove rng))
                                     r2)
                             (keep-indexed (fn [i rngs] (when (seq rngs) [i rngs]))))]
      (+ (* x max-value) y)))

  ; Part B - faster w/transducer
  (time
    (let [min-value 0
          max-value 4000000
          sensor-beacons (->> (io/inputs-text "2022_15.txt")
                              (re-seq sensor-pattern)
                              (map (fn->> (drop 1)
                                          (map read-string)
                                          ((fn [[sx sy bx by]]
                                             [sx sy (distance [sx sy] [bx by]) bx by])))))
          xf (comp (map (fn [row]
                          (keep (fn [[sx sy sr _ _]]
                                  (let [dist (abs (- sy row))
                                        half-width (- sr dist)]
                                    (when (<= dist sr)
                                      [(max min-value (- sx half-width))
                                       (min max-value (+ sx half-width))])))
                                sensor-beacons)))
                   (keep-indexed (fn [row rngs]
                                   (reduce (fn [n [mn mx]]
                                             (cond
                                               (< n mn) (reduced [n row])
                                               (= n (inc max-value)) (reduced nil)
                                               :else (max n (inc mx))))
                                           0 (sort rngs))))
                   (take 1))
          [x y] (->> (range max-value (dec min-value) -1)
                     (into [] xf)
                     first)]
      (+ (* x max-value) y)))
  )
