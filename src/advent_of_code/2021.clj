(ns advent-of-code.2021
  (:require [advent-of-code.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2021 Day 1

(comment
  ; Part A
  (->> (io/inputs "2021_1.txt")
       (partition 2 1)
       (map #(apply < %))
       (filter identity)
       count)

  ; Part B
  (->> (io/inputs "2021_1.txt")
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (map #(apply < %))
       (filter identity)
       count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2021 Day 2

(comment
  ; Part A
  (let [{:keys [horizontal depth]}
        (->> (io/inputs "2021_2.txt")
             (partition 2)
             (reduce
               (fn [state [direction distance]]
                 (case direction
                   forward (update state :horizontal + distance)
                   up (update state :depth - distance)
                   down (update state :depth + distance)))
               {:horizontal 0 :depth 0}))]
    (* horizontal depth))

  ; Part B
  (let [{:keys [horizontal depth]}
        (->> (io/inputs "2021_2.txt")
             (partition 2)
             (reduce
               (fn [state [direction distance]]
                 (case direction
                   up (update state :aim - distance)
                   down (update state :aim + distance)
                   forward (-> state
                               (update :horizontal + distance)
                               (update :depth + (* (:aim state) distance)))))
               {:horizontal 0 :depth 0 :aim 0}))]
    (* horizontal depth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2021 Day 3

(defn bit-one? [x m]
  (->> (bit-shift-left 1 m) (bit-and x) zero? not))

(defn bits [n]
  (fn [x]
    (reduce
      (fn [bs m]
        (assoc bs m (bit-one? x m)))
      {}
      (range n))))

(defn find-number [numbers selector-fn bit-order]
  (reduce
    (fn [numbers' bit-number]
      (let [ones (map #(bit-one? % bit-number) numbers')
            cnt (count numbers')
            cnt-ones (->> ones (filter identity) count)
            cnt-zeros (- cnt cnt-ones)
            selector (selector-fn cnt-zeros cnt-ones)
            selected (filter #(selector (bit-one? % bit-number)) numbers')]
        (if (> (count selected) 1)
          selected
          (reduced (first selected)))))
    numbers
    bit-order))

(comment
  ; Part A
  (let [bs (->> (io/inputs "2021_3.txt")
                (map (bits 12)))
        combined (apply merge-with
                        (fn [a b] (if (vector? a)
                                    (conj a b)
                                    [a b]))
                        bs)
        cnt (count bs)
        {:keys [gamma epsilon]}
        (reduce-kv
          (fn [state bit-number results]
            (let [ones (->> results (filter identity) count)
                  k (if (< ones (- cnt ones))
                      :epsilon
                      :gamma)]
              (update state k + (bit-shift-left 1 bit-number))))
          {:gamma 0 :epsilon 0}
          combined)]
    (* gamma epsilon))

  ; Part B
  (let [diagnostics (io/inputs "2021_3.txt")
        o2-selector (fn [cnt-zeros cnt-ones]
                      (if (>= cnt-ones cnt-zeros) true? false?))
        co2-selector (fn [cnt-zeros cnt-ones]
                       (if (<= cnt-zeros cnt-ones) false? true?))
        o2 (find-number diagnostics o2-selector (range 11 -1 -1))
        co2 (find-number diagnostics co2-selector (range 11 -1 -1))]
    (* o2 co2)))
