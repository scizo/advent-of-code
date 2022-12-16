(ns advent-of-code.io
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io PushbackReader]))

(set! *warn-on-reflection* true)

(defn inputs-from-string [string]
  (let [reader (-> (char-array string)
                   io/reader
                   PushbackReader.)]
    (->> #(read {:eof ::eof} reader)
         repeatedly
         (take-while #(not= % ::eof)))))

(defn inputs [name]
  (let [reader (-> (io/resource name)
                   io/reader
                   PushbackReader.)]
    (->> #(read {:eof ::eof} reader)
         repeatedly
         (take-while #(not= % ::eof)))))

(defn inputs-text [name]
  (slurp (io/resource name)))

(defn inputs-by-line [name]
  (->> (inputs-text name)
       str/split-lines
       (map #(inputs-from-string %))))
