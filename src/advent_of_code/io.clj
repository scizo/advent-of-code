(ns advent-of-code.io
  (:require [clojure.java.io :as io])
  (:import [java.io PushbackReader]))

(defn inputs [name]
  (let [reader (-> (io/resource name)
                   io/reader
                   PushbackReader.)]
    (->> #(read {:eof ::eof} reader)
         repeatedly
         (take-while #(not= % ::eof)))))

(defn inputs-text [name]
  (slurp (io/resource name)))
