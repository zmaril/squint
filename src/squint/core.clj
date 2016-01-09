(ns squint.core
  (:refer-clojure :exclude [cat])
  (:require [instaparse.core :as insta]
            [clojure.java.io :refer [resource]]
            [instaparse.combinators :refer :all]))

(def sqml-root (resource "postgresql/doc/src/sgml/ref/"))

(def allfiles  (slurp (str sqml-root "allfiles.sgml")))

(def all-files
  (map rest (re-seq #"(\w+)\s+SYSTEM\s+\"(.*)\">" allfiles)))

(defn extract-synopsis [s]
  (rest (re-find #"(?s)<synopsis>\n(.*)\n</synopsis>" s)))

(def all-synopsis
  (->> all-files 
       (map (fn [[name s]] [name (->> s (str sqml-root) slurp extract-synopsis)]))
       ))

(def sgml-grammar
  "statement = (keyword | optional | alt)*
   keyword   = #'[A-Z]+'
   optional  = <'['> statement <']'>
   alt       = statement (<'|'> statement)+"
  )

(def parse-synopsis
  (insta/parser sgml-grammar
                :auto-whitespace (insta/parser "ws = ' '")))

(def sgml->insta-map
  {:keyword   string
   :optional  opt
   :statement cat
   :alt       alt}
  )

(def sgml->insta (partial insta/transform sgml->insta-map))

(defn synopsis->parser [name s]
  (as-> s $
    (parse-synopsis $)
    (sgml->insta $)
    {name $}
    (insta/parser $ :start name)))

(def example (slurp (str sqml-root "/abort.sgml")))
(def parsed-example
  (synopsis->parser :abort (first (extract-synopsis example))))

(def rightnow
  (-> all-synopsis (nth 1) (nth 1))
  )
