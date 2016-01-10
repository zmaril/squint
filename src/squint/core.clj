(ns squint.core
  (:refer-clojure :exclude [cat])
  (:require [instaparse.core :as insta]
            [clojure.java.io :refer [resource]]
            [com.rpl.specter :refer :all]
            [clojure.pprint :refer [pprint]]
            [instaparse.combinators :refer :all]))

(def sgml-root (resource "postgresql/doc/src/sgml/ref/"))
(def allfiles  (slurp (str sgml-root "allfiles.sgml")))
;;Assumes only one synopsis per file
(def all-files
  (mapv (comp vec rest) (re-seq #"(\w+)\s+SYSTEM\s+\"(.*)\">" allfiles)))

(def get-file (comp slurp (partial str sgml-root)))
(def extract-synopsis (comp first rest (partial re-find #"(?s)<synopsis>\n(.*)\n</synopsis>")))
(def all-synopsis
  (->> all-files
       (transform [ALL (keypath 1)] get-file )
       (filter (fn [[n s]] (when s (not (.contains s "<cmdsynopsis>")))))
       (transform [ALL (keypath 1)] extract-synopsis)
   ) )

;;https://www.youtube.com/watch?v=hyLWrKh2fB0
(def extract-phrase (comp rest (partial re-find #"(<phrase>.*</phrase>)")))
(def all-phrase (mapcat (comp extract-phrase get-file) (select [ALL (keypath 1)] all-files)))

(defn alts [& s]
  (apply alt (map string s)))

(def sgml-grammar
  {:defs "statement (<'\n'+> statement)* <';'?>"
   :statement "(syntax | optional | alt | replaceable | grouping | phrase | repeated)+"
   :syntax    (regexp #"[A-Z()_*'=,.]+")
   :optional  "<'['> statement <']'>"
   :grouping  "<'{'> statement <'}'>"
   :alt       "statement (<'|'> statement)+"
   :repeated  "'[' ',' '...' ']'"
   :phrase        "<'<phrase>'> (old-syntax | real-insides) <'</phrase>'> "
   :old-syntax    "<'or the old syntax'>"
   :real-insides  (cat (string "where") (nt :replaceable) (nt :options) (string ":"))
   :options       "'is one of' | 'can be one of' | 'can be' | 'is' | 'can be empty or one of'"
   :replaceable
   (cat (hide-tag (alts "<replaceable>"
                    "<replaceable class=\"parameter\">"
                    "<replaceable class=\"PARAMETER\">"))
        (regexp #"[a-z_\-A-Z]+")
        (hide-tag (string "</replaceable>")))})

(def sgml-grammar
  (transform [ALL (keypath 1) string?] ebnf sgml-grammar))

(def parse-synopsis (insta/parser sgml-grammar :start :defs :auto-whitespace (insta/parser "ws = ' '+ | #'\n[ ]+'")))

(def sgml->insta-map
  {:syntax string
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

#_(def example (slurp (str sqml-root "/abort.sgml")))
#_(def parsed-example
  (synopsis->parser :abort (first (extract-synopsis example))))

;;Skipping for now because of phrasing
;;alterAggregate
(def rightnow
  (-> all-synopsis (nth 3) (nth 1))
  )

(def problematic
  #{"cluster" ;;contains two syntaxes
    "createEventTrigger" ;;Bug in the postgres docs: filter_value should be replaceable
    "createFunction" ;; Won't parse which is weird.
    ;;"createTransform" includes a ; at the end of the statement definition. Is that on purpose?
    "createView" ;;has two definitions in it
    "reset"      ;; has two syn's
    })

(defn go []
  (->> all-synopsis
       (filter (fn [[n s]] (when s (not (.contains s "<phrase>")))))
       (filter (fn [[n s]] (not (problematic n))))
       (drop-while (comp not insta/failure? parse-synopsis second))))

(def next-file   (comp ffirst go))
(def next-string (comp second first go))
(def next-error  (comp parse-synopsis second first go))
(def parse-file  (comp parse-synopsis extract-synopsis get-file))
