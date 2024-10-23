(ns json.core
  (:require
   [json.scanner :as scanner]
   [json.utils :refer [illegal-state]]))

(defrecord Result [parsed tokens])

(defn- expect [token-type t]
  (when (not= (:type t) token-type)
    (illegal-state "Expected " token-type " but got " (:type t))))

(defn- consume [token-type [t & tokens]]
  (expect token-type t)
  tokens)

(declare parse)

(defn- parse-primitive [expected-type [t & tokens]]
  (expect expected-type t)
  (->Result (:value t) tokens))

(defn- parse-signed-num [[t1 t2 & tokens]]
  (expect :number t2)
  (case (:type t1)
    :plus  (->Result (:value t2) tokens)
    :minus (->Result (- (:value t2)) tokens)
    :else  (illegal-state "Unexpected token " (:type t1) " while parsing a number")))

(defn- parse-array
  ([tokens] (parse-array tokens (list) false))
  ([[t & tokens] acc require-element]
   (if (= :close-bracket (:type t))
     (if require-element
       (illegal-state "Unexpected token " :close-bracket)
       (->Result (reverse acc) tokens))
     (let [{value :parsed next-tokens :tokens} (parse (cons t tokens))
           [next-t & remaining] next-tokens]
       (case (:type next-t)
         :comma         (parse-array remaining (cons value acc) true)
         :close-bracket (parse-array next-tokens (cons value acc) false)
         :else          (illegal-state "Unexpected token " (:type next-t)))))))

(defn- parse-object
  ([tokens] (parse-object tokens (hash-map) false))
  ([[t & tokens] acc require-element]
   (if (= (:type t) :close-brace)
     (if require-element
       (illegal-state "Unexpected token " :close-brace)
       (->Result acc tokens))
     (let [_ (expect :string t)
           {value :parsed next-tokens :tokens} (parse (consume :colon tokens))
           [next-t & remaining] next-tokens
           new-acc (assoc acc (:value t) value)]
       (case (:type next-t)
         :comma        (parse-object remaining new-acc true)
         :close-brace  (parse-object next-tokens new-acc false)
         :else         (illegal-state "Unexpected token " (:type next-t)))))))

(defn parse [tokens]
  (let [token-type (:type (first tokens))]
    (cond
      (#{:null :boolean :number :string} token-type) (parse-primitive token-type tokens)
      (#{:plus :minus} token-type) (parse-signed-num tokens)
      (= :open-brace token-type) (parse-object (rest tokens))
      (= :open-bracket token-type) (parse-array (rest tokens))
      :else (illegal-state "Unexpected token " token-type))))

(defn -main
  [& args]
  (doseq [json ["\"stringerina\""
                "[true, false, null, 1, 2, \"stringerina\"]"
                "{}"
                "1"
                "+2"
                "-3"
                "{\"simple\": \"no nesting\"}"
                "{\"whatever\": \"bibibi\", \"complex\": {\"nested\": 12, \"array\": [10, -1, {\"deeply\": \"very\"}]}}"]]
    (println "Parsing" json)
    (println (:parsed (parse (scanner/scan json))))
    (println (type (:parsed (parse (scanner/scan json)))))
    (println)))
