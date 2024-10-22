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
  ([tokens] (parse-object tokens (hash-map)))
  ([[t & tokens] acc]
   (let [ty (:type t)]
     (if (= ty :close-brace)
       (->Result acc tokens)
       (let [_ (expect :string t)
             {value :parsed next :tokens} (parse (consume :colon tokens))
             new-acc (assoc acc (:value t) value)]
               ; todo handle comma
         (parse-object next new-acc))))))

; :string (let [[value-token & rest-tokens] (consume :colon tokens)
;               _ (expect :string value-token)
;               value (:value value-token)]
;           (parse-object rest-tokens (assoc acc (:value t) value))))))

(defn parse [tokens]
  (let [token-type (:type (first tokens))]
    (cond
      (#{:null :boolean :number :string} token-type) (parse-primitive token-type tokens)
      (= :open-brace token-type) (parse-object (rest tokens))
      (= :open-bracket token-type) (parse-array (rest tokens))
      :else (illegal-state "Unexpected token " token-type))))

(defn -main
  [& args]
  ; (let [json (scanner/scan "\"stringerina\"")]
  ; (let [json "[true, false, null, 1, 2, \"stringerina\"]"]
  (let [json "{\"whatever\": \"bibibi\"}"]
    (println "Parsing" json)
    (println (:parsed (parse (scanner/scan json))))))
