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

(defn- parse-string [[t & tokens]]
  (expect :string t)
  (->Result (:value t) tokens))

(defn- parse-array
  ([tokens] (parse-array tokens (list)))
  ([[t & tokens] acc]
   (if (= :close-bracket (:type t))
     (->Result acc tokens)
     ())))

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
  (case (:type (first tokens))
    :string (parse-string tokens)
    :open-brace (parse-object (rest tokens))
    :open-bracket (parse-array (rest tokens))))

(defn -main
  [& args]
  ; (let [tokens (scanner/scan "\"stringerina\"")]
  ; (let [tokens (scanner/scan "[]")]
  (let [tokens (scanner/scan "{\"whatever\": \"bibibi\"}")]
    (println "Parsing: " tokens)
    (println (parse tokens))))
