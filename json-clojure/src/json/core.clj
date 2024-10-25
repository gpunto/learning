(ns json.core
  (:require
   [json.parser :as parser]
   [json.scanner :as scanner]))

(defn parse-file [filename]
  (->> filename
       (slurp)
       (scanner/scan)
       (parser/parse)))

(defn -main
  [& args]
  (println (parse-file (first args))))

