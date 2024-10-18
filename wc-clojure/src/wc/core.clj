(ns wc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn count-bytes [filename]
  (.length (io/file filename)))

(defn count-by [fun filename]
  (with-open [rdr (io/reader filename :encoding "UTF-8")]
    (fun (line-seq rdr))))

(def count-chars
  (partial count-by #(reduce + (map count %))))

(def count-words
  (partial count-by #(reduce + (map (comp count (partial re-seq #"\S+")) %))))

(def count-lines count)

(def usage "Usage: wc [opts] <filename>")

(defn compute-with [fun filename]
  (when (.exists (io/file filename))
    (fun filename)))

(def count-funs
  {nil  (juxt count-lines count-words count-bytes)
   "-c" (comp vector count-bytes)
   "-m" (comp vector count-chars)
   "-w" (comp vector count-words)
   "-l" (comp vector count-lines)})

(defn pad-left [s total-length]
  (format (str "%" total-length "s") s))

(defn execute [{:keys [opt filenames]}]
  (let [fun (count-funs opt)
        results (map #(compute-with fun %) filenames)
        str-results (map #(when % (map str %)) results)
        max-w (inc (apply max 0 (mapcat #(map count %) (remove nil? str-results))))]
    (doseq [[filename result] (map vector filenames str-results)]
      (if result
        (println (str/join (map #(pad-left % max-w) result)) filename)
        (println (str filename ": No such file or directory"))))
    (let [valued (remove nil? results)]
      (when (next valued)
        (->> valued
             (reduce (partial map +))
             (map str)
             (map #(pad-left % max-w))
             (str/join)
             #(println % "total"))))))

(defrecord Command [opt filenames])

(defn parse-command [args]
  (let [[opt & filenames] args]
    (cond
      (empty? args) nil
      (re-matches #"-[cmwl]" opt) (when (seq filenames) (Command. opt filenames))
      (.startsWith opt "-") nil
      :else (Command. nil args))))

(defn -main [& args]
  (if-let [cmd (parse-command args)]
    (execute cmd)
    (println usage)))
