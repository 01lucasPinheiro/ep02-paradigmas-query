(ns consultas.main
  (:require [clojure.string :as str]
            [consultas.core :as core]))

(defn- mensagem [e]
  (str "ERRO: " (or (ex-message e) (str e))))

(defn responder [linha dados]
  (try
    (let [cmd (str/trim linha)]
      (cond
        (str/blank? cmd) ["ERRO: linha em branco"]
        (re-matches #"(?s)QUERY(?:\s+(.*))?" cmd)
        (let [[_ texto] (re-matches #"(?s)QUERY(?:\s+(.*))?" cmd)
              ast (core/analisar (or texto ""))
              res (core/executar ast dados)]
          (if-let [erros (:erros res)]
            [(str "ERRO: " (first erros))]
            (core/linhas-resultado ast dados res)))
        :else [(str "ERRO: comando desconhecido: " cmd)]))
    (catch Throwable e
      [(mensagem e)])))

(defn- laco [dados]
  (loop []
    (print "> ")
    (flush)
    (let [linha (read-line)]
      (when (and linha (not= "EXIT" (str/trim linha)))
        (run! println (responder linha dados))
        (recur)))))

(defn -main [& args]
  (if-let [caminho (first args)]
    (let [dados (try
                  (core/carregar-csv (slurp caminho :encoding "UTF-8"))
                  (catch Throwable e
                    (println (mensagem e))
                    nil))]
      (if dados
        (laco dados)
        (System/exit 1)))
    (do (println "ERRO: uso: clojure -M:run <arquivo.csv>")
        (System/exit 1))))