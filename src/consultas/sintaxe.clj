(ns consultas.sintaxe
  (:require [clojure.string :as str]))

(declare parse-ou)

(defn- erro [msg]
  (throw (ex-info msg {:tipo :sintaxe})))

(def ^:private padrao
  #"\"[^\"]*\"|!=|<=|>=|[=<>(),|]|[^\s=<>!(),|\"]+|\S")

(defn- classificar [t]
  (cond
    (= t "\"") (erro "aspas sem fechar")
    (re-matches #"\"[^\"]*\"" t) [:str (subs t 1 (dec (count t)))]
    (re-matches #"!=|<=|>=|[=<>(),|!]" t) [:sim t]
    (re-matches #"-?\d+" t) [:num (Long/parseLong t)]
    (re-matches #"-?\d+\.\d+" t) [:num (Double/parseDouble t)]
    :else [:pal t]))

(defn- tokenizar [texto]
  (mapv classificar (re-seq padrao texto)))

(defn- dividir [tokens]
  (reduce (fn [acc t]
            (if (= t [:sim "|"])
              (conj acc [])
              (conj (pop acc) (conj (peek acc) t))))
          [[]]
          tokens))

(def ^:private operadores
  {"=" :=, "!=" :!=, "<" :<, "<=" :<=, ">" :>, ">=" :>=})

(defn- literal [t]
  (if (contains? #{:num :str} (first t))
    (second t)
    (erro "literal esperado")))

(defn- parse-comparacao [ts]
  (let [[[_ campo] op-tok lit-tok & resto] ts
        op (when (= :sim (first op-tok)) (operadores (second op-tok)))]
    (when-not op (erro "operador esperado depois do campo"))
    (when-not lit-tok (erro "falta o literal"))
    [[op (keyword campo) (literal lit-tok)] resto]))

(defn- parse-primario [ts]
  (let [t (first ts)]
    (cond
      (nil? t) (erro "expressao incompleta")
      (= t [:sim "("]) (let [[e resto] (parse-ou (rest ts))]
                         (if (= (first resto) [:sim ")"])
                           [e (rest resto)]
                           (erro "parentese sem fechar")))
      (= (first t) :pal) (parse-comparacao ts)
      :else (erro "campo ou expressao esperada"))))

(defn- parse-nao [ts]
  (if (= (first ts) [:pal "nao"])
    (let [[x resto] (parse-nao (rest ts))]
      [[:nao x] resto])
    (parse-primario ts)))

(defn- parse-e [ts]
  (let [[esq resto] (parse-nao ts)]
    (loop [esq esq, resto resto]
      (if (= (first resto) [:pal "e"])
        (let [[dir r] (parse-nao (rest resto))]
          (recur [:e esq dir] r))
        [esq resto]))))

(defn- parse-ou [ts]
  (let [[esq resto] (parse-e ts)]
    (loop [esq esq, resto resto]
      (if (= (first resto) [:pal "ou"])
        (let [[dir r] (parse-e (rest resto))]
          (recur [:ou esq dir] r))
        [esq resto]))))

(defn- campo-de [t]
  (if (= (first t) :pal)
    (keyword (second t))
    (erro "campo esperado")))

(defn- campo-unico [ts]
  (if (= 1 (count ts))
    (campo-de (first ts))
    (erro "um campo esperado")))

(defn- sem-resto [ts]
  (when (seq ts) (erro "token inesperado depois do fim do elemento")))

(defn- parse-campos [ts]
  (loop [ts ts, acc []]
    (let [acc (conj acc (campo-de (first ts)))
          resto (rest ts)]
      (cond
        (empty? resto) acc
        (= (first resto) [:sim ","]) (recur (rest resto) acc)
        :else (erro "virgula esperada entre campos")))))

(defn- parse-agregacao [ts]
  (let [[t & args] ts]
    (case (second t)
      "contar" (do (sem-resto args) [:contar])
      "soma" [:soma (campo-unico args)]
      "media" [:media (campo-unico args)]
      (erro "agregacao esperada: contar, soma ou media"))))

(defn- parse-segmento [seg]
  (when (empty? seg) (erro "estagio vazio"))
  (let [palavra (second (first seg))
        args (rest seg)]
    (case palavra
      "onde" (let [[e resto] (parse-ou args)]
               (sem-resto resto)
               [:onde e])
      "ordenar" (let [[p c & r] args]
                  (when-not (= p [:pal "por"]) (erro "falta \"por\" depois de ordenar"))
                  (let [campo (campo-de c)]
                    (cond
                      (empty? r) [:ordenar-por campo :asc]
                      (= r [[:pal "desc"]]) [:ordenar-por campo :desc]
                      :else (erro "so desc pode vir depois do campo"))))
      "limitar" (let [[t & r] args]
                  (sem-resto r)
                  (if (and (= :num (first t)) (integer? (second t)) (>= (second t) 0))
                    [:limitar (second t)]
                    (erro "numero esperado")))
      "selecionar" [:selecionar (parse-campos args)]
      "contar" (do (sem-resto args) [:contar])
      "soma" [:soma (campo-unico args)]
      "media" [:media (campo-unico args)]
      "agrupar" (let [[p c w & r] args]
                  (when-not (= p [:pal "por"]) (erro "falta \"por\" depois de agrupar"))
                  (let [campo (campo-de c)]
                    (when-not (= w [:pal "com"]) (erro "falta \"com <agregacao>\""))
                    [:agrupar campo (parse-agregacao r)]))
      (erro (str "estagio desconhecido: " palavra)))))

(def ^:private finais #{:selecionar :contar :soma :media :agrupar})

(defn- final? [no]
  (contains? finais (first no)))

(defn analisar [texto]
  (let [tokens (tokenizar texto)]
    (if (empty? tokens)
      {:estagios [] :terminal nil}
      (let [nos (mapv parse-segmento (dividir tokens))
            ultimo (peek nos)
            corpo (pop nos)]
        (when (some final? corpo)
          (erro "o elemento final tem que ser o ultimo"))
        (cond
          (= :selecionar (first ultimo)) {:estagios nos :terminal nil}
          (final? ultimo) {:estagios corpo :terminal ultimo}
          :else {:estagios nos :terminal nil})))))