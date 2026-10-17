(ns consultas.core
  (:require [clojure.string :as str]
            [consultas.sintaxe :as sintaxe]
            [consultas.compilador :as compilador]))

;; csv

(defn- parse-cabecalho [item]
  (let [[nome tipo & mais] (str/split item #":" -1)
        tipo (keyword (str/trim (or tipo "")))]
    (when (or mais (str/blank? nome) (not (contains? #{:inteiro :decimal :texto} tipo)))
      (throw (ex-info (str "cabecalho invalido: " item) {:tipo :csv})))
    [(keyword (str/trim nome)) tipo]))

(defn- converter [tipo texto]
  (case tipo
    :inteiro (Long/parseLong (str/trim texto))
    :decimal (Double/parseDouble (str/trim texto))
    :texto texto))

(defn- parse-linha [esquema linha]
  (let [valores (str/split linha #";" -1)]
    (when-not (= (count valores) (count esquema))
      (throw (ex-info (str "linha com numero errado de campos: " linha) {:tipo :csv})))
    (zipmap (map first esquema)
            (map (fn [[_ tipo] v] (converter tipo v)) esquema valores))))

(defn carregar-csv [texto]
  (let [linhas (->> (str/split-lines (str/replace texto "\uFEFF" ""))
                    (remove str/blank?))]
    (when (empty? linhas)
      (throw (ex-info "arquivo vazio" {:tipo :csv})))
    (let [esquema (mapv parse-cabecalho (str/split (first linhas) #";" -1))]
      {:esquema esquema
       :registros (mapv #(parse-linha esquema %) (rest linhas))})))

;; builder: consulta e dado, cada funcao devolve outra consulta

(defn consulta []
  {:estagios [] :terminal nil})

(defn- acrescentar [c estagio]
  (let [nova (update c :estagios conj estagio)]
    (if (:terminal c)
      (assoc nova :terminal-nao-final true)
      nova)))

(defn onde [c expr]
  (acrescentar c [:onde expr]))

(defn selecionar [c campos]
  (acrescentar c [:selecionar (vec campos)]))

(defn ordenar-por [c campo ordem]
  (acrescentar c [:ordenar-por campo ordem]))

(defn limitar [c n]
  (acrescentar c [:limitar n]))

(defn contar [c]
  (assoc c :terminal [:contar]))

(defn soma [c campo]
  (assoc c :terminal [:soma campo]))

(defn media [c campo]
  (assoc c :terminal [:media campo]))

(defn agrupar-por [c campo agregacao]
  (assoc c :terminal [:agrupar campo agregacao]))

(defn analisar [texto]
  (sintaxe/analisar texto))

(defn executar [c dados]
  (let [erros (compilador/conferir c (:esquema dados))]
    (if (seq erros)
      {:erros erros}
      ((compilador/compilar c (:esquema dados)) (:registros dados)))))

;; formatacao

(defn- formatar [v]
  (cond
    (integer? v) (str v)
    (float? v) (String/format java.util.Locale/ROOT "%.2f" (to-array [v]))
    :else (str v)))

(defn linhas-resultado [ast dados resultado]
  (let [{:keys [estagios terminal]} ast]
    (cond
      (nil? terminal)
      (let [ultimo (last estagios)
            campos (if (= :selecionar (first ultimo))
                     (second ultimo)
                     (mapv first (:esquema dados)))]
        (if (empty? resultado)
          ["(vazio)"]
          (mapv (fn [r] (str/join "; " (map (comp formatar r) campos))) resultado)))

      (= :agrupar (first terminal))
      (if (empty? resultado)
        ["(vazio)"]
        (mapv (fn [[k v]] (str (formatar k) "; " (formatar v))) resultado))

      :else [(formatar resultado)])))