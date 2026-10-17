(ns consultas.compilador)

(defn- tipo-literal [v]
  (cond (integer? v) :inteiro
        (number? v) :decimal
        (string? v) :texto
        :else :desconhecido))

(defn- numerico? [t]
  (contains? #{:inteiro :decimal} t))

(defn- compativel? [a b]
  (or (= a b) (and (numerico? a) (numerico? b))))

(defn- erros-campo [tipos campo]
  (if (contains? tipos campo)
    []
    [(str "campo inexistente: " (name campo))]))

(defn- erros-numerico [tipos campo]
  (let [e (erros-campo tipos campo)]
    (cond
      (seq e) e
      (numerico? (tipos campo)) []
      :else [(str "soma e media exigem campo numerico: " (name campo))])))

(defn- erros-expr [tipos no]
  (case (first no)
    (:e :ou) (vec (mapcat #(erros-expr tipos %) (rest no)))
    :nao (erros-expr tipos (second no))
    (:= :!= :< :<= :> :>=)
    (let [[_ campo lit] no]
      (if-not (contains? tipos campo)
        (erros-campo tipos campo)
        (let [tc (tipos campo)
              tl (tipo-literal lit)]
          (if (compativel? tc tl)
            []
            [(str "comparacao entre " (name tc) " e " (name tl)
                  " no campo " (name campo))]))))))

(defn- erros-agregacao [tipos no]
  (case (first no)
    :contar []
    (:soma :media) (erros-numerico tipos (second no))))

(defn- erros-terminal [tipos no]
  (case (first no)
    :agrupar (into (erros-campo tipos (second no))
                   (erros-agregacao tipos (nth no 2)))
    (:contar :soma :media) (erros-agregacao tipos no)))

(defn- erros-estagio [tipos [tipo & args]]
  (case tipo
    :onde (erros-expr tipos (first args))
    :ordenar-por (into (erros-campo tipos (first args))
                       (if (contains? #{:asc :desc} (second args))
                         []
                         ["ordem invalida: use :asc ou :desc"]))
    :limitar (let [n (first args)]
               (if (and (integer? n) (>= n 0))
                 []
                 [(str "limitar exige inteiro nao negativo: " n)]))
    :selecionar (vec (mapcat #(erros-campo tipos %) (first args)))))

(defn- selecionar? [est]
  (= :selecionar (first est)))

(defn- erros-posicao [ast]
  (let [ests (:estagios ast)]
    (cond
      (some selecionar? (butlast ests)) ["selecionar tem que ser o ultimo"]
      (and (:terminal ast) (some selecionar? ests)) ["selecionar tem que ser o ultimo"]
      (:terminal-nao-final ast) ["o terminal tem que ser o ultimo"]
      :else [])))

(defn conferir [ast esquema]
  (let [tipos (into {} esquema)]
    (vec (concat (mapcat #(erros-estagio tipos %) (:estagios ast))
                 (when-let [t (:terminal ast)] (erros-terminal tipos t))
                 (erros-posicao ast)))))

;; compilacao: ast -> funcoes

(defn- comparar [teste campo lit reg]
  (teste (compare (get reg campo) lit)))

(defn- compilar-expr [no]
  (case (first no)
    :e (apply every-pred (map compilar-expr (rest no)))
    :ou (apply some-fn (map compilar-expr (rest no)))
    :nao (complement (compilar-expr (second no)))
    (:= :!= :< :<= :> :>=)
    (let [[op campo lit] no
          teste (case op
                  := zero?
                  :!= (complement zero?)
                  :< neg?
                  :<= (complement pos?)
                  :> pos?
                  :>= (complement neg?))]
      (partial comparar teste campo lit))))

(defn- ordenar [campo ordem registros]
  (sort-by campo
           (case ordem
             :asc compare
             :desc #(compare %2 %1))
           registros))

(defn- compilar-estagio [[tipo & args]]
  (case tipo
    :onde (partial filter (compilar-expr (first args)))
    :ordenar-por (partial ordenar (first args) (second args))
    :limitar (partial take (first args))
    :selecionar (let [campos (first args)]
                  (partial map #(select-keys % campos)))))

(defn- compilar-agregacao [no tipos]
  (case (first no)
    :contar (partial reduce (fn [n _] (inc n)) 0)
    :soma (let [campo (second no)
                zero (if (= :decimal (tipos campo)) 0.0 0)]
            (partial reduce (fn [s r] (+ s (get r campo))) zero))
    :media (let [campo (second no)]
             (fn [regs]
               (let [[s n] (reduce (fn [[s n] r] [(+ s (get r campo)) (inc n)])
                                   [0 0]
                                   regs)]
                 (if (zero? n) 0.0 (double (/ s n))))))))

(defn- compilar-terminal [no tipos]
  (case (first no)
    :agrupar (let [campo (second no)
                   agg (compilar-agregacao (nth no 2) tipos)]
               (fn [regs]
                 (mapv (fn [[k g]] [k (agg g)])
                       (sort-by key compare (group-by campo regs)))))
    (:contar :soma :media) (compilar-agregacao no tipos)))

(defn compilar [ast esquema]
  (let [tipos (into {} esquema)
        etapas (reduce (fn [f g] (comp g f))
                       identity
                       (map compilar-estagio (:estagios ast)))
        fim (if-let [t (:terminal ast)]
              (compilar-terminal t tipos)
              identity)]
    (comp fim etapas)))