# query builder funcional em clojure

## Nomes
    Lucas Pinheiro da Costa Pedrosa
    Matheus Dutra 
    Pedro Henrique Lira

## como executar

    clojure -M:run dados/filmes.csv

comandos: `QUERY <consulta>` e `EXIT`.

## a forma da ast

a consulta e um mapa feito so de dados (mapas, vetores e palavras-chave):

    {:estagios [[:onde [:e [:= :genero "drama"] [:>= :nota 8]]]
                [:ordenar-por :nota :desc]
                [:limitar 3]
                [:selecionar [:titulo :nota]]]
     :terminal nil}

estagios (vetor, na ordem do texto):

- `[:onde expr]`
- `[:ordenar-por campo :asc|:desc]`
- `[:limitar n]`
- `[:selecionar [campo ...]]` (tem que ser o ultimo elemento)

expressoes: `[:e a b]`, `[:ou a b]`, `[:nao a]` e `[op campo literal]`
com op em `:=`, `:!=`, `:<`, `:<=`, `:>`, `:>=`.

terminal (ou nil):

- `[:contar]`
- `[:soma campo]`
- `[:media campo]`
- `[:agrupar campo agregacao]`, com agregacao `[:contar]`, `[:soma campo]` ou `[:media campo]`

exemplo de no desconhecido para teste: `{:estagios [[:voar 3]] :terminal nil}`
(o compilador falha com `No matching clause: :voar`).

## namespaces

- `consultas.main`: laco do terminal (loop/recur), leitura do arquivo e todo o i/o.
- `consultas.sintaxe`: texto -> ast, analise recursiva que devolve no e tokens restantes.
- `consultas.compilador`: conferencia da ast contra o esquema e ast -> funcoes (case sem padrao).
- `consultas.core`: leitura do csv, builder (`consulta`, `onde`, `selecionar`, `ordenar-por`,
  `limitar`, `analisar`, `executar`) e formatacao da saida.