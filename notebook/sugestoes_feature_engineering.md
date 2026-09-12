# Sugestões de engenharia de *features*

**Projeto Integrador II · Insper PADS 2026 — acessibilidade e commute em São Paulo**

Documento para **discussão com o grupo**, não para virar código antes da
conversa. Tudo aqui sai direto dos achados do
[`eda_acessibilidade_sp.ipynb`](eda_acessibilidade_sp.ipynb) e usa apenas
`acessibilidade_sp_2017_2019.csv`, salvo a seção 7 (enriquecimento externo).

---

## 0. Antes das features: três decisões que o grupo precisa tomar

Nenhuma feature faz sentido sem isto resolvido. São as perguntas para levar
para a próxima reunião.

**Decisão 1 — qual é a unidade de análise?**
A base tem grão `(hexágono, ano, modo, período)`. Três recortes possíveis:

| Recorte | Linhas | Serve para |
|:--|--:|:--|
| **Hexágono, TP, pico, 2019** (recomendado) | 10.056 | Diagnosticar e priorizar território. Uma linha = um lugar. |
| Hexágono × ano (TP, pico) | 30.168 | Testar tendência — mas 3 anos e demografia congelada dão pouca variação. |
| Hexágono × modo (2019) | 40.224 | Comparar modos como tratamento. Exige cuidado: `automovel` só existe em 2019. |

**Decisão 2 — qual é o alvo?** O EDA não define isso, e cada opção muda tudo:

- **(a) Índice de acesso ao trabalho** — construir um score contínuo por
  hexágono, sem modelo supervisionado. É descritivo, defensável e entrega
  priorização direta. *Menor risco, menor "cara de ML".*
- **(b) Classificação de "deserto de emprego"** — rotular hexágonos e prever o
  rótulo a partir de características estruturais. Ver §5 para a definição.
- **(c) Modelo explicativo** — regressão de `empregos_60min` contra atributos
  do lugar, para **quantificar** quanto do acesso é explicado por renda,
  distância ao centro e presença de trilho. *É o que mais responde ao problem
  statement.*
- **(d) Simulação contrafactual** — estimar o ganho de acesso de uma
  intervenção (nova estação, corredor). Exige GTFS + roteamento; é a mais
  ambiciosa e a mais vendável.

**Decisão 3 — como validar.** Os hexágonos são **espacialmente
autocorrelacionados**: vizinhos se parecem. Split aleatório vai inflar a
métrica de forma grosseira, porque o vizinho do treino está no teste. Usar
**validação por blocos espaciais** (dividir a cidade em faixas ou distritos e
segurar blocos inteiros fora do treino). Isso não é preciosismo — é a diferença
entre um R² honesto e um R² falso.

---

## 1. Princípios que valem para toda feature

1. **Ponderar pela população.** Toda estatística agregada usa `pop_total` como
   peso. Vale também para métrica de modelo: um erro em hexágono de 3 mil
   moradores custa mais que em um de 30.
2. **Normalizar antes de comparar.** `empregos_60min` é contagem absoluta e o
   total da cidade muda a cada ano. Usar sempre a fração do mercado
   (`empregos_60min / empregos_da_cidade_no_ano`).
3. **Nunca imputar `tempo_*` com média.** Vazio significa *não alcançável*. Ver
   §3.4.
4. **Não misturar oferta local com acesso.** `empregos_total` e
   `empregos_60min` medem coisas diferentes. Se as duas entrarem no modelo,
   documentar por quê.
5. **Cuidado com vazamento conceitual.** Prever `empregos_60min` usando
   `empregos_90min` como preditor não é modelo, é aritmética. Se o alvo é
   acesso, os outros cortes de tempo saem do conjunto de preditores.

---

## 2. O que já está pronto no notebook e vira feature direto

| Feature | Fórmula | De onde vem |
|:--|:--|:--|
| `pct_empregos_60` | `empregos_60min / empregos_cidade_ano` | Passo 1.5 |
| `residuo_decil` | acesso observado − média do próprio decil | Passo 6.3 |
| `presenca_saude`, `presenca_cras` | `1` se a contagem local > 0 | Passo 3.2 |
| `pop_economicamente_ativa` | `pop_19a24 + pop_25a39 + pop_40a69` | Passo 2.3 |
| `pct_pop_negra` | `pop_negra / (branca+negra+amarela+indigena)` | Passo 2.3 |

---

## 3. Features novas propostas

### 3.1 Forma da curva de acesso — o achado mais aproveitável do EDA

O Passo 6.2 mostrou que os cortes de tempo são quase colineares entre si
(ρ > 0,9). Colocar os seis no modelo é redundância pura. Mas a **forma** da
curva carrega informação que nenhum corte isolado tem.

| Feature | Fórmula | Por que importa |
|:--|:--|:--|
| `minutos_para_25pct` | interpolar a curva cumulativa e achar o tempo em que se alcança 25% dos empregos da cidade | Inverte a pergunta: em vez de "quanto alcanço em 1h", vira **"quanto tempo custa um quarto do mercado"**. É a métrica que fala a língua da pessoa que faz o commute. |
| `elasticidade_30_60` | `empregos_60min / empregos_30min` | Razão alta = o lugar depende de viagem longa; razão baixa = já tem mercado por perto. |
| `ganho_marginal_90` | `(empregos_90min − empregos_60min) / empregos_cidade` | Quanto se compra com a segunda meia hora. Alto na periferia bem conectada por trilho. |
| `saturacao_120` | `empregos_120min / empregos_cidade` | Teto do lugar: mesmo com 2h, quanto da cidade se alcança. |

**Nota:** `minutos_para_25pct` exige interpolação sobre 5 pontos (15/30/60/90/
120) e vai saturar em hexágonos que nunca chegam a 25%. Tratar a saturação como
censura à direita, não como valor faltante — codificar
`atinge_25pct = False` e usar 120 como limite inferior do tempo real.

### 3.2 Penalidade modal — o número que mais "vende" o problema

| Feature | Fórmula | Leitura |
|:--|:--|:--|
| `penalidade_tp_carro` | `empregos_60min[automovel] / empregos_60min[TP]`, 2019 pico | Quantas vezes o carro multiplica o mercado de trabalho daquele endereço. Na média da cidade o carro entrega 3,9x mais (75,3% contra 19,5%). **Onde é 8x, o lugar é refém do automóvel.** |
| `vantagem_tp_bici` | `empregos_60min[TP] / empregos_60min[bicicleta]` | Onde o ônibus quase não supera a bicicleta, a rede está falhando de forma qualitativa, não quantitativa. |
| `dependencia_motorizada` | `1 − empregos_60min[caminhada] / empregos_60min[TP]` | Fração do acesso que só existe porque há transporte motorizado. |

Requer *pivotar* a base por modo. Cuidado: `automovel` só em 2019, então
`penalidade_tp_carro` só existe nesse ano.

### 3.3 Sensibilidade ao horário — a hipótese do Achado 11

| Feature | Fórmula | Leitura |
|:--|:--|:--|
| `dependencia_do_pico` | `(empregos_60min[pico] − empregos_60min[fora_pico]) / empregos_60min[fora_pico]` | Quanto o lugar perde quando o reforço de pico acaba. Decil 1: +15%; decil 10: +5%. **É a melhor proxy disponível de "a rede aqui é feita de linhas de pico".** |

Essa é, na nossa avaliação, a feature mais promissora da base para conectar
com o GTFS depois: onde a dependência do pico é alta, esperamos encontrar
poucas linhas com *headway* longo fora do horário.

### 3.4 Tempo até o mais próximo — tratar a censura, não imputar

O padrão correto para cada `tempo_X` é **decompor em duas colunas**:

```
alcanca_X        = tempo_X.notna()                  # binária
tempo_X_censurado = tempo_X.fillna(LIMITE[modo])    # 60 a pé, 90 bici, 175 TP
```

Modelar `alcanca_X` e `tempo_X_censurado` separadamente é honesto; preencher o
vazio com a média é fabricar acesso onde não existe. Se o modelo aceitar,
melhor ainda é usar regressão para dados censurados (Tobit, ou sobrevivência
com censura à direita).

Features derivadas úteis:

| Feature | Fórmula |
|:--|:--|
| `n_servicos_inalcancaveis_a_pe` | soma das binárias `~alcanca_X` para escola, saúde, CRAS, a pé |
| `ganho_do_onibus_saude` | `tempo_saude_alta[caminhada] − tempo_saude_alta[TP]` |

### 3.5 Espaciais — derivadas só de `lon`/`lat`

| Feature | Como | Por quê |
|:--|:--|:--|
| `dist_centro_km` | Haversine até a Praça da Sé (−46,6333; −23,5505) | Controle óbvio; separa "longe" de "mal servido". |
| `dist_polo_empregos_km` | distância ao centroide dos empregos, ponderado por `empregos_total` | Melhor que a Sé: o polo real de São Paulo puxa para o vetor sudoeste. |
| `acesso_vizinhanca_k6` | média de `pct_empregos_60` dos 6 vizinhos imediatos | Suaviza ruído e permite calcular o item seguinte. |
| `gradiente_local` | `pct_empregos_60 − acesso_vizinhanca_k6` | Isola **quebras abruptas** de acesso — tipicamente a borda de uma área servida por trilho. Candidato a detector de estação sem usar o GTFS. |
| `isolamento` | distância ao hexágono mais próximo com `pct_empregos_60` acima da mediana da cidade | "Quão longe estou de um lugar com acesso decente". |

`cKDTree` do SciPy resolve vizinhança e distâncias em segundos; converter
lon/lat para metros com a aproximação local já usada no notebook.

### 3.6 Descasamento morador × vaga

| Feature | Fórmula | Leitura |
|:--|:--|:--|
| `mix_qualificacao_alcancado` | `empregos_alta_esc_60min / empregos_baixa_esc_60min` | Achado 10: sobe monotonicamente com o decil. |
| `desvio_do_mix_da_cidade` | acima dividido pela mesma razão na cidade inteira | Normaliza: >1 = alcança mais vaga qualificada que a média. |
| `vagas_baixa_esc_por_ativo` | `empregos_baixa_esc_60min / pop_economicamente_ativa` | Concorrência aproximada por vaga compatível. **Interpretar com cuidado:** o denominador é local e o numerador é de toda a região alcançável. |

### 3.7 Massa e custo social — para priorização

| Feature | Fórmula | Leitura |
|:--|:--|:--|
| `deficit_de_acesso_pessoas` | `pop_total × (mediana_da_cidade − pct_empregos_60)`, truncado em 0 | **Pessoas × pontos percentuais faltantes.** Ordena onde uma intervenção rende mais. Um hexágono muito ruim com 40 moradores perde para um médio com 4 mil — e é assim que se prioriza política pública. |
| `razao_ativa_passiva` | `empregos_60min / pessoas_que_alcancam_60min` | >1 = lugar de destino (emprego); <1 = lugar de origem (dormitório). Separa a cidade em duas funções sem precisar de pesquisa OD. |

---

## 4. Índice composto — se a decisão for a opção (a)

Para um score único e defensável, o caminho mais limpo:

1. Escolher 4 a 6 componentes **não redundantes** (ex.: `pct_empregos_60`,
   `dependencia_do_pico`, `penalidade_tp_carro`, `n_servicos_inalcancaveis_a_pe`).
2. Transformar cada uma em **percentil ponderado pela população** — não em
   z-score. Percentil é robusto à assimetria brutal dessas distribuições e é
   interpretável ("este lugar está no percentil 12 de acesso").
3. Combinar com pesos explícitos e **documentados**. Peso igual é uma escolha,
   não a ausência de escolha — dizer isso na apresentação.
4. Testar a sensibilidade: se mudar os pesos em ±30% reordena o ranking dos 50
   piores hexágonos, o índice é frágil e não deve ser apresentado como número
   único.

---

## 5. Se a decisão for classificação: definir "deserto de emprego"

Proposta de rótulo, para o grupo criticar:

> Um hexágono é **deserto de emprego** se, simultaneamente:
> - `pct_empregos_60` abaixo do 1º quartil ponderado da cidade **e**
> - `pop_total > 500` (não é área vazia) **e**
> - `dependencia_do_pico` acima da mediana (o acesso que existe é frágil).

Com esse corte é preciso checar o balanceamento antes de escolher métrica —
provavelmente ficará em torno de 15–20% de positivos, o que joga a avaliação
para AUC/PR e não para acurácia, como já fizemos na Integradora I.

**Alternativa mais forte:** em vez de classificar o que já é observável,
prever `pct_empregos_60` **sem usar nenhuma variável de acessibilidade como
preditor** — só uso do solo, demografia e geografia. O resíduo desse modelo é
a parte do acesso que a localização **não** explica, e essa parte é a rede de
transporte. É um jeito elegante de isolar o efeito do transporte sem GTFS.

---

## 6. Armadilhas conhecidas

| Armadilha | Consequência | Mitigação |
|:--|:--|:--|
| Autocorrelação espacial | R² inflado, modelo que não generaliza | Validação por blocos espaciais |
| Colinearidade entre cortes de tempo | Coeficientes instáveis e sem interpretação | Usar um corte + features de forma (§3.1) |
| Média não ponderada | Toda estatística vira "média de lugares", não de pessoas | `pond()` do notebook em tudo |
| Censo 2010 com rede de 2019 | Atribui a 2019 uma demografia de 9 anos antes | Declarar como limitação; não usar a base para afirmar nada sobre população atual |
| RAIS = só emprego formal | Subestima o mercado real da periferia, onde a informalidade é maior | Declarar; se possível, contrastar com PNAD |
| `empregos_total` vs `empregos_60min` | Confusão silenciosa que inverte conclusões | Prefixar os nomes no dataset de modelagem (`local_*` vs `acesso_*`) |
| Modo `automovel` só em 2019 | Feature com 2/3 de nulos se o painel for por ano | Calcular `penalidade_tp_carro` só no recorte 2019 |

---

## 7. Enriquecimento externo — a agenda depois desta base

Ordenado por relação esforço/retorno:

1. **GTFS SPTrans** (já temos da V1) — `n_linhas_distintas` alcançáveis,
   `headway_medio_pico`, `headway_medio_fora_pico`, `n_paradas_300m`,
   `distancia_estacao_trilho`. A última é a que testa o Achado 16
   diretamente, e é barata: é só distância a um ponto.
2. **Distância a estação de metrô/CPTM** — isolada, já vale um gráfico.
   Hipótese: explica a maior parte dos resíduos positivos do Passo 6.3.
3. **ObservaSampa** (já baixado em `Design/`) — indicadores por distrito, para
   validação externa e para dar nome aos lugares na apresentação.
4. **Censo 2022** — corrige a demografia congelada, mas não tem grade H3
   pronta; exige compatibilização de setor censitário para hexágono. **Alto
   custo, avaliar se compensa.**
5. **Tarifa e integração** — não existe em base pública padronizada; provável
   desk research. É a dimensão de **custo** do commute, que hoje está
   inteiramente fora do trabalho.

---

## 8. Sugestão de próximo passo concreto

Um único entregável, pequeno, antes de qualquer modelo:

> `notebook/features.py` com uma função `construir_features(df) -> DataFrame`
> no grão hexágono-2019, produzindo as features das seções 3.1 a 3.7 e
> gravando `dados/saida/features_hex_2019.csv`. Sem modelo, sem escolha de
> alvo — só a tabela de trabalho, versionada e revisável.

Assim a discussão sobre alvo (§0, Decisão 2) acontece **olhando as features
prontas**, e não no abstrato.
