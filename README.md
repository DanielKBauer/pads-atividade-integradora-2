# Acessibilidade urbana em São Paulo — 2017 a 2019

PADS · Atividade Integradora 2. Base AOP/Ipea + cruzamento com o **IPTC 2025
(SPTrans)** e dashboard Shiny para o time explorar onde a nota oficial e o
acesso a empregos em ≤60 min divergem.

## Dashboard (v0) — como o time sobe

Na raiz do repositório, com R ≥ 4.2:

```r
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "sf", "dplyr",
  "htmltools", "scales", "data.table"
))

shiny::runApp("dashboard")
```

As camadas já estão em `dashboard/data/camadas.rds` — **não precisa** rodar o
EDA para abrir o mapa. Para regenerar as camadas (depois de mudar dados):

```bash
Rscript dashboard/prep_dados.R
```

Detalhes do app: [`dashboard/README.md`](dashboard/README.md).

## Os dois arquivos que importam

**A planilha:** `dados/saida/acessibilidade_sp_2017_2019.csv`

**O racional:** `notebook/base_acessibilidade_sp_2017_2019.html` — abra no
navegador. Documenta de onde vem cada dado, por que a janela é 2017–2019, as
sete decisões de engenharia que definiram a base, as validações que ela passou
e o código que a gerou. É o material para apresentar o trabalho.

## A planilha

Cada linha responde: *a partir deste hexágono, neste ano, por este modo e neste
horário — quem mora aqui, o que existe aqui, e o que se alcança daqui.*

140.784 linhas, 69 colunas, 10.056 hexágonos × 3 anos × 4 modos de transporte.
Cabe no Excel e abre em R ou Python sem preparação. O painel é balanceado: os
mesmos 10.056 hexágonos aparecem em todos os anos e modos, o que é a condição
para comparar 2017, 2018 e 2019 entre si.

As colunas se dividem em três blocos com significados bem diferentes:

| Bloco | Responde | Varia com |
|:--|:--|:--|
| `pop_*`, `renda_*` | Quem mora **aqui** | Nada — é o Censo 2010 |
| `empregos_total`, `escolas_total`, … | O que existe **aqui** | O ano |
| `empregos_60min`, `tempo_saude_*`, … | O que se alcança **daqui** | O ano, o modo e o período |

`empregos_total` e `empregos_60min` medem coisas distintas: o primeiro são os
empregos dentro do hexágono, o segundo é quanto emprego da cidade se alcança
partindo dele. Confundir os dois é o erro mais fácil de cometer.

O significado de cada uma das 69 colunas está em
`dados/saida/dicionario_variaveis.csv`, com o código AOP de origem.

## Como carregar

```r
source("R/carregar_base.R")

base <- carregar_base()
tp   <- carregar_base(modo = "transporte_publico", periodo = "pico")
```

**Não leia o CSV direto com `fread()`.** Um arquivo de texto não guarda o tipo
das colunas, então o `fread()` infere "inteiro" para as contagens. O inteiro em
R satura em 2.147.483.647, e `weighted.mean(empregos_60min, w = pop_total)`
multiplica valor por peso antes de somar — basta 1,1 milhão de empregos ao
alcance e 1.905 habitantes para passar do limite, situação de 8% dos hexágonos
habitados. O R devolve `NA` com um aviso discreto: a conta não quebra, ela
silenciosamente deixa de existir. O carregador desarma isso.

Em Python o problema não existe:

```python
import pandas as pd, numpy as np
base = pd.read_csv("dados/saida/acessibilidade_sp_2017_2019.csv")
tp = base.query("modo == 'transporte_publico' and periodo == 'pico' and ano == 2019")
np.average(tp.empregos_60min, weights=tp.pop_total)
```

## Regras de uso

0. **Carregue com `R/carregar_base.R`**, pelo motivo acima. É a única regra
   cuja violação produz um erro invisível.
1. **Sempre pondere pela população.** Os indicadores descrevem *lugares*, não
   pessoas. Um hexágono com 3 mil moradores não pode pesar igual a um com 30.
   Use `weighted.mean(x, w = pop_total)`.
2. **População e renda são do Censo 2010** e são idênticas nos três anos. As
   oportunidades variam ano a ano. A variação da acessibilidade reflete
   mudanças na rede de transporte e nas oportunidades, não demografia.
3. **Compare modos só nos cortes de 15, 30 e 60 minutos.** São os únicos que
   existem em todos os modos. `empregos_45min` só tem valor para caminhada e
   bicicleta; `empregos_90min` e `empregos_120min`, só para transporte público
   e automóvel.
4. **Automóvel só existe em 2019.** Serve para comparar modos naquele ano,
   nunca para tendência.
5. **Célula vazia em `tempo_*` significa "não alcançável"**, não dado
   faltante. Se precisar imputar, o limite de roteamento difere por modo: 60
   minutos na caminhada, 90 na bicicleta e acima de 175 no transporte público.
   Automóvel não tem célula vazia.
6. **`pop_homens` vem de `P007` e `pop_mulheres` de `P006`** — o inverso do que
   o dicionário oficial do `{aopdata}` documenta. A inversão foi detectada
   comparando as somas com o Censo 2010 em três cidades, e está demonstrada no
   notebook. As colunas desta base estão nomeadas pelo conteúdo verificado; se
   você reler o dado direto do `{aopdata}`, o erro volta.

## Fonte

Projeto Acesso a Oportunidades (AOP) do Ipea, acessado pela biblioteca de R
[`{aopdata}`](https://ipeagit.github.io/aopdata/). As estimativas de
acessibilidade foram calculadas pela equipe do Ipea com o `{r5r}`, sobre a
cidade inteira, usando os arquivos GTFS contemporâneos de cada ano.

O projeto não baixa nem armazena rede viária, feeds GTFS ou modelos de
elevação — tudo vem da biblioteca. O notebook explica por que essa foi a
decisão certa e o que se perde ao refazer o cálculo localmente.

## Reproduzir

Requer R e [Quarto](https://quarto.org):

```bash
quarto render notebook/base_acessibilidade_sp_2017_2019.qmd
```

O primeiro processamento baixa os dados do AOP (poucos minutos) e os guarda em
`dados/cache/`. As renderizações seguintes reaproveitam o cache. O notebook
também roda com os chunks executados um a um no console, de qualquer diretório.

Pacotes necessários: `aopdata`, `data.table`, `sf`, `ggplot2`, `knitr`.

## Estrutura

```
notebook/   o notebook (.qmd) e o relatório renderizado (.html)
dashboard/  app shinydashboard: IPTC × % do mercado em 60 min (mapa interativo)
R/          carregar_base.R — o carregador da planilha
dados/
  saida/    a planilha, o dicionário e a grade — é o que se compartilha
  externo/  fontes de fora do AOP: limites dos 96 distritos (GeoSampa) e as
            notas do IPTC 2025 da SPTrans, com as imagens de origem
  cache/    downloads intermediários do AOP (descartável)
gerar_zip.sh  empacota o projeto para distribuição
```
