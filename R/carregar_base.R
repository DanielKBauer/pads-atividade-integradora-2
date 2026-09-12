# =============================================================================
# carregar_base.R — ponto de entrada para ler a planilha de acessibilidade
#
# Use este script em vez de ler o CSV diretamente com fread().
#
# Motivo: um arquivo CSV não guarda o tipo das colunas. O fread() infere
# "inteiro" para as contagens, e o inteiro em R satura em 2.147.483.647. Como
# weighted.mean(empregos, w = pop_total) multiplica valor por peso antes de
# somar, basta um hexágono com 1,1 milhão de empregos ao alcance e 1.905
# habitantes para estourar o limite — e 8% dos hexágonos habitados de São Paulo
# estão nessa situação. O R então devolve NA com um aviso discreto: a conta não
# quebra, ela silenciosamente deixa de existir.
#
# As funções abaixo convertem as contagens para double, o que elimina o
# problema. Detalhes na seção "Uma armadilha de tipo" do notebook.
#
# Uso:
#   source("R/carregar_base.R")
#   base <- carregar_base()
#   tp   <- carregar_base(modo = "transporte_publico", periodo = "pico")
# =============================================================================

suppressPackageStartupMessages(library(data.table))

ARQUIVO_PLANILHA   <- "acessibilidade_sp_2017_2019.csv"
ARQUIVO_DICIONARIO <- "dicionario_variaveis.csv"
ARQUIVO_GRADE      <- "grade_hexagonos_sp.gpkg"

# Localiza dados/saida subindo diretórios, para funcionar tanto da raiz do
# projeto quanto de dentro de notebook/ ou R/.
.dir_saida <- function() {
  atual <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 1:6) {
    candidato <- file.path(atual, "dados", "saida")
    if (file.exists(file.path(candidato, ARQUIVO_PLANILHA))) return(candidato)
    pai <- dirname(atual)
    if (identical(pai, atual)) break
    atual <- pai
  }
  stop("Nao encontrei dados/saida/", ARQUIVO_PLANILHA,
       " a partir de: ", getwd())
}

# Converte para double toda coluna de contagem. Preserva como inteiro apenas
# o que e identificador ou categoria, e portanto nunca entra em multiplicacao:
# ano, code_muni e as faixas de renda.
.NAO_CONVERTER <- c("ano", "code_muni", "quintil_renda", "decil_renda")

.para_double <- function(dt) {
  colunas <- names(dt)[vapply(dt, is.integer, logical(1))]
  colunas <- setdiff(colunas, .NAO_CONVERTER)
  if (length(colunas)) {
    dt[, (colunas) := lapply(.SD, as.numeric), .SDcols = colunas]
  }
  dt[]
}

#' Carrega a planilha de acessibilidade, com os tipos corrigidos
#'
#' @param modo Filtra o modo de transporte. Um ou mais de
#'   "transporte_publico", "caminhada", "bicicleta", "automovel".
#'   NULL (padrao) devolve todos.
#' @param periodo Filtra o periodo do dia. Um ou mais de "pico",
#'   "fora_pico", "nao_se_aplica". NULL (padrao) devolve todos.
#' @param ano Filtra o ano: 2017, 2018 e/ou 2019. NULL devolve todos.
#'
#' @return data.table com uma linha por hexagono x ano x modo x periodo.
carregar_base <- function(modo = NULL, periodo = NULL, ano = NULL) {
  base <- .para_double(fread(file.path(.dir_saida(), ARQUIVO_PLANILHA)))

  # Nomes distintos dos das colunas: dentro de [ ], 'modo' resolveria para a
  # coluna e o filtro se compararia consigo mesmo.
  aplicar_filtro <- function(dt, coluna, valores) {
    if (is.null(valores)) return(dt)
    disponiveis <- unique(dt[[coluna]])
    desconhecidos <- setdiff(valores, disponiveis)
    if (length(desconhecidos)) {
      stop("Valor inexistente em '", coluna, "': ",
           paste(desconhecidos, collapse = ", "),
           ". Disponiveis: ", paste(sort(disponiveis), collapse = ", "),
           call. = FALSE)
    }
    dt[dt[[coluna]] %in% valores]
  }

  base <- aplicar_filtro(base, "modo", modo)
  base <- aplicar_filtro(base, "periodo", periodo)
  base <- aplicar_filtro(base, "ano", ano)

  if (!nrow(base)) stop("A combinacao de filtros nao retornou nenhuma linha.")

  # Falha alto se a armadilha de tipo voltar
  teste <- base[, weighted.mean(empregos_60min, w = pop_total, na.rm = TRUE)]
  if (!is.finite(teste)) {
    stop("A media ponderada devolveu NA: a conversao de tipo falhou.",
         call. = FALSE)
  }

  base[]
}

#' Dicionario de variaveis: o significado de cada coluna da planilha
carregar_dicionario <- function() {
  fread(file.path(.dir_saida(), ARQUIVO_DICIONARIO))
}

#' Grade hexagonal com geometria, para mapas. Requer o pacote {sf}.
carregar_grade <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("O pacote {sf} e necessario para ler a grade geografica.",
         call. = FALSE)
  }
  sf::st_read(file.path(.dir_saida(), ARQUIVO_GRADE), quiet = TRUE)
}
