# =============================================================================
# Dashboard SPTrans — IPTC × acessibilidade a empregos (≤60 min)
#
# Layout: 3 botões no topo (médias) + mapa + tabela por distrito
# Verde = melhor · Vermelho = pior  (no GAP, quanto maior, pior)
#
#   shiny::runApp("dashboard")
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(htmltools)
  library(scales)
})

sf_use_s2(FALSE)

.achar_dados <- function() {
  candidatos <- c(
    file.path(getwd(), "data", "camadas.rds"),
    file.path(getwd(), "dashboard", "data", "camadas.rds"),
    file.path(dirname(getwd()), "dashboard", "data", "camadas.rds")
  )
  hit <- candidatos[file.exists(candidatos)]
  if (!length(hit)) stop("Não encontrei dashboard/data/camadas.rds. Rode dashboard/prep_dados.R")
  hit[[1]]
}

dados <- readRDS(.achar_dados())
hex <- dados$hex
dist <- dados$dist
contorno <- dados$contorno
rede <- dados$rede
MEDIANA_ACESSO <- dados$mediana_acesso
CIDADE_IPTC <- dados$cidade_iptc
ACESSO_MEDIO <- dados$acesso_medio

hab <- hex %>% st_drop_geometry() %>% filter(habitado %in% TRUE, !is.na(gap), pop_total > 0)
GAP_MEDIO <- weighted.mean(hab$gap, w = hab$pop_total)

bb_sp <- st_bbox(contorno)
pad <- 0.02
MAX_BOUNDS <- list(
  lng1 = unname(bb_sp["xmin"] - pad), lat1 = unname(bb_sp["ymin"] - pad),
  lng2 = unname(bb_sp["xmax"] + pad), lat2 = unname(bb_sp["ymax"] + pad)
)

PAL_BR <- c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027")
pal_acesso <- colorNumeric(PAL_BR, domain = c(0, max(80, max(hex$pct60, na.rm = TRUE))),
                           reverse = TRUE, na.color = "#d9d8d4")
pal_iptc <- colorNumeric(PAL_BR, domain = range(hex$iptc_2025, na.rm = TRUE),
                         reverse = TRUE, na.color = "#d9d8d4")
pal_gap <- colorNumeric(PAL_BR, domain = range(hex$gap, na.rm = TRUE),
                        reverse = FALSE, na.color = "#d9d8d4")

COR_MODAL <- c(metro = "#003DA5", cptm = "#7F0000", corredor = "#D2641E")
COR_SELECAO <- c("#111111", "#2b6cb0", "#b7791f", "#6b46c1", "#c53030",
                 "#2f855a", "#c05621", "#2c7a7b")

fmt_pct <- function(x) {
  if (length(x) == 0 || is.na(x)) return("—")
  paste0(format(round(x, 1), nsmall = 1, decimal.mark = ","), "%")
}
fmt_num <- function(x) {
  if (length(x) == 0 || is.na(x)) return("—")
  format(round(x, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
}
fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("—")
  format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

tab_distritos <- hex %>%
  st_drop_geometry() %>%
  filter(!is.na(ds_nome)) %>%
  group_by(ds_nome, ds_subpref, iptc_2025, classe_iptc) %>%
  summarise(
    n_hex = n(),
    n_hab = sum(habitado %in% TRUE),
    pop = sum(pop_total, na.rm = TRUE),
    pct60 = if (sum(pop_total, na.rm = TRUE) > 0) {
      weighted.mean(pct60, w = pmax(pop_total, 0), na.rm = TRUE)
    } else {
      mean(pct60, na.rm = TRUE)
    },
    gap_med = median(gap[habitado %in% TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pct60) %>%
  mutate(rank = row_number())

POP_DISTRITO <- setNames(tab_distritos$pop, tab_distritos$ds_nome)

# --- painel executivo: N ajustável via slider, padrão = metade da cidade ----
EXEC_N_DEFAULT <- floor(nrow(tab_distritos) / 2)

rotulo_hex <- function(d) {
  sprintf(
    paste0(
      "<div style='font-family:system-ui,sans-serif;min-width:200px;line-height:1.35;'>",
      "<div style='font-size:14px;font-weight:700;margin-bottom:6px;'>%s</div>",
      "<table style='width:100%%;font-size:12.5px;border-collapse:collapse;'>",
      "<tr><td style='color:#6b6560;padding:2px 8px 2px 0;'>População do distrito</td>",
      "<td style='text-align:right;font-weight:700;'>%s</td></tr>",
      "<tr><td style='color:#6b6560;padding:2px 8px 2px 0;'>Acesso ≤60 min</td>",
      "<td style='text-align:right;font-weight:700;'>%s</td></tr>",
      "<tr><td style='color:#6b6560;padding:2px 8px 2px 0;'>IPTC</td>",
      "<td style='text-align:right;font-weight:700;'>%s</td></tr>",
      "<tr><td style='color:#6b6560;padding:2px 8px 2px 0;'>GAP (IPTC − acesso)</td>",
      "<td style='text-align:right;font-weight:700;color:#d73027;'>%s</td></tr>",
      "</table>",
      "<div style='margin-top:6px;font-size:11px;color:#87857c;'>Quanto maior o GAP, pior</div>",
      "</div>"
    ),
    htmlEscape(as.character(d$ds_nome)),
    fmt_int(POP_DISTRITO[as.character(d$ds_nome)]),
    fmt_num(d$pct60),
    fmt_num(d$iptc_2025),
    fmt_num(d$gap)
  )
}

rotulos_hex <- setNames(
  lapply(seq_len(nrow(hex)), function(i) HTML(rotulo_hex(hex[i, , drop = FALSE]))),
  hex$id_hex
)

# --- rede planejada: impacto marginal (cruza área já bem servida?) ----------
# Para cada trecho planejado, olha os hexágonos habitados num raio de 600m
# (catchment de uma parada) e calcula o acesso médio (ponderado por população)
# já existente ali. Se esse acesso local já é >= média da cidade, o trecho
# cruza uma área bem servida: o ganho marginal de investir ali tende a ser menor.
REDE_PLANEJADA_IMPACTO <- NULL
if (!is.null(rede) && nrow(rede) > 0 && any(rede$status == "planejada")) {
  hex_c <- suppressWarnings(st_centroid(st_geometry(hex)))
  hex_pts_31983 <- st_transform(
    st_sf(id_hex = hex$id_hex, pop_total = hex$pop_total, pct60 = hex$pct60,
          geometry = hex_c, crs = st_crs(hex)),
    31983
  )
  rede_pl <- rede[rede$status == "planejada", ]
  rede_pl_31983 <- st_transform(rede_pl, 31983)
  buf <- st_buffer(rede_pl_31983, dist = 600)
  inter <- st_intersects(hex_pts_31983, buf)

  impacto <- lapply(seq_len(nrow(buf)), function(i) {
    idx <- which(vapply(inter, function(x) i %in% x, logical(1)))
    sub <- hex_pts_31983[idx, ]
    sub <- sub[sub$pop_total > 0, ]
    if (!nrow(sub)) return(c(acesso_local = NA_real_, pop_local = 0))
    c(acesso_local = weighted.mean(sub$pct60, w = sub$pop_total), pop_local = sum(sub$pop_total))
  })
  impacto <- as.data.frame(do.call(rbind, impacto))

  rede_pl$acesso_local <- impacto$acesso_local
  rede_pl$pop_local <- impacto$pop_local
  rede_pl$km <- as.numeric(st_length(rede_pl_31983)) / 1000
  rede_pl$tier <- ifelse(
    is.na(rede_pl$acesso_local), "sem_dados",
    ifelse(rede_pl$acesso_local >= ACESSO_MEDIO, "baixo_impacto", "alto_impacto")
  )
  rede_pl$modal_nome <- c(metro = "Metrô", cptm = "CPTM/trem", corredor = "Corredor")[rede_pl$modal]
  rede_pl$tier_txt <- c(
    alto_impacto  = "cruza área hoje mal servida — maior potencial de impacto",
    baixo_impacto = "cruza área já bem servida — ganho marginal esperado",
    sem_dados     = "sem hexágonos habitados próximos"
  )[rede_pl$tier]
  acesso_local_txt <- ifelse(
    is.na(rede_pl$acesso_local), "—",
    format(round(rede_pl$acesso_local, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
  )
  rede_pl$label_html <- sprintf(
    "<b>%s planejado</b><br/>Acesso local: %s<br/><span style='font-size:11px;'>%s</span>",
    rede_pl$modal_nome,
    acesso_local_txt,
    rede_pl$tier_txt
  )

  REDE_PLANEJADA_IMPACTO <- rede_pl
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "SPTrans · IPTC × Acesso", titleWidth = 260),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f3f0; }
        .box { border-top: 3px solid #52514e; box-shadow: none; border-radius: 2px; }
        #mapa { border: 1px solid #d9d8d4; border-radius: 2px; }
        #mapa, .leaflet-container { background: #eceae4 !important; }
        .leaflet-control-attribution { display: none !important; }
        .left-side, .main-sidebar { display: none !important; }
        .content-wrapper, .main-footer, .right-side { margin-left: 0 !important; }
        .main-header .navbar { margin-left: 0 !important; }
        .main-header .logo { width: auto; padding: 0 18px; }

        .camada-seletor { display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px; }
        .camada-pill {
          border: 1px solid #d9d8d4 !important; background: #fff !important; border-radius: 20px !important;
          padding: 7px 14px !important; font-size: 12.5px; color: #52514e !important; cursor: pointer;
          box-shadow: none !important; outline: none !important; height: auto !important;
          display: inline-flex; align-items: center; gap: 6px; white-space: normal;
        }
        .camada-pill .valor { font-weight: 700; }
        .camada-pill:hover { border-color: #52514e !important; }
        .camada-pill.ativa { color: #fff !important; }
        #btn_acesso.ativa { background: #0073b7 !important; border-color: #0073b7 !important; }
        #btn_iptc.ativa { background: #ff851b !important; border-color: #ff851b !important; }
        #btn_gap.ativa { background: #dd4b39 !important; border-color: #dd4b39 !important; }

        .tabela-dist { width: 100%; font-size: 12px; border-collapse: collapse; }
        .tabela-dist th {
          text-align: left; color: #6b6560; font-weight: 600; padding: 6px 5px;
          border-bottom: 1px solid #ddd; position: sticky; top: 0; background: #fff; z-index: 1;
        }
        .tabela-dist th.ordenavel { cursor: pointer; user-select: none; }
        .tabela-dist th.ordenavel:hover { color: #1f1d1b; }
        .tabela-dist td { padding: 5px; border-bottom: 1px solid #eee; }
        .tabela-dist tr:hover { background: #f7f3ef; cursor: pointer; }
        .tabela-dist tr.selecionada { background: #efeae3; }
        .tabela-dist .gap-ruim { color: #d73027; font-weight: 700; }
        .tabela-wrap { max-height: 1080px; overflow-y: auto; }
        .legenda-topo { font-size: 12px; color: #6b6560; margin: 0 0 8px 0; }
        .rede-box .checkbox { margin-top: 2px; margin-bottom: 2px; }
        .rede-box label { font-weight: 400; font-size: 12.5px; }
        .cmp-card {
          display: inline-block; min-width: 118px; margin: 0 6px 6px 0; padding: 8px 10px;
          border: 1px solid #d9d8d4; border-radius: 3px; background: #fff; vertical-align: top;
        }
        .cmp-card .nome { font-size: 12px; font-weight: 700; margin-bottom: 4px; }
        .cmp-card .meta { font-size: 11px; color: #6b6560; line-height: 1.45; }
        .cmp-card .meta b { color: #1f1d1b; }

        .secao-titulo {
          font-size: 18px; font-weight: 700; color: #1f1d1b; margin: 22px 0 8px 0;
        }
        .exec-subtitulo {
          font-size: 15px; font-weight: 700; color: #1f1d1b; margin: 0 0 8px 0;
          border-left: 4px solid #52514e; padding-left: 10px;
        }
        .exec-headline {
          font-size: 14.5px; line-height: 1.5; padding: 12px 16px; background: #fdf2f1;
          border-left: 4px solid #d73027; border-radius: 3px; margin-bottom: 14px;
        }

        .exec-matriz-wrap { display: flex; gap: 48px; flex-wrap: wrap; align-items: center; }
        .exec-matriz-grid {
          display: grid; grid-template-columns: 190px 190px 190px; gap: 10px;
          align-items: stretch;
        }
        .exec-matriz-corner { }
        .exec-matriz-colhead, .exec-matriz-rowhead {
          font-size: 12px; font-weight: 600; color: #6b6560; display: flex; align-items: center; padding: 4px;
          line-height: 1.3;
        }
        .exec-matriz-rowhead { justify-content: flex-end; text-align: right; }
        .exec-matriz-cel {
          border-radius: 5px; padding: 22px 10px; text-align: center; border: 1px solid #d9d8d4; background: #fff;
        }
        .exec-matriz-cel .exec-matriz-n { font-size: 34px; font-weight: 700; }
        .exec-matriz-cel .exec-matriz-pop { font-size: 11.5px; color: #6b6560; margin-top: 4px; }
        .exec-matriz-cel.exec-matriz-ok { background: #eaf6ee; border-color: #1a9850; }
        .exec-matriz-cel.exec-matriz-ok .exec-matriz-n { color: #1a9850; }
        .exec-matriz-cel.exec-matriz-warn { background: #fef7f0; border-color: #fc8d59; }
        .exec-matriz-cel.exec-matriz-warn .exec-matriz-n { color: #c9660b; }
        .exec-matriz-cel.exec-matriz-bad { background: #fdf2f1; border-color: #d73027; }
        .exec-matriz-cel.exec-matriz-bad .exec-matriz-n { color: #d73027; }
        .exec-matriz-cel.exec-matriz-neutro { background: #f4f3f0; border-color: #d9d8d4; }
        .exec-matriz-cel.exec-matriz-neutro .exec-matriz-n { color: #52514e; }

        .exec-legenda {
          flex: 1 1 480px; min-width: 420px; display: grid;
          grid-template-columns: 1fr 1fr; gap: 18px 32px;
        }
        .exec-legenda-item { display: flex; gap: 10px; align-items: flex-start; }
        .exec-legenda-dot { width: 12px; height: 12px; border-radius: 50%; margin-top: 4px; flex: none; }
        .exec-legenda-titulo { font-size: 13.5px; font-weight: 700; color: #1f1d1b; }
        .exec-legenda-desc { font-size: 12px; color: #6b6560; line-height: 1.45; margin-top: 2px; }

        .exec-barra-row { margin-top: 12px; }
        .exec-barra-titulo { font-size: 12px; font-weight: 600; color: #52514e; margin-bottom: 4px; }
        .exec-barra-track {
          display: flex; width: 100%; height: 22px; border-radius: 3px; overflow: hidden; border: 1px solid #d9d8d4;
        }
        .exec-barra-seg { height: 100%; }
        .exec-barra-alto { background: #0073b7; }
        .exec-barra-baixo { background: #2b2a28; }
        .exec-barra-legenda-num { font-size: 11px; color: #6b6560; margin-top: 3px; }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('btnAtivo', function(msg) {
          ['btn_iptc','btn_acesso','btn_gap'].forEach(function(id){
            var el = document.getElementById(id);
            if (el) el.classList.remove('ativa');
          });
          var map = {iptc_2025:'btn_iptc', pct60:'btn_acesso', gap:'btn_gap'};
          var el = document.getElementById(map[msg.camada]);
          if (el) el.classList.add('ativa');
        });
      "))
    ),

    tags$div(
      class = "legenda-topo",
      HTML(paste0(
        "<b style='color:#1a9850;'>Verde</b> = melhor · ",
        "<b style='color:#d73027;'>Vermelho</b> = pior · ",
        "GAP alto = IPTC alto com pouco acesso ao emprego (interpretação livre). · ",
        "IPTC e Acesso na mesma escala 0–100 (Acesso = % do mercado de empregos alcançável em ≤60 min) · ",
        "Mediana de acesso ", sprintf("%.1f", MEDIANA_ACESSO), " · ",
        format(dados$n_hex, big.mark = ".", decimal.mark = ",", scientific = FALSE), " hexágonos"
      ))
    ),

    tags$div(class = "secao-titulo", "Mapa Interativo"),
    fluidRow(
      box(
        width = 3, solidHeader = FALSE,
        title = "Distritos — comparar acesso",
        tags$div(
          class = "legenda-topo",
          "Clique para selecionar/desselecionar (vários). Ordenado do pior para o melhor acesso."
        ),
        tags$div(
          style = "margin-bottom:8px;",
          actionButton("limpar_sel", "Limpar seleção", class = "btn-sm"),
          tags$span(
            style = "font-size:11px;color:#6b6560;margin-left:8px;",
            textOutput("n_sel", inline = TRUE)
          )
        ),
        uiOutput("comparacao"),
        textInput("busca_dist", NULL, placeholder = "Buscar distrito…", width = "100%"),
        tags$div(class = "tabela-wrap", uiOutput("tabela_dist"))
      ),
      box(
        width = 9, solidHeader = FALSE,
        title = uiOutput("titulo_mapa"),
        tags$div(
          class = "camada-seletor",
          actionButton("btn_acesso", class = "camada-pill", label = tagList(
            "Acesso", tags$span(class = "valor", fmt_num(ACESSO_MEDIO))
          )),
          actionButton("btn_iptc", class = "camada-pill", label = tagList(
            "IPTC", tags$span(class = "valor", sprintf("%.2f", CIDADE_IPTC))
          )),
          actionButton("btn_gap", class = "camada-pill", label = tagList(
            "GAP", tags$span(class = "valor", fmt_num(GAP_MEDIO))
          ))
        ),
        tags$div(
          class = "rede-box",
          style = "margin-bottom:8px;",
          tags$div(
            class = "legenda-topo",
            "Sobreponha a rede existente / planejada para enxergar lacunas e onde uma nova linha faria sentido."
          ),
          checkboxGroupInput(
            "rede_modais",
            label = NULL,
            inline = TRUE,
            choices = c(
              "Metrô" = "metro",
              "CPTM / trem" = "cptm",
              "Corredor ônibus" = "corredor",
              "Planejada / expansão prevista" = "planejada"
            ),
            selected = c("metro", "cptm")
          ),
          tags$div(
            class = "legenda-topo",
            style = "margin-top:-4px;",
            HTML(
              "<i>Planejada</i> = camada oficial de expansão da rede (GeoSampa: metrô, trem e corredores ",
              "planejados com horizonte 2025 na fonte). Não é uma promessa de entrega — parte pode já ter ",
              "sido inaugurada ou adiada desde a coleta dos dados. A cor da linha tracejada não indica o modal ",
              "(metrô/trem/corredor) — indica o potencial de impacto (veja o painel executivo abaixo): ",
              "<b style='color:#0073b7;'>azul</b> cruza área hoje mal servida, <b style='color:#2b2a28;'>preta</b> já é bem servida."
            )
          )
        ),
        leafletOutput("mapa", height = 1050),
        footer = tags$span(
          style = "font-size:11px;color:#6b6560;",
          "Somente o município de São Paulo. Rede: GeoSampa / sp-mapas. IPTC SPTrans 2025 · AOP/Ipea 2019 (TP, pico)."
        )
      )
    ),

    fluidRow(
      box(
        width = 12, solidHeader = FALSE,
        title = tags$span(class = "secao-titulo", style = "margin:0;", "Principais Oportunidades"),
        collapsible = TRUE, collapsed = FALSE,

        tags$div(class = "exec-subtitulo", "Distritos que ficariam de fora da priorização via IPTC"),
        tags$div(
          class = "legenda-topo",
          "Esta seção mostra o que mudaria se a priorização levasse em conta o acesso real a emprego em ",
          "≤60 min — usando os mesmos dados e a mesma agregação por distrito do mapa e da tabela acima."
        ),
        sliderInput(
          "exec_topn",
          sprintf("Quantos distritos considerar como \"prioritários\" (top N piores, de %d no total)", nrow(tab_distritos)),
          min = 5, max = 90, value = EXEC_N_DEFAULT, step = 1, width = "100%"
        ),
        uiOutput("exec_headline"),
        tags$div(
          class = "exec-matriz-wrap",
          uiOutput("exec_matriz"),
          tags$div(class = "exec-legenda", uiOutput("exec_legenda"))
        ),
        tags$div(
          style = "margin-top:12px;",
          actionButton(
            "exec_ver_mapa",
            "Ver no mapa os distritos que o IPTC deixaria de fora",
            class = "btn-sm"
          )
        ),

        tags$hr(),

        tags$div(class = "exec-subtitulo", "Análise dos investimentos planejados"),
        tags$div(
          class = "legenda-topo",
          "Para cada trecho planejado (metrô, CPTM, corredor), olhamos o acesso já existente num raio de ",
          "600m (catchment de uma parada). Se essa área já tem acesso acima da média da cidade, o ganho ",
          "marginal de investir ali tende a ser menor — gasto vs. retorno pior."
        ),
        uiOutput("exec_rede_planejada")
      )
    )
  )
)

server <- function(input, output, session) {

  camada <- reactiveVal("pct60")
  selecionados <- reactiveVal(character(0))
  ordenacao <- reactiveVal(list(col = "pct60", dir = "asc"))

  observeEvent(input$btn_iptc,   { camada("iptc_2025") })
  observeEvent(input$btn_acesso, { camada("pct60") })
  observeEvent(input$btn_gap,    { camada("gap") })
  observeEvent(input$limpar_sel, { selecionados(character(0)) })

  # --- ordenação da tabela (clique no cabeçalho, tipo planilha) ----------
  DIR_PADRAO_COL <- c(pct60 = "asc", iptc_2025 = "asc", gap_med = "desc")
  observeEvent(input$tabela_sort_col, {
    col <- input$tabela_sort_col
    atual <- ordenacao()
    novo_dir <- if (identical(atual$col, col)) {
      if (atual$dir == "asc") "desc" else "asc"
    } else {
      DIR_PADRAO_COL[[col]]
    }
    ordenacao(list(col = col, dir = novo_dir))
  }, ignoreInit = TRUE)

  # --- busca de distrito (tabela lateral) ---------------------------------
  tab_distritos_filtrado <- reactive({
    termo <- trimws(input$busca_dist %||% "")
    if (!nzchar(termo)) return(tab_distritos)
    alvo <- tolower(iconv(termo, to = "ASCII//TRANSLIT"))
    nomes <- tolower(iconv(as.character(tab_distritos$ds_nome), to = "ASCII//TRANSLIT"))
    tab_distritos[grepl(alvo, nomes, fixed = TRUE), , drop = FALSE]
  })

  tab_distritos_ordenado <- reactive({
    tab <- tab_distritos_filtrado()
    o <- ordenacao()
    vals <- tab[[o$col]]
    ord <- if (o$dir == "asc") order(vals) else order(-vals)
    tab[ord, , drop = FALSE]
  })

  # --- painel executivo: IPTC vs. acesso real (N ajustável pelo slider) --
  exec_L <- reactive({
    n <- input$exec_topn
    req(n)
    piores_iptc   <- tab_distritos %>% arrange(iptc_2025) %>% slice_head(n = n) %>% pull(ds_nome)
    piores_acesso <- tab_distritos %>% arrange(pct60) %>% slice_head(n = n) %>% pull(ds_nome)
    list(
      n         = n,
      iptc      = piores_iptc,
      acesso    = piores_acesso,
      overlap   = intersect(piores_iptc, piores_acesso),
      so_acesso = setdiff(piores_acesso, piores_iptc),
      so_iptc   = setdiff(piores_iptc, piores_acesso)
    )
  })

  output$exec_headline <- renderUI({
    L <- exec_L()
    pop_cidade  <- sum(tab_distritos$pop)
    pop_perdida <- sum(tab_distritos$pop[tab_distritos$ds_nome %in% L$so_acesso])
    tags$div(
      class = "exec-headline",
      HTML(sprintf(
        "<b>%d distritos</b> (%s pessoas, %s da cidade) estão entre os piores em acesso real a emprego, mas <b>ficariam de fora</b> da priorização pelo IPTC.",
        length(L$so_acesso), fmt_int(pop_perdida), fmt_pct(100 * pop_perdida / pop_cidade)
      ))
    )
  })

  observeEvent(input$exec_ver_mapa, {
    selecionados(exec_L()$so_acesso)
  })

  output$exec_matriz <- renderUI({
    L <- exec_L()
    n <- L$n
    resto <- setdiff(tab_distritos$ds_nome, union(L$iptc, L$acesso))

    celula <- function(classe, n_dist) {
      tags$div(
        class = paste("exec-matriz-cel", classe),
        tags$div(class = "exec-matriz-n", n_dist),
        tags$div(class = "exec-matriz-pop", "distritos")
      )
    }

    tags$div(
      class = "exec-matriz-grid",
      tags$div(class = "exec-matriz-corner"),
      tags$div(class = "exec-matriz-colhead", sprintf("Entre os %d piores em acesso", n)),
      tags$div(class = "exec-matriz-colhead", "NÃO está entre os piores"),

      tags$div(class = "exec-matriz-rowhead", sprintf("Priorizado pelo IPTC (top %d)", n)),
      celula("exec-matriz-ok", length(L$overlap)),
      celula("exec-matriz-warn", length(L$so_iptc)),

      tags$div(class = "exec-matriz-rowhead", "NÃO priorizado pelo IPTC"),
      celula("exec-matriz-bad", length(L$so_acesso)),
      celula("exec-matriz-neutro", length(resto))
    )
  })

  output$exec_legenda <- renderUI({
    L <- exec_L()
    resto <- setdiff(tab_distritos$ds_nome, union(L$iptc, L$acesso))
    pop_de <- function(nomes) sum(tab_distritos$pop[tab_distritos$ds_nome %in% nomes])

    item <- function(cor, titulo, n_dist, pop_dist, desc) {
      tags$div(
        class = "exec-legenda-item",
        tags$span(class = "exec-legenda-dot", style = sprintf("background:%s;", cor)),
        tags$div(
          tags$div(class = "exec-legenda-titulo", sprintf(
            "%s — %d distritos, %s pessoas", titulo, n_dist, fmt_int(pop_dist)
          )),
          tags$div(class = "exec-legenda-desc", desc)
        )
      )
    }

    tagList(
      item("#1a9850", "Acerto", length(L$overlap), pop_de(L$overlap),
           "Priorizado pelo IPTC e também entre os piores em acesso real — a priorização atual funcionaria aqui."),
      item("#c9660b", "Prioridade desperdiçada", length(L$so_iptc), pop_de(L$so_iptc),
           "Priorizado pelo IPTC, mas não está entre os piores em acesso real."),
      item("#d73027", "Oportunidade perdida", length(L$so_acesso), pop_de(L$so_acesso),
           "Fora da priorização do IPTC, mas está entre os piores em acesso real — ficaria sem melhorias."),
      item("#9c9a94", "Acerto por omissão", length(resto), pop_de(resto),
           "Corretamente não priorizado nos dois critérios.")
    )
  })

  output$exec_rede_planejada <- renderUI({
    if (is.null(REDE_PLANEJADA_IMPACTO)) {
      return(tags$p(style = "color:#6b6560;", "Sem dados de rede planejada."))
    }
    rp <- REDE_PLANEJADA_IMPACTO %>% st_drop_geometry()

    resumo_tier <- function(df) {
      r <- df %>% group_by(tier) %>% summarise(km = sum(km), .groups = "drop")
      c(
        alto  = sum(r$km[r$tier == "alto_impacto"]),
        baixo = sum(r$km[r$tier == "baixo_impacto"])
      )
    }

    barra <- function(km_alto, km_baixo, titulo) {
      total <- km_alto + km_baixo
      if (total <= 0) return(NULL)
      p_alto <- 100 * km_alto / total
      tags$div(
        class = "exec-barra-row",
        tags$div(class = "exec-barra-titulo", titulo),
        tags$div(
          class = "exec-barra-track",
          tags$div(class = "exec-barra-seg exec-barra-alto", style = sprintf("width:%.2f%%;", p_alto)),
          tags$div(class = "exec-barra-seg exec-barra-baixo", style = sprintf("width:%.2f%%;", 100 - p_alto))
        ),
        tags$div(class = "exec-barra-legenda-num", HTML(sprintf(
          "<b style='color:#0073b7;'>%s km</b> em área mal servida (%.0f%%) · <b style='color:#2b2a28;'>%s km</b> em área já bem servida (%.0f%%)",
          fmt_num(km_alto), p_alto, fmt_num(km_baixo), 100 - p_alto
        )))
      )
    }

    nomes_modal <- c(metro = "Metrô", cptm = "CPTM/trem", corredor = "Corredor")
    linhas_modal <- lapply(names(nomes_modal), function(mod) {
      sub <- resumo_tier(rp %>% filter(modal == mod))
      barra(sub["alto"], sub["baixo"], nomes_modal[[mod]])
    })

    geral <- resumo_tier(rp)
    tagList(
      barra(geral["alto"], geral["baixo"], "Rede planejada (total)"),
      tags$div(style = "margin-top:2px;", Filter(Negate(is.null), linhas_modal))
    )
  })

  observe({
    session$sendCustomMessage("btnAtivo", list(camada = camada()))
  })
  session$onFlushed(function() {
    session$sendCustomMessage("btnAtivo", list(camada = "pct60"))
    # o container do mapa só atinge a altura final (CSS) depois do primeiro
    # flush; sem isso o fitBounds inicial calcula o zoom com base num
    # tamanho de container ainda pequeno e abre zoomed out demais.
    proxy <- leafletProxy("mapa")
    invokeMethod(proxy, data = NULL, "invalidateSize")
    proxy %>% fitBounds(MAX_BOUNDS$lng1, MAX_BOUNDS$lat1, MAX_BOUNDS$lng2, MAX_BOUNDS$lat2)
  }, once = TRUE)

  observeEvent(input$dist_toggle, {
    nome <- input$dist_toggle
    if (is.null(nome) || !nzchar(nome)) return()
    atual <- selecionados()
    if (nome %in% atual) {
      selecionados(setdiff(atual, nome))
    } else {
      selecionados(c(atual, nome))
    }
  }, ignoreInit = TRUE)

  output$n_sel <- renderText({
    n <- length(selecionados())
    if (!n) "nenhum distrito"
    else if (n == 1) "1 distrito"
    else paste(n, "distritos")
  })

  output$titulo_mapa <- renderUI({
    tit <- switch(camada(),
      pct60     = "Acesso a empregos em ≤60 min — verde = melhor, vermelho = pior",
      iptc_2025 = "Nota IPTC (2025) — verde = melhor, vermelho = pior",
      gap       = "GAP (IPTC − % acesso) — quanto maior, pior (vermelho)"
    )
    tags$span(style = "font-weight:600;", tit)
  })

  output$comparacao <- renderUI({
    sel <- selecionados()
    if (!length(sel)) return(NULL)
    tab <- tab_distritos %>% filter(ds_nome %in% sel)
    if (!nrow(tab)) return(NULL)
    # mantém a ordem de seleção
    tab <- tab[match(sel, tab$ds_nome), , drop = FALSE]
    cards <- lapply(seq_len(nrow(tab)), function(i) {
      r <- tab[i, ]
      cor <- COR_SELECAO[((i - 1) %% length(COR_SELECAO)) + 1]
      tags$div(
        class = "cmp-card",
        style = paste0("border-left: 4px solid ", cor, ";"),
        tags$div(class = "nome", htmlEscape(as.character(r$ds_nome))),
        tags$div(
          class = "meta",
          HTML(paste0(
            "Acesso <b>", fmt_num(r$pct60), "</b><br/>",
            "IPTC <b>", fmt_num(r$iptc_2025), "</b><br/>",
            "GAP <b style='color:#d73027;'>", fmt_num(r$gap_med), "</b>"
          ))
        )
      )
    })
    tags$div(style = "margin-bottom:10px;", cards)
  })

  output$tabela_dist <- renderUI({
    tab <- tab_distritos_ordenado()
    sel <- selecionados()
    o <- ordenacao()

    if (!nrow(tab)) {
      return(tags$p(style = "color:#6b6560;", "Nenhum distrito encontrado."))
    }

    seta <- function(col) {
      if (!identical(o$col, col)) return("")
      if (o$dir == "asc") " ▲" else " ▼"
    }
    th_ord <- function(col, rotulo) {
      tags$th(
        class = "ordenavel", style = "text-align:right;",
        onclick = sprintf("Shiny.setInputValue('tabela_sort_col','%s',{priority:'event'})", col),
        paste0(rotulo, seta(col))
      )
    }

    linhas <- lapply(seq_len(nrow(tab)), function(i) {
      r <- tab[i, ]
      marcado <- r$ds_nome %in% sel
      tags$tr(
        class = if (marcado) "selecionada" else NULL,
        onclick = sprintf(
          "Shiny.setInputValue('dist_toggle', '%s', {priority: 'event'})",
          gsub("'", "\\\\'", r$ds_nome)
        ),
        tags$td(style = "width:22px;text-align:center;", if (marcado) "✓" else ""),
        tags$td(style = "color:#87857c;width:26px;", i),
        tags$td(htmlEscape(as.character(r$ds_nome))),
        tags$td(style = "text-align:right;", fmt_num(r$pct60)),
        tags$td(style = "text-align:right;", fmt_num(r$iptc_2025)),
        tags$td(class = "gap-ruim", style = "text-align:right;", fmt_num(r$gap_med))
      )
    })

    tags$table(
      class = "tabela-dist",
      tags$thead(tags$tr(
        tags$th(""),
        tags$th("#"),
        tags$th("Distrito"),
        th_ord("pct60", "Acesso"),
        th_ord("iptc_2025", "IPTC"),
        th_ord("gap_med", "GAP")
      )),
      tags$tbody(linhas)
    )
  })

  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = TRUE,
      minZoom = 10,
      maxZoom = 14,
      attributionControl = FALSE
    )) %>%
      # panes com z-index fixo: garante que rede e destaque fiquem sempre
      # acima dos hexágonos, mesmo quando cada camada é redesenhada em
      # momentos diferentes (uma independe da outra)
      addMapPane("panoRede", zIndex = 410) %>%
      addMapPane("panoDestaque", zIndex = 420) %>%
      setMaxBounds(MAX_BOUNDS$lng1, MAX_BOUNDS$lat1, MAX_BOUNDS$lng2, MAX_BOUNDS$lat2) %>%
      fitBounds(MAX_BOUNDS$lng1, MAX_BOUNDS$lat1, MAX_BOUNDS$lng2, MAX_BOUNDS$lat2) %>%
      addPolygons(
        data = contorno,
        fillColor = "#f7f5f1", fillOpacity = 1,
        color = "#1f1d1b", weight = 2.4, opacity = 0.95,
        group = "Contorno", options = pathOptions(clickable = FALSE)
      )
  })

  observe({
    d <- hex
    c <- camada()

    if (c == "pct60") {
      valores <- d$pct60
      pal <- pal_acesso
      titulo_legenda <- HTML("Acesso<br/><small>verde → vermelho</small>")
      dominio <- c(0, max(80, max(hex$pct60, na.rm = TRUE)))
    } else if (c == "iptc_2025") {
      valores <- d$iptc_2025
      pal <- pal_iptc
      titulo_legenda <- HTML("IPTC<br/><small>verde → vermelho</small>")
      dominio <- range(hex$iptc_2025, na.rm = TRUE)
    } else {
      valores <- d$gap
      pal <- pal_gap
      titulo_legenda <- HTML("GAP<br/><small>maior = pior</small>")
      dominio <- range(hex$gap, na.rm = TRUE)
    }

    labs <- unname(rotulos_hex[d$id_hex])

    leafletProxy("mapa", data = d) %>%
      clearGroup("Hexágonos") %>%
      clearGroup("Contorno") %>%
      clearControls() %>%
      addPolygons(
        data = contorno,
        fillColor = "#f7f5f1", fillOpacity = 1,
        color = "#1f1d1b", weight = 2.4, opacity = 0.95,
        group = "Contorno", options = pathOptions(clickable = FALSE)
      ) %>%
      addPolygons(
        group = "Hexágonos",
        layerId = ~id_hex,
        fillColor = pal(valores),
        fillOpacity = 0.88,
        color = "#ffffff",
        weight = 0.25,
        opacity = 0.35,
        label = labs,
        labelOptions = labelOptions(
          style = list(
            "background" = "rgba(255,255,255,0.96)",
            "border" = "1px solid #d6d1ca",
            "padding" = "8px 10px",
            "box-shadow" = "0 4px 14px rgba(0,0,0,.12)"
          ),
          textsize = "13px", direction = "auto", opacity = 1
        ),
        highlightOptions = highlightOptions(
          weight = 2.2, color = "#1f1d1b", fillOpacity = 0.98, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, values = dominio,
        title = titulo_legenda, opacity = 0.9, labFormat = labelFormat(digits = 0)
      )
  })

  # overlay de rede modal
  # Metrô/CPTM/Corredor controlam só a rede EXISTENTE. "Planejada" é um toggle
  # independente: quando marcado, sempre mostra a rede planejada inteira
  # (todos os modais), sem depender de quais modais estão marcados ao lado.
  observe({
    req(!is.null(rede), nrow(rede) > 0)
    mods <- input$rede_modais
    proxy <- leafletProxy("mapa") %>% clearGroup("Rede")

    if (is.null(mods) || !length(mods)) return()

    modos_existente <- intersect(mods, c("metro", "cptm", "corredor"))
    quer_planejada  <- "planejada" %in% mods

    r_ex <- if (length(modos_existente)) {
      rede %>% filter(status == "existente", modal %in% modos_existente)
    } else {
      rede[0, ]
    }
    r_pl <- if (quer_planejada && !is.null(REDE_PLANEJADA_IMPACTO)) {
      REDE_PLANEJADA_IMPACTO
    } else {
      NULL
    }

    if (nrow(r_ex)) {
      proxy <- proxy %>%
        addPolylines(
          data = r_ex, group = "Rede",
          color = ~ifelse(!is.na(cor) & cor != "", cor, COR_MODAL[modal]),
          weight = 3.2, opacity = 0.92,
          options = pathOptions(clickable = FALSE, pane = "panoRede")
        )
    }
    if (!is.null(r_pl) && nrow(r_pl)) {
      cor_tier <- c(alto_impacto = "#0073b7", baixo_impacto = "#2b2a28", sem_dados = "#9c9a94")
      proxy <- proxy %>%
        addPolylines(
          data = r_pl, group = "Rede",
          color = ~unname(cor_tier[tier]),
          weight = 2.4, opacity = 0.75, dashArray = "6,6",
          label = lapply(r_pl$label_html, HTML),
          labelOptions = labelOptions(
            style = list(
              "background" = "rgba(255,255,255,0.96)",
              "border" = "1px solid #d6d1ca",
              "padding" = "6px 9px"
            ),
            textsize = "12px"
          ),
          options = pathOptions(clickable = TRUE, pane = "panoRede")
        )
    }
  })

  # seleção múltipla → destaque + zoom no conjunto
  observe({
    sel <- selecionados()
    proxy <- leafletProxy("mapa") %>% clearGroup("Destaque")
    if (!length(sel)) return()

    sel_dist <- dist[as.character(dist$ds_nome) %in% sel, ]
    if (!nrow(sel_dist)) return()

    # ordena na mesma ordem da seleção para cores estáveis
    ordem <- match(sel, as.character(sel_dist$ds_nome))
    sel_dist <- sel_dist[ordem[!is.na(ordem)], ]
    cores <- COR_SELECAO[((seq_len(nrow(sel_dist)) - 1) %% length(COR_SELECAO)) + 1]

    proxy %>%
      addPolygons(
        data = sel_dist, group = "Destaque",
        fill = FALSE, color = cores, weight = 2.8, opacity = 1,
        options = pathOptions(clickable = FALSE, pane = "panoDestaque")
      )

    bb <- st_bbox(sel_dist)
    proxy %>%
      flyToBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"],
                  options = list(maxZoom = 12, duration = 0.45))
  })
}

shinyApp(ui, server)
