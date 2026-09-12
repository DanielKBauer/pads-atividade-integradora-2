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

rotulo_hex <- function(d) {
  sprintf(
    paste0(
      "<div style='font-family:system-ui,sans-serif;min-width:200px;line-height:1.35;'>",
      "<div style='font-size:14px;font-weight:700;margin-bottom:6px;'>%s</div>",
      "<table style='width:100%%;font-size:12.5px;border-collapse:collapse;'>",
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
    fmt_pct(d$pct60),
    fmt_num(d$iptc_2025),
    fmt_num(d$gap)
  )
}

rotulos_hex <- setNames(
  lapply(seq_len(nrow(hex)), function(i) HTML(rotulo_hex(hex[i, , drop = FALSE]))),
  hex$id_hex
)

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
  arrange(desc(gap_med)) %>%
  mutate(rank = row_number())

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

        .btn-topo {
          width: 100%; border: none !important; border-radius: 3px; padding: 14px 16px;
          text-align: left; color: #fff !important; position: relative;
          box-shadow: none; outline: none !important; display: block;
          white-space: normal; height: auto !important;
        }
        .btn-topo .rotulo { font-size: 12px; opacity: .9; display: block; }
        .btn-topo .valor { font-size: 28px; font-weight: 700; line-height: 1.15; display: block; margin-top: 2px; }
        .btn-topo .hint { font-size: 11px; opacity: .8; display: block; margin-top: 4px; }
        .btn-iptc { background: #ff851b; }
        .btn-acesso { background: #0073b7; }
        .btn-gap { background: #dd4b39; }
        .btn-topo.ativa { box-shadow: inset 0 0 0 3px #111; filter: brightness(1.05); }
        .btn-topo:hover { filter: brightness(1.08); }

        .tabela-dist { width: 100%; font-size: 12px; border-collapse: collapse; }
        .tabela-dist th {
          text-align: left; color: #6b6560; font-weight: 600; padding: 6px 5px;
          border-bottom: 1px solid #ddd; position: sticky; top: 0; background: #fff; z-index: 1;
        }
        .tabela-dist td { padding: 5px; border-bottom: 1px solid #eee; }
        .tabela-dist tr:hover { background: #f7f3ef; cursor: pointer; }
        .tabela-dist tr.selecionada { background: #efeae3; }
        .tabela-dist .gap-ruim { color: #d73027; font-weight: 700; }
        .tabela-wrap { max-height: 420px; overflow-y: auto; }
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

    fluidRow(
      column(4, actionButton(
        "btn_iptc",
        class = "btn-topo btn-iptc",
        label = tagList(
          tags$span(class = "rotulo", "IPTC da cidade (oficial 2025)"),
          tags$span(class = "valor", sprintf("%.2f", CIDADE_IPTC)),
          tags$span(class = "hint", "Clique para ver a nota no mapa")
        )
      )),
      column(4, actionButton(
        "btn_acesso",
        class = "btn-topo btn-acesso",
        label = tagList(
          tags$span(class = "rotulo", "Acesso médio ponderado · TP ≤60 min"),
          tags$span(class = "valor", fmt_pct(ACESSO_MEDIO)),
          tags$span(class = "hint", "Clique para ver o acesso no mapa")
        )
      )),
      column(4, actionButton(
        "btn_gap",
        class = "btn-topo btn-gap",
        label = tagList(
          tags$span(class = "rotulo", "GAP médio · IPTC − % acesso"),
          tags$span(class = "valor", fmt_num(GAP_MEDIO)),
          tags$span(class = "hint", "Clique para ver o GAP no mapa")
        )
      ))
    ),
    tags$div(
      class = "legenda-topo",
      HTML(paste0(
        "<b style='color:#1a9850;'>Verde</b> = melhor · ",
        "<b style='color:#d73027;'>Vermelho</b> = pior · ",
        "GAP alto = IPTC alto com pouco acesso ao emprego (interpretação livre). · ",
        "Mediana de acesso ", sprintf("%.1f", MEDIANA_ACESSO), "% · ",
        format(dados$n_hex, big.mark = ".", decimal.mark = ",", scientific = FALSE), " hexágonos"
      ))
    ),

    fluidRow(
      box(
        width = 8, solidHeader = FALSE,
        title = uiOutput("titulo_mapa"),
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
              "Planejadas (2025)" = "planejada"
            ),
            selected = c("metro", "cptm")
          )
        ),
        leafletOutput("mapa", height = 560),
        footer = tags$span(
          style = "font-size:11px;color:#6b6560;",
          "Somente o município de São Paulo. Rede: GeoSampa / sp-mapas. IPTC SPTrans 2025 · AOP/Ipea 2019 (TP, pico)."
        )
      ),
      box(
        width = 4, solidHeader = FALSE,
        title = "Distritos — comparar GAP",
        tags$div(
          class = "legenda-topo",
          "Clique para selecionar/desselecionar (vários). Ordenado pelo maior GAP."
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
        tags$div(class = "tabela-wrap", uiOutput("tabela_dist"))
      )
    )
  )
)

server <- function(input, output, session) {

  camada <- reactiveVal("pct60")
  selecionados <- reactiveVal(character(0))

  observeEvent(input$btn_iptc,   { camada("iptc_2025") })
  observeEvent(input$btn_acesso, { camada("pct60") })
  observeEvent(input$btn_gap,    { camada("gap") })
  observeEvent(input$limpar_sel, { selecionados(character(0)) })

  observe({
    session$sendCustomMessage("btnAtivo", list(camada = camada()))
  })
  session$onFlushed(function() {
    session$sendCustomMessage("btnAtivo", list(camada = "pct60"))
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
            "IPTC <b>", fmt_num(r$iptc_2025), "</b><br/>",
            "Acesso <b>", fmt_pct(r$pct60), "</b><br/>",
            "GAP <b style='color:#d73027;'>", fmt_num(r$gap_med), "</b>"
          ))
        )
      )
    })
    tags$div(style = "margin-bottom:10px;", cards)
  })

  output$tabela_dist <- renderUI({
    tab <- tab_distritos
    sel <- selecionados()

    if (!nrow(tab)) {
      return(tags$p(style = "color:#6b6560;", "Nenhum distrito."))
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
        tags$td(style = "text-align:right;", fmt_num(r$iptc_2025)),
        tags$td(style = "text-align:right;", fmt_pct(r$pct60)),
        tags$td(class = "gap-ruim", style = "text-align:right;", fmt_num(r$gap_med))
      )
    })

    tags$table(
      class = "tabela-dist",
      tags$thead(tags$tr(
        tags$th(""),
        tags$th("#"),
        tags$th("Distrito"),
        tags$th(style = "text-align:right;", "IPTC"),
        tags$th(style = "text-align:right;", "Acesso"),
        tags$th(style = "text-align:right;", "GAP")
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
      titulo_legenda <- HTML("Acesso %<br/><small>verde → vermelho</small>")
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
  observe({
    req(!is.null(rede), nrow(rede) > 0)
    mods <- input$rede_modais
    proxy <- leafletProxy("mapa") %>% clearGroup("Rede")

    if (is.null(mods) || !length(mods)) return()

    quer_planejada <- "planejada" %in% mods
    modos <- setdiff(mods, "planejada")
    if (!length(modos)) {
      # só "planejada" marcada → todos os modos planejados
      modos <- c("metro", "cptm", "corredor")
      r <- rede %>% filter(status == "planejada", modal %in% modos)
    } else if (quer_planejada) {
      r <- rede %>% filter(modal %in% modos)
    } else {
      r <- rede %>% filter(status == "existente", modal %in% modos)
    }

    if (!nrow(r)) return()

    # existentes sólidas; planejadas tracejadas
    r_ex <- r %>% filter(status == "existente")
    r_pl <- r %>% filter(status == "planejada")

    if (nrow(r_ex)) {
      proxy <- proxy %>%
        addPolylines(
          data = r_ex, group = "Rede",
          color = ~ifelse(!is.na(cor) & cor != "", cor, COR_MODAL[modal]),
          weight = 3.2, opacity = 0.92,
          options = pathOptions(clickable = FALSE)
        )
    }
    if (nrow(r_pl)) {
      proxy <- proxy %>%
        addPolylines(
          data = r_pl, group = "Rede",
          color = ~ifelse(!is.na(cor) & cor != "", cor, COR_MODAL[modal]),
          weight = 2.4, opacity = 0.75, dashArray = "6,6",
          options = pathOptions(clickable = FALSE)
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
        options = pathOptions(clickable = FALSE)
      )

    bb <- st_bbox(sel_dist)
    proxy %>%
      flyToBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"],
                  options = list(maxZoom = 12, duration = 0.45))
  })
}

shinyApp(ui, server)
