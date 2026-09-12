# Regenera dashboard/data/camadas.rds
# Grade completa de São Paulo (com e sem população), sem hexágonos fora do município.
#
# Rode na raiz do projeto:
#   Rscript dashboard/prep_dados.R

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
})

sf_use_s2(FALSE)

args_raiz <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_raiz, value = TRUE)
raiz <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."))
} else if (basename(normalizePath(".")) == "dashboard") {
  normalizePath("..")
} else {
  normalizePath(".")
}

message("raiz: ", raiz)

# --- limites do município (96 distritos) --------------------------------------
dist_geo <- st_read(file.path(raiz, "dados/externo/distritos-sp.geojson"), quiet = TRUE)
dist_geo$ds_codigo <- as.integer(dist_geo$ds_codigo)
dist_geo <- dist_geo[, c("ds_codigo", "ds_nome", "ds_subpref")]

dmet <- fread(file.path(raiz, "dados/saida/iptc_custo_commute_distritos.csv"))
dist <- dist_geo %>%
  left_join(
    as.data.frame(dmet) %>%
      select(ds_codigo, iptc_2025, classe_iptc, pop, hexagonos, pct60, pct60_car,
             penal_car, dep_pico, renda, deficit, ponto_cego),
    by = "ds_codigo"
  )

contorno <- st_make_valid(st_union(st_geometry(dist_geo)))
contorno_sf <- st_sf(geom = contorno, crs = st_crs(dist_geo))

# --- grade H3 completa + métricas de acessibilidade (todos os hexágonos) ------
hex <- st_read(file.path(raiz, "dados/saida/grade_hexagonos_sp.gpkg"), quiet = TRUE)
dt <- fread(file.path(raiz, "dados/saida/acessibilidade_sp_2017_2019.csv"), encoding = "UTF-8")
setnames(dt, names(dt), gsub("^\ufeff", "", names(dt)))

tp <- dt[ano == 2019 & modo == "transporte_publico" & periodo == "pico"]
TOT19 <- sum(tp$empregos_total)
tp[, `:=`(
  pct60 = empregos_60min / TOT19 * 100,
  pea = pop_19a24 + pop_25a39 + pop_40a69
)]

tpf <- dt[ano == 2019 & modo == "transporte_publico" & periodo == "fora_pico",
          .(id_hex, emp60_fp = empregos_60min)]
car <- dt[ano == 2019 & modo == "automovel" & periodo == "pico",
          .(id_hex, emp60_car = empregos_60min)]

iptc <- fread(file.path(raiz, "dados/externo/iptc_2025_distritos.csv"))

met <- tp[, .(
  id_hex, lon, lat, pop_total, renda_per_capita, decil_renda,
  empregos_total, empregos_60min = empregos_60min, pct60, pea
)]
met <- merge(met, tpf, by = "id_hex", all.x = TRUE)
met <- merge(met, car, by = "id_hex", all.x = TRUE)
met[, `:=`(
  pct60_car = emp60_car / TOT19 * 100,
  dep_pico = (empregos_60min - emp60_fp) / fifelse(emp60_fp > 0, emp60_fp, NA_real_) * 100,
  penal_car = emp60_car / fifelse(empregos_60min > 0, empregos_60min, NA_real_)
)]

# --- só hexágonos que intersectam São Paulo; geometria cortada no contorno ----
message("filtrando grade ao município…")
hex <- st_make_valid(hex)
hit <- lengths(st_intersects(hex, contorno_sf)) > 0
message("grade original: ", nrow(hex), " · dentro/tocando SP: ", sum(hit),
        " · removidos fora: ", sum(!hit))
hex <- hex[hit, ]

message("recortando hexágonos ao contorno do município…")
hex <- suppressWarnings(st_intersection(hex, contorno_sf))
# st_intersection pode gerar GeometryCollection — força polígono
hex <- st_collection_extract(hex, "POLYGON")
hex <- hex[!st_is_empty(hex), ]
hex <- hex %>%
  group_by(id_hex) %>%
  summarise(.groups = "drop")   # sf une geometrias do grupo
hex <- st_make_valid(hex)

# distrito pelo ponto interior (já dentro do município após o clip)
cent <- st_point_on_surface(st_geometry(hex))
pts <- st_sf(id_hex = hex$id_hex, geometry = cent, crs = st_crs(hex))
liga <- st_join(pts, dist_geo, join = st_within)
orf <- is.na(liga$ds_codigo)
if (any(orf)) {
  nn <- st_join(
    st_transform(pts[orf, ], 31983),
    st_transform(dist_geo, 31983),
    join = st_nearest_feature
  )
  liga$ds_codigo[orf] <- nn$ds_codigo
  liga$ds_nome[orf] <- nn$ds_nome
  liga$ds_subpref[orf] <- nn$ds_subpref
}
liga_df <- data.frame(
  id_hex = liga$id_hex,
  ds_codigo = as.integer(liga$ds_codigo),
  ds_nome = as.character(liga$ds_nome),
  ds_subpref = as.character(liga$ds_subpref),
  stringsAsFactors = FALSE
)

# base = todos os hexágonos em SP + métricas de acessibilidade
met <- as.data.table(merge(liga_df, as.data.frame(met), by = "id_hex", all.x = TRUE))
met <- merge(met, iptc, by = "ds_codigo", all.x = TRUE)

CLASSES <- data.table(
  lim = c(63.6, 69.3, 73.4, 82.0, 101),
  classe = c("Muito Baixa", "Baixa", "Média", "Alta", "Muito Alta")
)
classe_de <- function(v) {
  CLASSES$classe[findInterval(v, c(-Inf, CLASSES$lim), rightmost.closed = TRUE)]
}
met[, classe_iptc := classe_de(iptc_2025)]

# mediana e ponto cego só com população > 0 (mesmo critério do EDA)
hab <- met[pop_total > 0]
mediana_pond <- function(valores, pesos) {
  o <- order(valores)
  v <- valores[o]
  w <- pesos[o]
  csum <- cumsum(w) - 0.5 * w
  approx(csum, v, xout = 0.5 * sum(w))$y
}
med <- mediana_pond(hab$pct60, hab$pop_total)
cidade_iptc <- 72.16
met[, ponto_cego := pop_total > 0 & iptc_2025 >= cidade_iptc & pct60 < med]
met[, gap := iptc_2025 - pct60]
met[, deficit := pmax(0, pop_total * (med - pct60))]
met[, habitado := pop_total > 0]

g <- hex %>% left_join(as.data.frame(met), by = "id_hex")
g <- st_simplify(g, dTolerance = 1e-5, preserveTopology = TRUE)

# distritos também recortados visualmente já são o contorno
outdir <- file.path(raiz, "dashboard/data")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# --- linhas modais (GeoSampa / sp-mapas) — overlay para decidir novas linhas ---
rede_dir <- file.path(raiz, "dados/externo/rede_tp")
ler_linha <- function(arq, modal, status) {
  p <- file.path(rede_dir, arq)
  if (!file.exists(p)) return(NULL)
  x <- st_read(p, quiet = TRUE)
  x <- st_make_valid(x)
  x <- suppressWarnings(st_intersection(x, contorno_sf))
  if (!nrow(x)) return(NULL)
  nm <- if ("NUM_NOME" %in% names(x)) as.character(x$NUM_NOME) else NA_character_
  cor <- if ("COLOR" %in% names(x)) as.character(x$COLOR) else NA_character_
  st_sf(
    modal = modal,
    status = status,
    nome = nm,
    cor = cor,
    geometry = st_geometry(x),
    crs = st_crs(x)
  )
}
rede <- dplyr::bind_rows(
  ler_linha("metro_linha.geojson", "metro", "existente"),
  ler_linha("trem_linha.geojson", "cptm", "existente"),
  ler_linha("corredor_municipal.geojson", "corredor", "existente"),
  ler_linha("corredor_inter.geojson", "corredor", "existente"),
  ler_linha("metro_planejada.geojson", "metro", "planejada"),
  ler_linha("trem_planejada.geojson", "cptm", "planejada"),
  ler_linha("corredor_planejado.geojson", "corredor", "planejada")
)
if (!is.null(rede) && nrow(rede)) {
  rede$cor[is.na(rede$cor) | rede$cor == ""] <- dplyr::case_when(
    rede$modal == "metro" ~ "#003DA5",
    rede$modal == "cptm" ~ "#7F0000",
    TRUE ~ "#D2641E"
  )
  rede <- st_simplify(rede, dTolerance = 1e-4, preserveTopology = TRUE)
} else {
  rede <- NULL
}

saveRDS(
  list(
    hex = g,
    dist = dist,
    contorno = contorno_sf,
    rede = rede,
    mediana_acesso = med,
    cidade_iptc = cidade_iptc,
    acesso_medio = weighted.mean(hab$pct60, hab$pop_total),
    n_hex = nrow(g),
    n_habitado = sum(met$habitado),
    n_vazio = sum(!met$habitado),
    pop_total = sum(met$pop_total),
    n_cego = sum(met$ponto_cego),
    pop_cego = sum(met$pop_total[met$ponto_cego])
  ),
  file.path(outdir, "camadas.rds")
)

message(
  "OK · ", nrow(g), " hexágonos em SP (",
  sum(met$habitado), " habitados + ", sum(!met$habitado), " vazios) · ",
  "fora removidos · mediana acesso ", round(med, 2), "% · ",
  round(file.size(file.path(outdir, "camadas.rds")) / 1e6, 2), " MB"
)
