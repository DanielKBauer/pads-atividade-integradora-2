# Dashboard SPTrans — IPTC × acessibilidade a empregos

App [shinydashboard](https://rstudio.github.io/shinydashboard/) para o decisor
da SPTrans cruzar, no mapa de São Paulo:

- **nota do IPTC (2025)** — permeabilidade oficial
- **% do mercado alcançável em ≤60 min** — resultado do commute (AOP/Ipea, 2019)

Inspiração de interação: hover sobre o território com leitura imediata da
métrica (no espírito de visualizações tipo
[VisQuill / NYC 311](https://visquill.com/gallery/nyc-311/)).

## Como rodar

Na raiz do projeto (clone do GitHub):

```r
install.packages(
  c("shiny", "shinydashboard", "leaflet", "sf", "dplyr",
    "htmltools", "scales", "data.table"),
  repos = "https://cloud.r-project.org"
)

# sobe o app (usa dashboard/data/camadas.rds já commitado)
shiny::runApp("dashboard")
```

Ou no terminal:

```bash
Rscript -e 'shiny::runApp("dashboard", launch.browser = TRUE)'
```

No macOS, o aviso de `R_X11.so` / XQuartz pode ser ignorado — o app roda no browser.

Para regenerar as camadas após mudar dados de saída/EDA:

```bash
Rscript dashboard/prep_dados.R
```

Pacotes: `shiny`, `shinydashboard`, `leaflet`, `sf`, `dplyr`, `htmltools`, `scales`, `data.table`.

## O que o dashboard faz

| Elemento | Função |
|:--|:--|
| **3 botões no topo** | IPTC (72,16) · Acesso 60 min (19,5%) · GAP — alternam a camada do mapa |
| Escala verde → vermelho | Melhor → pior (no GAP, maior = pior) |
| Mapa de hexágonos | Hover com IPTC, acesso e GAP |
| **Rede modal** | Metrô, CPTM e corredores de ônibus (existentes / planejados 2025) sobre o mapa |
| Tabela à direita | 96 distritos com IPTC, acesso e GAP — **multi-seleção** para comparar |

## Dados

- `dashboard/data/camadas.rds` — gerado por `prep_dados.R`
- Grade **completa do município** (habitados + vazios), recortada ao contorno
  dos 96 distritos; hexágonos fora de São Paulo são removidos
- Rede TP: `dados/externo/rede_tp/` (GeoSampa / [sp-mapas](https://github.com/nucleo-digital/sp-mapas))
- Fonte métricas: `grade_hexagonos_sp.gpkg` + acessibilidade 2019 + IPTC 2025
  + `distritos-sp.geojson`
