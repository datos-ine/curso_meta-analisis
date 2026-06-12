# opciones globales
knitr::opts_chunk$set(
  fig.align = "center",
  out.width = "85%",
  dpi = 300
)


# paquetes
pacman::p_load(
  patchwork,
  scico,
  performance,
  knitr,
  kableExtra,
  webshot,
  webshot2,
  meta,
  metafor,
  janitor,
  tidyverse
)

# paleta colorblind-friendly
pal <- scico::scico(n = 9, palette = "tokyo")

# Formato tabla
kbl_format <- function(x) {
  if (knitr::is_html_output()) {
    kable(x) |>
      kable_styling(
        bootstrap_options = c("stripped", "condensed", "hover"),
        fixed_thead = TRUE,
        html_font = "Calibri"
      ) |>
      row_spec(0, background = "#2f2e4e", color = "white")
  } else {
    kable(x, format = "pipe")
  }
}
