# =============================================================================
# TFG: Conflicto armado colombiano y desigualdad regional
# Capítulo 3 — Gráfico 3.1
# Serie temporal del desplazamiento forzado en Colombia, 1993-2022
# Universitat de València — Grau en Economia 2025-26
# =============================================================================
# ENTRADA:  outputs/panel_base.rds  (panel generado por 01_pipeline_panel.R)
# SALIDAS:  outputs/grafico_3_1.pdf
#           outputs/grafico_3_1.png
# =============================================================================

library(tidyverse)
library(scales)

# ── 1. Cargar el panel ───────────────────────────────────────────────────────
panel <- readRDS("outputs/panel_base.rds")

# ── 2. Agregar expulsados a nivel nacional por año ───────────────────────────
# El panel está a nivel departamento-año-ciclo_vital. Para la serie nacional
# total, sumamos todos los registros por año (todos los ciclos vitales y
# todos los departamentos).
serie_nacional <- panel |>
  group_by(anio) |>
  summarise(
    expulsados_total = sum(expulsados_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(anio >= 1993, anio <= 2022)

# ── 3. Definir hitos históricos ──────────────────────────────────────────────
hitos <- tibble(
  anio   = c(1999, 2005, 2016),
  evento = c("Plan Colombia", "Ley de Justicia y Paz", "Acuerdo Final")
)

# ── 4. Construir el gráfico ──────────────────────────────────────────────────
graf_3_1 <- ggplot(serie_nacional, aes(x = anio, y = expulsados_total)) +
  # Líneas verticales de los hitos
  geom_vline(
    data       = hitos,
    aes(xintercept = anio),
    linetype   = "dashed",
    color      = "grey55",
    linewidth  = 0.4
  ) +
  # Etiquetas de los hitos (rotadas, ancladas en la parte superior)
  geom_text(
    data  = hitos,
    aes(x = anio, y = Inf, label = evento),
    angle = 90,
    hjust = 1.05,
    vjust = -0.4,
    size  = 3,
    color = "grey25"
  ) +
  # Serie principal
  geom_line(color = "#1f4e79", linewidth = 0.9) +
  geom_point(color = "#1f4e79", size = 1.8, alpha = 0.85) +
  # Escalas
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", scale = 1),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_x_continuous(
    breaks = seq(1993, 2022, by = 3),
    expand = expansion(add = c(0.6, 0.6))
  ) +
  # Etiquetas (sin título: el pie del gráfico va aparte en el documento)
  labs(
    x       = "Año",
    y       = "Personas expulsadas",
    caption = "Fuente: Elaboración propia a partir del RUV-UARIV."
  ) +
  # Tema sobrio para TFG
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(
      hjust  = 0,
      color  = "grey40",
      size   = 9,
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(margin = margin(t = 10), size = 10),
    axis.title.y = element_text(margin = margin(r = 10), size = 10),
    plot.margin  = margin(15, 15, 10, 15)
  )

# ── 5. Visualizar en pantalla ────────────────────────────────────────────────
print(graf_3_1)

# ── 6. Exportar ──────────────────────────────────────────────────────────────
ggsave(
  filename = "outputs/grafico_3_1.pdf",
  plot     = graf_3_1,
  width    = 7.5,
  height   = 4.5,
  device   = cairo_pdf
)

ggsave(
  filename = "outputs/grafico_3_1.png",
  plot     = graf_3_1,
  width    = 7.5,
  height   = 4.5,
  dpi      = 300
)

message("Gráfico 3.1 generado en outputs/grafico_3_1.{pdf,png}")
