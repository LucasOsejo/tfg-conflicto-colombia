# =============================================================================
# TFG: Conflicto armado colombiano y desigualdad regional
# grafico_3_2_mapa.R — Mapa coroplético del desplazamiento acumulado por dpto.
# Apartado 3.2 — El desplazamiento forzado: magnitud y distribución departamental
# =============================================================================
# Entrada: outputs/panel_base.rds
# Salidas: outputs/grafico_3_2_mapa.png  y  outputs/grafico_3_2_mapa.pdf
#
# Requiere conexión a internet la primera vez (descarga los contornos de GADM).
# =============================================================================

# Si es la primera vez, instala:
# install.packages(c("geodata", "sf", "ggplot2", "dplyr", "tibble", "scales"))

library(geodata)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)
library(scales)

# --- 1. Datos: expulsión acumulada por departamento --------------------------
panel <- readRDS("outputs/panel_base.rds")

acum <- panel |>
  group_by(dp, depto_nom) |>
  summarise(expul_acum = sum(expulsados_total, na.rm = TRUE), .groups = "drop")

# --- 2. Cartografía: contornos de departamentos desde GADM -------------------
# La primera ejecución descarga el archivo y lo cachea en gadm_cache/.
dir.create("gadm_cache", showWarnings = FALSE)
col   <- geodata::gadm(country = "COL", level = 1, path = "gadm_cache")
col_sf <- sf::st_as_sf(col)

# --- 3. Puente DIVIPOLA <-> nombre en GADM (NAME_1) --------------------------
# Algunos nombres difieren entre el DANE y GADM (San Andrés, Bogotá…),
# por eso emparejamos manualmente para evitar mismatches silenciosos.
puente <- tribble(
  ~dp,   ~NAME_1,
  "05",  "Antioquia",
  "08",  "Atlántico",
  "11",  "Bogotá D.C.",
  "13",  "Bolívar",
  "15",  "Boyacá",
  "17",  "Caldas",
  "18",  "Caquetá",
  "19",  "Cauca",
  "20",  "Cesar",
  "23",  "Córdoba",
  "25",  "Cundinamarca",
  "27",  "Chocó",
  "41",  "Huila",
  "44",  "La Guajira",
  "47",  "Magdalena",
  "50",  "Meta",
  "52",  "Nariño",
  "54",  "Norte de Santander",
  "63",  "Quindío",
  "66",  "Risaralda",
  "68",  "Santander",
  "70",  "Sucre",
  "73",  "Tolima",
  "76",  "Valle del Cauca",
  "81",  "Arauca",
  "85",  "Casanare",
  "86",  "Putumayo",
  "88",  "San Andrés y Providencia",
  "91",  "Amazonas",
  "94",  "Guainía",
  "95",  "Guaviare",
  "97",  "Vaupés",
  "99",  "Vichada"
)

# Comprobación: ¿qué nombres del puente NO aparecen en GADM?
faltan <- setdiff(puente$NAME_1, col_sf$NAME_1)
if (length(faltan) > 0) {
  message("\n*** Hay nombres del puente que no casan con GADM: ",
          paste(faltan, collapse = ", "), " ***")
  message("Estos son los NAME_1 de GADM (cópialos y pásamelos para ajustar el puente):")
  print(sort(col_sf$NAME_1))
} else {
  message("OK: los 33 departamentos casan correctamente con GADM.")
}

# --- 4. Unión y plot ---------------------------------------------------------
mapa <- col_sf |>
  left_join(puente, by = "NAME_1") |>
  left_join(acum,   by = "dp")

g <- ggplot(mapa) +
  geom_sf(aes(fill = expul_acum), color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(
    option   = "magma",
    direction = -1,
    na.value = "grey90",
    labels   = scales::label_number(big.mark = ".", decimal.mark = ","),
    name     = "Personas expulsadas\n(acumulado 1993-2022)"
  ) +
  labs(caption = "Fuente: elaboración propia a partir del RUV-UARIV y GADM.") +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    plot.caption    = element_text(size = 8, hjust = 0, color = "grey30"),
    plot.margin     = margin(10, 10, 10, 10)
  )

print(g)

# --- 5. Guardar --------------------------------------------------------------
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/grafico_3_2_mapa.png", g, width = 7.5, height = 9, dpi = 300, bg = "white")
ggsave("outputs/grafico_3_2_mapa.pdf", g, width = 7.5, height = 9, bg = "white")
cat("\n✓ Mapa guardado en outputs/grafico_3_2_mapa.png y .pdf\n")
