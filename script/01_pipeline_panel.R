# =============================================================================
# TFG: Conflicto armado colombiano y desigualdad regional
# Script 01: Construcción del panel departamental
# Universitat de València — Grau en Economia 2025-26
# =============================================================================
# FUENTES:
#   - Población DANE: bloques 1993-2004, 2005-2017, 2018-2050
#   - PIB departamental DANE: Cuadro 1 (series encadenadas base 2015)
#   - Víctimas RUV: registro de desplazamiento forzado 1985-2025
# SALIDA:
#   - panel_base.csv   → panel balanceado 33 depto × años disponibles
#   - panel_base.rds   → mismo objeto en formato nativo R (más rápido)
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)   # clean_names()
library(here)      # rutas relativas (opcional, ajusta si no lo usas)

# ── Rutas de entrada ─────────────────────────────────────────────────────────
ruta_pob_9304  <- "datos/DCD-areaproypoblacion-dep-1993-2004.xlsx"
ruta_pob_0517 <- "datos/DCD-area-proypoblacion-dep-2005-2017_VP (1).xlsx"
ruta_pob_1850  <- "datos/PPED-AreaDep-2018-2050_VP.xlsx"
ruta_pib       <- "datos/anex-PIBDep-RetropoDepart-2024pr.xlsx"
ruta_ruv       <- "datos/Víctimas_desplazamiento_anualizado_ocurrencia_y_llegada_Corte_DICIEMBRE_DE_2025.xls"

# =============================================================================
# 1. POBLACIÓN DEPARTAMENTAL
# =============================================================================

# Función genérica para leer cada bloque DANE de población
leer_pob <- function(ruta, hoja, skip, col_pob = "Población") {
  read_excel(ruta, sheet = hoja, skip = skip) |>
    select(1:5) |>
    set_names(c("dp_raw", "depto_nom", "anio", "area", "pob_total")) |>
    filter(str_to_lower(str_trim(area)) == "total") |>
    select(dp_raw, depto_nom, anio, pob_total) |>
    mutate(
      # Estandarizar código DIVIPOLA: siempre 2 dígitos con cero inicial
      dp = str_pad(as.character(as.integer(dp_raw)), width = 2, pad = "0"),
      anio = as.integer(anio),
      pob_total = as.numeric(pob_total)
    ) |>
    select(dp, depto_nom, anio, pob_total) |>
    filter(!is.na(dp), !is.na(anio), !is.na(pob_total))
}

pob_9304 <- leer_pob(ruta_pob_9304, "Departamental_1993_2004",   skip = 11)
pob_0517 <- leer_pob(ruta_pob_0517, "Departamental_2005-2019",   skip = 11)
pob_1822 <- leer_pob(ruta_pob_1850, "PobDepartamentalxÁrea",     skip = 7,
                     col_pob = "TOTAL") |>
  filter(anio <= 2022)   # Recortamos en 2022 (último año con PIB disponible)

# Unir los tres bloques — el período resultante es 1993-2022
pob <- bind_rows(pob_9304, pob_0517, pob_1822) |>
  distinct(dp, anio, .keep_all = TRUE) |>   # eliminar duplicados en solapamientos
  arrange(dp, anio)

message("Población: ", nrow(pob), " filas | ",
        min(pob$anio), "-", max(pob$anio), " | ",
        n_distinct(pob$dp), " departamentos")

# =============================================================================
# 2. PIB DEPARTAMENTAL (series encadenadas volumen, base 2015)
# =============================================================================
# Fuente: DANE — Cuadro 2 del archivo anex-PIBDep-RetropoDepart-2024pr.xlsx
# Estructura: fila 10 = encabezados (código, nombre, 1980, 1981, ..., 2024pr)
#             fila 11 = COLOMBIA total (sin código DIVIPOLA — se excluye)
#             filas 12+ = departamentos con código DIVIPOLA en col 1
# El cuadro tiene datos duplicados a la derecha (tasas de crecimiento),
# nos quedamos solo con las primeras 47 columnas (código + nombre + 45 años)

pib_ancho <- read_excel(ruta_pib,
                        sheet    = "Cuadro 2",
                        skip     = 9,    # saltar filas 1-9
                        col_names = TRUE) |>
  select(1:47)   # código, nombre, 1980:2024pr

# Renombrar las dos primeras columnas
names(pib_ancho)[1:2] <- c("dp_raw", "depto_nom")

pib <- pib_ancho |>
  # Excluir fila COLOMBIA (dp_raw == "COLOMBIA" o NA) y filas vacías
  filter(!is.na(dp_raw),
         !str_to_upper(str_trim(as.character(dp_raw))) %in% c("COLOMBIA", "")) |>
  pivot_longer(
    cols      = -c(dp_raw, depto_nom),
    names_to  = "anio",
    values_to = "pib_miles_millones"
  ) |>
  mutate(
    dp                 = str_pad(as.character(as.integer(dp_raw)), width = 2, pad = "0"),
    anio               = as.integer(str_extract(as.character(anio), "\\d{4}")),
    pib_miles_millones = as.numeric(pib_miles_millones)
  ) |>
  filter(!is.na(pib_miles_millones), !is.na(anio),
         anio >= 1993, anio <= 2022) |>
  select(dp, anio, pib_miles_millones)

message("PIB: ", nrow(pib), " filas | ",
        min(pib$anio), "-", max(pib$anio), " | ",
        n_distinct(pib$dp), " departamentos")

# =============================================================================
# 3. PIB PER CÁPITA
# =============================================================================

pib_pc <- pib |>
  left_join(pob, by = c("dp", "anio")) |>
  mutate(
    pib_pc        = (pib_miles_millones * 1e9) / pob_total,  # pesos constantes 2015
    ln_pib_pc     = log(pib_pc)
  ) |>
  filter(!is.na(pib_pc), pib_pc > 0)

message("PIB per cápita: ", nrow(pib_pc), " filas sin nulos")

# =============================================================================
# 4. RUV — REGISTRO ÚNICO DE VÍCTIMAS
# =============================================================================
# El archivo es HTML disfrazado de .xls → leer con xml2/rvest
# Columnas confirmadas: FECHA_CORTE, NOM_RPT, COD_PAIS, PAIS,
#   COD_ESTADO_DEPTO, ESTADO_DEPTO, VIGENCIA, PARAM_HECHO, HECHO,
#   SEXO, ETNIA, DISCAPACIDAD, CICLO_VITAL, PER_OCU, PER_LLEGADA, EVENTOS
# CICLO_VITAL valores: 'entre 0 y 5', 'entre 6 y 11', 'entre 12 y 17',
#                      'entre 18 y 28', 'entre 29 y 59', 'entre 60 y 110', 'ND'

ruv_raw <- xml2::read_html(ruta_ruv) |>
  rvest::html_table(fill = TRUE) |>
  purrr::pluck(1) |>
  as_tibble(.name_repair = "unique")

ruv <- ruv_raw |>
  rename(
    dp_raw     = COD_ESTADO_DEPTO,
    depto_nom  = ESTADO_DEPTO,
    anio       = VIGENCIA,
    ciclo      = CICLO_VITAL,
    expulsados = PER_OCU,
    recibidos  = PER_LLEGADA
  ) |>
  mutate(
    dp         = str_pad(as.character(as.integer(dp_raw)), width = 2, pad = "0"),
    anio       = as.integer(anio),
    expulsados = as.numeric(expulsados),
    recibidos  = as.numeric(recibidos),
    EVENTOS    = as.numeric(EVENTOS)
  ) |>
  filter(
    !is.na(dp),
    dp != "00",                          # excluir SIN DEFINIR (código 0)
    anio >= 1993, anio <= 2022,
    ciclo != "ND"                        # excluir no definidos (0.2%)
  )

# ── 4a. Agregado total por departamento-año ───────────────────────────────────
ruv_total <- ruv |>
  group_by(dp, anio) |>
  summarise(
    expulsados_total = sum(expulsados, na.rm = TRUE),
    recibidos_total  = sum(recibidos,  na.rm = TRUE),
    eventos_total    = sum(EVENTOS ,    na.rm = TRUE),
    .groups = "drop"
  )

# ── 4b. Desplazados adolescentes (12-17 años) ────────────────────────────────
ruv_adol <- ruv |>
  filter(ciclo == "entre 12 y 17") |>
  group_by(dp, anio) |>
  summarise(
    expulsados_adol = sum(expulsados, na.rm = TRUE),
    .groups = "drop"
  )

ruv_dep <- ruv_total |>
  left_join(ruv_adol, by = c("dp", "anio")) |>
  mutate(expulsados_adol = replace_na(expulsados_adol, 0))

message("RUV agregado: ", nrow(ruv_dep), " filas | ",
        n_distinct(ruv_dep$dp), " departamentos")

# =============================================================================
# 5. CONSTRUCCIÓN DEL PANEL
# =============================================================================

# Grid completo: todos los departamentos × todos los años del período
deptos <- pob |>
  mutate(depto_nom = case_when(
    dp == "63" ~ "Quindío",
    dp == "88" ~ "Archipiélago de San Andrés, Providencia y Santa Catalina",
    TRUE ~ depto_nom
  )) |>
  distinct(dp, depto_nom)
anios  <- tibble(anio = 1993:2022)

panel_grid <- cross_join(deptos, anios)

# Merge secuencial
panel <- panel_grid |>
  left_join(pib_pc |> select(dp, anio, pib_miles_millones, pob_total,
                             pib_pc, ln_pib_pc),
            by = c("dp", "anio")) |>
  left_join(ruv_dep, by = c("dp", "anio")) |>
  mutate(
    # Tasas de desplazamiento por cada 1000 habitantes
    tasa_expulsion = (expulsados_total / pob_total) * 1000,
    tasa_expul_adol = (expulsados_adol / pob_total) * 1000,
    
    # Log de desplazamiento (más 1 para evitar log(0))
    ln_expulsados  = log(expulsados_total + 1),
    ln_expul_adol  = log(expulsados_adol  + 1),
    
    # Variable de intensidad acumulada de conflicto (se calcula después)
    # Se construirá en el script 02 de análisis
  ) |>
  arrange(dp, anio)

# ── 5a. Clasificar departamentos por intensidad histórica de conflicto ────────
# Alta intensidad = departamentos en el cuartil superior de expulsión acumulada
intensidad <- panel |>
  group_by(dp) |>
  summarise(expul_acum = sum(expulsados_total, na.rm = TRUE)) |>
  mutate(
    cuartil_conflicto = ntile(expul_acum, 4),
    alta_intensidad   = as.integer(cuartil_conflicto == 4)
  ) |>
  select(dp, expul_acum, cuartil_conflicto, alta_intensidad)

panel <- panel |>
  left_join(intensidad, by = "dp")

# =============================================================================
# 6. DIAGNÓSTICO DEL PANEL
# =============================================================================

cat("\n", strrep("=", 60), "\n")
cat("DIAGNÓSTICO DEL PANEL FINAL\n")
cat(strrep("=", 60), "\n\n")

cat("Dimensiones:", nrow(panel), "filas ×", ncol(panel), "columnas\n")
cat("Departamentos:", n_distinct(panel$dp), "\n")
cat("Período:", min(panel$anio), "-", max(panel$anio), "\n\n")

cat("Cobertura de variables clave:\n")
panel |>
  summarise(
    pib_pc_ok      = mean(!is.na(ln_pib_pc))    * 100,
    pob_ok         = mean(!is.na(pob_total))     * 100,
    expulsados_ok  = mean(!is.na(tasa_expulsion))* 100
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_completo") |>
  mutate(pct_completo = round(pct_completo, 1)) |>
  print()

cat("\nDepartamentos de alta intensidad de conflicto:\n")
panel |>
  filter(alta_intensidad == 1) |>
  distinct(dp, depto_nom, expul_acum) |>
  arrange(desc(expul_acum)) |>
  print(n = 15)

cat("\nEstadísticas descriptivas:\n")
panel |>
  select(ln_pib_pc, tasa_expulsion, tasa_expul_adol, pob_total) |>
  summary() |>
  print()

# =============================================================================
# 7. GUARDAR
# =============================================================================

write_csv(panel, "outputs/panel_base.csv")
saveRDS(panel,   "outputs/panel_base.rds")

cat("\n✓ Panel guardado en outputs/panel_base.csv y outputs/panel_base.rds\n")
cat("  Siguiente paso: script 02_modelo_econometrico.R\n")

nrow(panel)                                  # el objeto que crea el script → debe dar 990
panel_base <- readRDS("outputs/panel_base.rds")  # recarga la versión nueva del disco
nrow(panel_base)                             # ahora sí 990
nrow(dplyr::distinct(panel_base, dp, depto_nom)) # 33, ya no 35
