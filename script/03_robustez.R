# =============================================================================
# TFG: Conflicto armado colombiano y desigualdad regional
# 03_robustez.R — Pruebas de robustez del modelo base (apartado 5.3)
# Universitat de València — Grau en Economia 2025-26
# =============================================================================
# ENTRADA: outputs/panel_base.rds
# Modelo base: ln_pib_pc ~ tasa_expulsion (efectos fijos de dos vías).
# Cada prueba responde a una de las cuatro amenazas del apartado 5.2.
# Errores agrupados por departamento en todas las especificaciones.
# =============================================================================

library(plm)
library(lmtest)
library(sandwich)

panel <- readRDS("outputs/panel_base.rds")
panel$trend <- panel$anio - 1993
pdata <- pdata.frame(panel, index = c("dp", "anio"))

# Errores estándar agrupados por departamento (Arellano)
cl <- function(m) coeftest(m, vcov = vcovHC(m, method = "arellano", cluster = "group"))

# --- Referencia: modelo base ------------------------------------------------
m_base <- plm(ln_pib_pc ~ tasa_expulsion, pdata,
              model = "within", effect = "twoways")

# --- (1) Tendencias temporales propias por departamento ---------------------
# Cada departamento con su propia tendencia lineal (factor(dp):trend).
# Amenazas: omitida con variacion temporal + tendencias diferenciales.
m_trend <- plm(ln_pib_pc ~ tasa_expulsion + factor(dp):trend, pdata,
               model = "within", effect = "twoways")

# --- (2) Regresor rezagado un anio ------------------------------------------
# El desplazamiento del anio anterior. Amenaza: causalidad inversa.
m_lag <- plm(ln_pib_pc ~ lag(tasa_expulsion, 1), pdata,
             model = "within", effect = "twoways")

# --- (3) Especificacion alternativa del shock -------------------------------
# Log del numero de expulsados en lugar de la tasa. Amenaza: error de medida.
m_lns <- plm(ln_pib_pc ~ ln_expulsados, pdata,
             model = "within", effect = "twoways")

# --- (4) Sensibilidad de la muestra -----------------------------------------
# (a) Sin Bogota (11) ni Choco (27), los extremos del PIB per capita.
m_out <- plm(ln_pib_pc ~ tasa_expulsion,
             subset(panel, !(dp %in% c("11", "27"))),
             index = c("dp", "anio"), model = "within", effect = "twoways")

# (b) Sin el periodo de pandemia (desde 2020), shock comun atipico.
m_cov <- plm(ln_pib_pc ~ tasa_expulsion,
             subset(panel, anio <= 2019),
             index = c("dp", "anio"), model = "within", effect = "twoways")

# --- Resultados (coeficiente de interes con SE agrupados) -------------------
cat("\n=== M base ===\n");                print(cl(m_base))
cat("\n=== (1) Tendencias dpto ===\n");   print(cl(m_trend)["tasa_expulsion", , drop = FALSE])
cat("\n=== (2) Rezago t-1 ===\n");        print(cl(m_lag))
cat("\n=== (3) ln(expulsados) ===\n");    print(cl(m_lns))
cat("\n=== (4a) Sin Bogota/Choco ===\n"); print(cl(m_out))
cat("\n=== (4b) Sin pandemia ===\n");     print(cl(m_cov))

# --- Tabla comparativa para el capitulo 6 (descomentar para exportar) -------
# library(modelsummary)
# modelsummary(list("Base"=m_base, "Tend. dpto"=m_trend, "Rezago"=m_lag,
#                   "ln(expuls.)"=m_lns, "Sin Bog/Choco"=m_out, "Sin COVID"=m_cov),
#              vcov = ~dp, stars = TRUE, output = "outputs/tabla_robustez.docx")
