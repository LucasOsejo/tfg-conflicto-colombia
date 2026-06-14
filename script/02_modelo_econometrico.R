# =============================================================================
# TFG: Conflicto armado colombiano y desigualdad regional
# 02_modelo_econometrico.R — Modelos de panel (capítulos 5 y 6)
# Universitat de València — Grau en Economia 2025-26
# =============================================================================
# ENTRADA: outputs/panel_base.rds  (panel 33 dptos x 1993-2022, generado por 01)
# Estimador: efectos fijos de dos vías (within), errores agrupados por dpto.
# =============================================================================

#install.packages(c("plm", "lmtest", "sandwich"))

library(plm)
library(lmtest)
library(sandwich)

panel <- readRDS("outputs/panel_base.rds")
pdata <- pdata.frame(panel, index = c("dp", "anio"))

# --- Modelo 1: especificación base (5.1) -------------------------------------
m1  <- plm(ln_pib_pc ~ tasa_expulsion,
           data = pdata, model = "within", effect = "twoways")
se1 <- coeftest(m1, vcov = vcovHC(m1, method = "arellano", cluster = "group"))

# --- Modelo 2: heterogeneidad por intensidad del conflicto (5.4) -------------
# alta_intensidad es invariante en el tiempo, así que su efecto principal lo
# absorben los efectos fijos de departamento; solo identificamos la interacción.
m2  <- plm(ln_pib_pc ~ tasa_expulsion + tasa_expulsion:alta_intensidad,
           data = pdata, model = "within", effect = "twoways")
se2 <- coeftest(m2, vcov = vcovHC(m2, method = "arellano", cluster = "group"))

# --- Modelo 3: canal de capital humano (5.6) ---------------------------------
# Especificación separada: la tasa adolescente no entra junto a la total
# (son casi colineales, una es subconjunto de la otra).
m3  <- plm(ln_pib_pc ~ tasa_expul_adol,
           data = pdata, model = "within", effect = "twoways")
se3 <- coeftest(m3, vcov = vcovHC(m3, method = "arellano", cluster = "group"))

cat("\n--- M1 base ---\n");          print(se1)
cat("\n--- M2 heterogeneidad ---\n"); print(se2)
cat("\n--- M3 capital humano ---\n"); print(se3)

# --- Validación (5.5) --------------------------------------------------------
# (a) F de significancia conjunta de los efectos fijos frente al pooled OLS.
pool <- plm(ln_pib_pc ~ tasa_expulsion, data = pdata, model = "pooling")
print(pFtest(m1, pool))

# (b) Hausman: efectos fijos vs. aleatorios (sobre efectos individuales).
fe_i <- plm(ln_pib_pc ~ tasa_expulsion, data = pdata, model = "within",  effect = "individual")
re_i <- plm(ln_pib_pc ~ tasa_expulsion, data = pdata, model = "random", effect = "individual")
print(phtest(fe_i, re_i))

# --- Tabla para el capítulo 6 (descomentar cuando la quieras exportar) -------
# library(modelsummary)
# modelsummary(list("Base" = m1, "Heterogeneidad" = m2, "Capital humano" = m3),
#              vcov = ~dp, stars = TRUE, output = "outputs/tabla_modelos.docx")
