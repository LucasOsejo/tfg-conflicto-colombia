# TFG — Conflicto armado colombiano y desarrollo económico regional (1993-2022)

Código y datos de reproducción del Trabajo de Fin de Grado.
Universitat de València, Grado en Economía, curso 2025-26.

## Requisitos
R (>= 4.5.1) y los paquetes: tidyverse, plm, sf, ggplot2.

## Ejecución
Ejecutar los scripts (en la carpeta script/) en orden:
1. 01_pipeline_panel.R      — construcción y depuración del panel
2. 02_modelo_econometrico.R — modelos de efectos fijos de dos vías (plm)
3. 03_robustez.R            — pruebas de robustez
4. grafico_3_1.R / grafico_3_2_mapa.R — figuras

El panel construido se guarda en outputs/panel_base.rds.

## Fuentes de datos
DANE (PIB departamental y proyecciones de población),
RUV-UARIV (registro de desplazamiento forzado),
DIVIPOLA (identificador territorial).

Usuarios de Windows: extraer el proyecto antes de abrir el .Rproj.

## Disponibilidad de los datos
El panel ya construido está en outputs/panel_base.rds, suficiente para
reproducir todas las estimaciones. Los datos brutos no se alojan aquí por
tamaño y proceden de fuentes públicas:
- PIB departamental y proyecciones de población: DANE — https://www.dane.gov.co/
- Registro de desplazamiento forzado: RUV-UARIV / RNI — https://www.unidadvictimas.gov.co/
- Límites departamentales: GADM — https://gadm.org/
