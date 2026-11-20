# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 05 - Cambio climático observado
# =============================================================================
#
# Series diarias areales de la cuenca del río Huancané (PISCOp v3 y PISCO
# temperatura), anomalías, tendencias (Mann-Kendall, pendiente de Sen,
# Mann-Kendall modificado) e índices de extremos climáticos (ETCCDI).
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes -------------------------------------------------------------------

library(tidyverse)
library(terra)        # solo para leer las áreas de las subcuencas
library(trend)        # Mann-Kendall y pendiente de Sen
library(modifiedmk)   # Mann-Kendall modificado (series autocorrelacionadas)


# 2. Series diarias areales de la cuenca ------------------------------------------

# Precipitación (PISCOp v3, 1981-2025) y temperatura (PISCO, 1981-2020)
# vienen como promedio de cada subcuenca. La cuenca = promedio ponderado por área.
subcuencas <- vect("data/basin/Subbasins.shp")
areas <- tibble(subcuenca = subcuencas$Subbasin,
                area_km2  = expanse(subcuencas, unit = "km"))

serie_cuenca <- function(archivo, nombre) {
  read_csv(archivo, show_col_types = FALSE) |>
    pivot_longer(-Date, names_to = "subcuenca", values_to = "valor") |>
    mutate(subcuenca = parse_number(subcuenca)) |>
    left_join(areas, by = "subcuenca") |>
    group_by(fecha = Date) |>
    summarise({{ nombre }} := weighted.mean(valor, area_km2))
}

clima <- serie_cuenca("data/pcp/Datos_pcp.csv", pp) |>
  left_join(serie_cuenca("data/temp/Datos_tmax.csv", tmax), by = "fecha") |>
  left_join(serie_cuenca("data/temp/Datos_tmin.csv", tmin), by = "fecha")
clima
#>   fecha         pp  tmax  tmin
#>   1981-01-01  6.40  14.5  2.14
#>   1981-01-02  5.20  12.9  1.56


# 3. Índices anuales de extremos (ETCCDI) ---------------------------------------

# Índices recomendados por el ETCCDI (Expert Team on Climate Change Detection
# and Indices). Se calculan año por año con summarise().

# Racha más larga de días que cumplen una condición (TRUE seguidos).
# rle() cuenta "corridas": rle(c(T, T, F, T)) -> longitudes 2, 1, 1
racha_max <- function(condicion) {
  r <- rle(condicion)
  max(c(0, r$lengths[r$values]))
}
racha_max(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))   #> 3

indices_pp <- clima |>
  group_by(anio = year(fecha)) |>
  summarise(
    PRCPTOT = sum(pp[pp >= 1]),                 # lluvia total en días húmedos (mm)
    Rx1day  = max(pp),                          # máxima lluvia en 1 día (mm)
    Rx5day  = max(zoo::rollsum(pp, 5)),         # máxima lluvia en 5 días seguidos
    SDII    = mean(pp[pp >= 1]),                # intensidad: mm por día húmedo
    R10mm   = sum(pp >= 10),                    # días con lluvia >= 10 mm
    CDD     = racha_max(pp < 1),                # racha seca más larga (días)
    CWD     = racha_max(pp >= 1)                # racha húmeda más larga (días)
  )

indices_t <- clima |>
  filter(year(fecha) <= 2020) |>               # la temperatura llega a 2020
  group_by(anio = year(fecha)) |>
  summarise(
    TX   = mean(tmax),                          # Tmax media anual (°C)
    TN   = mean(tmin),                          # Tmin media anual (°C)
    TXx  = max(tmax),                           # Tmax más alta del año
    TNn  = min(tmin),                           # Tmin más baja del año
    FD   = sum(tmin < 0),                       # días con helada (Tmin < 0 °C)
    DTR  = mean(tmax - tmin)                    # rango térmico diario (°C)
  )

indices <- full_join(indices_pp, indices_t, by = "anio")
indices
#>   anio PRCPTOT Rx1day Rx5day SDII R10mm CDD CWD   TX    TN  TXx   TNn  FD  DTR
#>   1981     821   17.5   41.3 4.42    10  45  31 13.1 -1.48 16.4 -9.73 199 14.6
#>   1982     672   16.4   52.0 4.20    11 107  29 13.2 -1.58 16.1 -10.2 195 14.7


# 4. Anomalías respecto a la normal 1991-2020 -------------------------------------

anomalias <- indices |>
  filter(anio <= 2020) |>
  mutate(
    ref = anio %in% 1991:2020,   # años de la normal
    anom_TX = TX - mean(TX[ref]),
    anom_TN = TN - mean(TN[ref]))

g_anom <- anomalias |>
  select(anio, Tmax = anom_TX, Tmin = anom_TN) |>
  pivot_longer(-anio, names_to = "variable", values_to = "anomalia") |>
  ggplot(aes(anio, anomalia, fill = anomalia > 0)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ variable, ncol = 1) +
  scale_fill_manual(values = c("#0B5394", "#CC0000"), guide = "none") +
  labs(title = "Anomalía de temperatura en la cuenca Huancané (vs 1991-2020)",
       x = NULL, y = "°C") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"),
        strip.text = element_text(face = "bold", hjust = 0))
g_anom


# 5. Tendencias: Mann-Kendall y pendiente de Sen ----------------------------------

# Mann-Kendall: ¿la serie crece (o decrece) de forma monótona?  -> p-valor
# Pendiente de Sen: ¿cuánto cambia por año? (mediana de todas las pendientes)
tx <- indices_t$TX

mk.test(tx)
#> z = 4.3458, n = 40, p-value = 1.387e-05

sens.slope(tx)
#> Sen's slope 0.0345 °C/año  (IC 95 %: 0.023 a 0.048)

# Si la serie está autocorrelacionada (un año se parece al anterior), el
# Mann-Kendall clásico encuentra tendencias "de más". El MK modificado de
# Hamed y Rao (1998) corrige la varianza del estadístico.
acf(tx, plot = FALSE)$acf[2]  #> 0.27 (autocorrelación de orden 1)
mmkh(tx)
#> new P-value 1.39e-05 (sin cambio: la autocorrelación no es significativa)

# Una función que resume todo para cualquier índice
tendencia <- function(x) {
  x  <- x[!is.na(x)]
  mk <- mk.test(x)
  tibble(
    n           = length(x),
    sen_decada  = unname(sens.slope(x)$estimates) * 10,
    p_mk        = mk$p.value,
    p_mmk       = unname(mmkh(x)["new P-value"]),
    significativa = if_else(p_mmk < 0.05, "sí", "no")
  )
}

tendencia(tx)
#>       n sen_decada      p_mk     p_mmk significativa
#>      40      0.345 0.0000139 0.0000139 sí

# map() aplica tendencia() a cada índice (columna) de la tabla
resumen <- indices |>
  select(-anio) |>
  map(tendencia) |>
  list_rbind(names_to = "indice") |>
  mutate(across(c(sen_decada, p_mk, p_mmk), \(x) round(x, 3)))
resumen
#>   indice    n sen_decada  p_mk p_mmk significativa
#>   PRCPTOT  45     -6.62  0.66  0.66  no
#>   Rx1day   45      0.276 0.564 0.482 no
#>   Rx5day   45     -1.14  0.373 0.358 no
#>   SDII     45      0.007 0.822 0.723 no
#>   R10mm    45      0     0.738 0.738 no
#>   CDD      45      2.25  0.531 0.422 no
#>   CWD      45     -1.16  0.247 0.071 no
#>   TX       40      0.345 0     0     sí
#>   TN       40      0.136 0.004 0.004 sí
#>   TXx      40      0.479 0     0     sí
#>   TNn      40      0.157 0.23  0.062 no
#>   FD       40     -4.51  0.015 0.015 sí
#>   DTR      40      0.177 0.002 0.002 sí


# 6. Gráfico de los índices con su tendencia --------------------------------------

# Recta de Sen: pendiente de Sen y ordenada = mediana(y - pendiente * t)
recta_sen <- function(anio, y) {
  ok <- !is.na(y)
  b  <- unname(sens.slope(y[ok])$estimates)
  a  <- median(y[ok] - b * anio[ok])
  a + b * anio
}

seleccion <- c("PRCPTOT", "Rx1day", "CDD", "TX", "TN", "FD")

indices_largo <- indices |>
  pivot_longer(-anio, names_to = "indice", values_to = "valor") |>
  filter(indice %in% seleccion, !is.na(valor)) |>
  group_by(indice) |>
  mutate(sen = recta_sen(anio, valor)) |>
  ungroup() |>
  left_join(select(resumen, indice, sen_decada, significativa), by = "indice") |>
  mutate(titulo = paste0(indice, ": ", sprintf("%+.2f", sen_decada), " /década",
                         if_else(significativa == "sí", " *", "")),
         titulo = fct_reorder(titulo, match(indice, seleccion)))

g_indices <- ggplot(indices_largo, aes(anio, valor)) +
  geom_line(color = "grey50") +
  geom_point(color = "#0B5394", size = 1.5) +
  geom_line(aes(y = sen, color = significativa), linewidth = 1.2) +
  facet_wrap(~ titulo, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("no" = "grey60", "sí" = "#CC0000"),
                     labels = c("no" = "no significativa", "sí" = "significativa (p < 0.05)")) +
  labs(title = "Índices climáticos de la cuenca Huancané y pendiente de Sen",
       x = NULL, y = NULL, color = "Tendencia (MK modificado)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#073763"),
        strip.text = element_text(face = "bold", hjust = 0))
g_indices


# 7. Exportar ----------------------------------------------------------------------

# Serie diaria areal: se usa en la corrección de sesgo y en el modelo GR4J
write_csv(clima,   "data/processed/Clima_diario_cuenca.csv")
write_csv(indices, "outputs/tablas/Indices_extremos_cuenca.csv")
write_csv(resumen, "outputs/tablas/Tendencias_indices_cuenca.csv")
ggsave("outputs/figuras/CC_anomalias_T.png", g_anom,    width = 10, height = 6,   dpi = 150)
ggsave("outputs/figuras/CC_indices.png",     g_indices, width = 11, height = 6.5, dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Calcula el índice R20mm (días con lluvia >= 20 mm) y su tendencia.
#     ¿Por qué hay tan pocos días en una serie AREAL de la cuenca?

# E2. Calcula los índices de precipitación con la estación Cojata
#     (data/processed/PP_diario_estaciones.csv). Compara Rx1day de la estación
#     con el de la cuenca: ¿cuál es mayor y por qué?

# E3. Repite las tendencias de temperatura solo para la época seca (mayo a
#     agosto). ¿Las heladas disminuyen más rápido en esa época?

# E4. ¿Cambia la conclusión de la precipitación anual si usas 1981-2020 en
#     lugar de 1981-2025? Pista: filter(anio <= 2020) antes de tendencia().
