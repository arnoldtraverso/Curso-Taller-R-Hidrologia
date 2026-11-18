# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 02 - Procesamiento y visualización
# =============================================================================
#
# Importar datos SENAMHI, limpiar con dplyr/tidyr, control de calidad,
# agregación diaria -> mensual -> anual y gráficos con ggplot2.
# Datos: caudal Puente Carretera Huancané (210201) y precipitación Huancané (000786).
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes -------------------------------------------------------------------

library(tidyverse)   # dplyr, tidyr, ggplot2, lubridate, readr
library(readxl)


# 2. Importar y ordenar el caudal ---------------------------------------------

q_raw <- read_excel("data/raw/caudal/Q_210201_PteCarretera_Huancane.xlsx",
                    skip = 13)
names(q_raw)                 #> "FECHA" "HORA" "VALOR (m³/s)"

q <- q_raw |>
  rename(fecha = FECHA, hora = HORA, q = 3) |>   # nombres cortos y sin espacios
  mutate(fecha = dmy(fecha)) |>                  # texto -> Date
  arrange(fecha) |>                              # de 1956 a 2026
  select(fecha, q)

q
nrow(q)                      #> 25208
range(q$fecha)               #> "1956-01-01" "2026-05-04"


# 3. Los verbos de dplyr ----------------------------------------------------------

# filter(): filas que cumplen una condición
q |> filter(q > 200)                         # días con más de 200 m³/s
q |> filter(q > 200) |> nrow()               #> 72
q |> filter(year(fecha) == 2023)             # un año

# mutate(): crear o modificar columnas
q |> mutate(vol_hm3 = q * 86400 / 1e6)       # volumen diario en hm³

# arrange(): ordenar
q |> arrange(desc(q))                        # el máximo: 368.59 el 2016-02-27

# summarise(): resumir
q |> summarise(q_medio = mean(q), q_max = max(q), n = n())
#>   q_medio q_max     n
#>      18.3  369. 25208

# group_by() + summarise(): resumir por grupos
q |>
  group_by(anio = year(fecha)) |>
  summarise(q_medio = mean(q))


# 4. Completar las fechas faltantes ----------------------------------------------

# Los días sin dato NO tienen fila: hay que crearlos para verlos como NA
q_full <- q |>
  complete(fecha = seq(min(fecha), max(fecha), by = "day"))

nrow(q_full)                 #> 25692
sum(is.na(q_full$q))         #> 484  días sin dato

# ¿Qué años tienen más vacíos?
faltantes_anio <- q_full |>
  group_by(anio = year(fecha)) |>
  summarise(n_na = sum(is.na(q))) |>
  arrange(desc(n_na))
faltantes_anio
#>   anio  n_na
#>   1975   244
#>   2020   119
#>   1985    32
#>   1993    30


# 5. Precipitación: registros duplicados -----------------------------------------

pp_raw <- read_excel("data/raw/precipitacion/PP_000786_Huancane.xlsx", skip = 13)

pp <- pp_raw |>
  rename(fecha = FECHA, hora = HORA, pp = 3) |>
  mutate(fecha = dmy(fecha))

table(pp$hora)
#> 00:00 01:00 05:00
#> 19030     1  1260

# ¿Cuántas fechas aparecen más de una vez?
duplicados <- pp |> count(fecha) |> filter(n > 1)
nrow(duplicados)             #> 1260
range(duplicados$fecha)      #> "2017-01-01" "2024-04-10"

# Fechas donde los dos registros NO coinciden
pp |>
  group_by(fecha) |>
  filter(n() > 1, n_distinct(pp) > 1) |>
  arrange(fecha, hora)
#>   fecha      hora     pp
#>   2017-02-28 00:00  21.9
#>   2017-02-28 05:00   2.9  ...

# Si no se tratan, la lluvia anual sale casi el doble
pp |> filter(year(fecha) == 2019) |> summarise(total = sum(pp))   #> 1334.5

# Regla del curso: por fecha usar 00:00; si no existe, 05:00
# ("00:00" < "05:00" al ordenar, y distinct() conserva la primera fila)
pp <- pp |>
  arrange(fecha, hora) |>
  distinct(fecha, .keep_all = TRUE) |>
  select(fecha, pp)

pp |> filter(year(fecha) == 2019) |> summarise(total = sum(pp))   #> 685.8

# Completar fechas igual que con el caudal
pp_full <- pp |>
  complete(fecha = seq(min(fecha), max(fecha), by = "day"))
nrow(pp_full)                #> 22939
sum(is.na(pp_full$pp))       #> 3908


# 6. Control de calidad básico ----------------------------------------------------

# Valores imposibles o sospechosos
q_full  |> summarise(negativos = sum(q < 0, na.rm = TRUE),
                     ceros     = sum(q == 0, na.rm = TRUE))
#>   negativos ceros
#>           0     6      (2012 y 2014: revisar)

pp_full |> summarise(negativos = sum(pp < 0, na.rm = TRUE),
                     maximo    = max(pp, na.rm = TRUE))
#>   negativos maximo
#>           0   68.7     (1994-03-15)

# Mapa de vacíos: días sin dato por año y mes (precipitación)
vacios_pp <- pp_full |>
  group_by(anio = year(fecha), mes = month(fecha)) |>
  summarise(n_na = sum(is.na(pp)), .groups = "drop")


# 7. Agregación diaria -> mensual -> anual -----------------------------------------

# Regla de completitud: un mes es válido si tiene como máximo 3 días sin dato
max_na <- 3

q_mensual <- q_full |>
  group_by(anio = year(fecha), mes = month(fecha)) |>
  summarise(n_na = sum(is.na(q)),
            q    = mean(q, na.rm = TRUE),      # caudal: PROMEDIO
            .groups = "drop") |>
  mutate(q = if_else(n_na > max_na, NA, q))

pp_mensual <- pp_full |>
  group_by(anio = year(fecha), mes = month(fecha)) |>
  summarise(n_na = sum(is.na(pp)),
            pp   = sum(pp, na.rm = TRUE),      # precipitación: SUMA
            .groups = "drop") |>
  mutate(pp = if_else(n_na > max_na, NA, pp))

pp_mensual |> filter(anio == 2019)

# Anual: válido solo si sus 12 meses son válidos
# (n_meses va ANTES que q: summarise calcula en orden)
q_anual <- q_mensual |>
  group_by(anio) |>
  summarise(n_meses = sum(!is.na(q)),
            q       = mean(q)) |>
  filter(n_meses == 12)

q_anual |> arrange(q) |> head(3)
#>   anio n_meses     q
#>   2023      12  4.75     <- el año más seco del registro
#>   1983      12  7.23
#>   1967      12  7.67

# Climatología mensual 1991-2020 (régimen)
q_clim <- q_mensual |>
  filter(anio >= 1991, anio <= 2020) |>
  group_by(mes) |>
  summarise(q = mean(q, na.rm = TRUE))
q_clim
#> ene 39.3  feb 57.4  mar 48.9 ... set 2.57 (mínimo)


# 8. Gráficos con ggplot2 --------------------------------------------------------

# ggplot(datos, aes(x, y)) + geom_...() + capas extra
meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

tema_curso <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"))

# 8.1 Hidrograma diario
g_hidro <- ggplot(q_full, aes(x = fecha, y = q)) +
  geom_line(color = "#0B5394", linewidth = 0.25) +
  labs(title = "Caudal diario - Puente Carretera Huancané",
       x = NULL, y = "Caudal (m³/s)") +
  tema_curso
g_hidro

# 8.2 Régimen mensual
g_clim <- ggplot(q_clim, aes(x = factor(mes), y = q)) +
  geom_col(fill = "#0B5394", width = 0.7) +
  scale_x_discrete(labels = meses_es) +
  labs(title = "Caudal medio mensual (1991-2020)",
       x = NULL, y = "Caudal (m³/s)") +
  tema_curso
g_clim

# 8.3 Caudal medio anual, resaltando 2023
q_medio <- mean(q_anual$q)       #> 17.9  (62 años completos)

g_anual <- ggplot(q_anual, aes(x = anio, y = q)) +
  geom_col(aes(fill = anio == 2023), width = 0.8, show.legend = FALSE) +
  geom_hline(yintercept = q_medio, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2023, y = q_anual$q[q_anual$anio == 2023], label = "2023", vjust = -0.6,
           color = "#FF6732", fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "#0B5394", "TRUE" = "#FF6732")) +
  labs(title = "Caudal medio anual (años completos)",
       x = NULL, y = "Caudal (m³/s)") +
  tema_curso
g_anual

# 8.4 Mapa de vacíos de la precipitación
g_vacios <- ggplot(vacios_pp, aes(x = anio, y = factor(mes), fill = n_na)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_y_discrete(labels = meses_es, limits = rev) +
  scale_fill_gradient(low = "#EEF4FB", high = "#073763",
                      name = "Días sin\ndato") +
  labs(title = "Precipitación Huancané: días sin dato por mes",
       x = NULL, y = NULL) +
  tema_curso
g_vacios


# 9. Exportar resultados ---------------------------------------------------------

write_csv(q_full,     "data/processed/Q_diario_Huancane.csv")
write_csv(pp_full,    "data/processed/PP_diario_Huancane.csv")
write_csv(q_mensual,  "data/processed/Q_mensual_Huancane.csv")
write_csv(pp_mensual, "data/processed/PP_mensual_Huancane.csv")
write_csv(q_anual,    "outputs/tablas/Q_anual_Huancane.csv")

ggsave("outputs/figuras/Q_hidrograma.png", g_hidro,  width = 10, height = 4.5, dpi = 150)
ggsave("outputs/figuras/Q_regimen.png",    g_clim,   width = 8,  height = 4.5, dpi = 150)
ggsave("outputs/figuras/Q_anual.png",      g_anual,  width = 10, height = 4.5, dpi = 150)
ggsave("outputs/figuras/PP_vacios.png",    g_vacios, width = 10, height = 4.5, dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. ¿Cuántos días del registro de caudal superan los 100 m³/s?
#     ¿En qué año ocurrieron más de esos días?

# E2. Calcula el caudal máximo de cada año (q_max) con group_by() + summarise().
#     ¿Cuál fue el año con el mayor caudal máximo?

# E3. Repite la sección 5 con la estación Cojata (PP_000827_Cojata.xlsx).
#     ¿Cuántas fechas duplicadas tiene? ¿Cuántas solo tienen el registro 05:00?

# E4. Con pp_mensual, calcula la precipitación anual de Huancané considerando
#     solo los años con 12 meses válidos. ¿Qué años quedan fuera?

# E5. Haz un gráfico de la precipitación mensual de 2019 con geom_col().

# E6. Cambia max_na a 0 y luego a 10. ¿Cuántos años válidos de caudal
#     quedan en cada caso? ¿Qué criterio te parece más razonable?
