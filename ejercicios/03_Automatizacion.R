# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 03 - Automatización
# =============================================================================
#
# Condicionales (if/else, case_when), bucles for, funciones propias e
# iteración con purrr::map. Aplicado a las 5 estaciones de precipitación
# y al caudal de Huancané.
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes -------------------------------------------------------------------

library(tidyverse)
library(readxl)


# 2. Funciones propias ----------------------------------------------------------

# Todo lo que hicimos en 02_Procesamiento.R para limpiar un archivo SENAMHI,
# escrito UNA vez y guardado con un nombre.
leer_datos <- function(archivo) {
  read_excel(archivo, skip = 13) |>
    rename(fecha = 1, hora = 2, valor = 3) |>
    mutate(fecha = dmy(fecha)) |>
    arrange(fecha, hora) |>
    distinct(fecha, .keep_all = TRUE) |>          # regla 00:00 -> 05:00
    select(fecha, valor) |>
    complete(fecha = seq(min(fecha), max(fecha), by = "day"))
}

# Sirve igual para caudal o precipitación
q <- leer_datos("data/raw/caudal/Q_210201_PteCarretera_Huancane.xlsx")
nrow(q)                       #> 25692

pp_huancane <- leer_datos("data/raw/precipitacion/PP_000786_Huancane.xlsx")
nrow(pp_huancane)             #> 22939

# Una función con argumentos y valores por defecto:
# agrega a escala mensual con la regla de completitud
agregar_mensual <- function(datos, fun = sum, max_na = 3) {
  datos |>
    group_by(anio = year(fecha), mes = month(fecha)) |>
    summarise(n_na  = sum(is.na(valor)),
              valor = fun(valor, na.rm = TRUE),
              .groups = "drop") |>
    mutate(valor = if_else(n_na > max_na, NA, valor))
}

pp_mensual <- agregar_mensual(pp_huancane)                # suma (por defecto)
q_mensual  <- agregar_mensual(q, fun = mean)              # promedio
q_mensual0 <- agregar_mensual(q, fun = mean, max_na = 0)  # criterio estricto

# Y una función para pasar a escala anual (solo años con 12 meses válidos)
agregar_anual <- function(mensual, fun = sum) {
  mensual |>
    group_by(anio) |>
    summarise(n_meses = sum(!is.na(valor)),
              valor   = fun(valor)) |>
    filter(n_meses == 12)
}

q_anual <- agregar_anual(q_mensual, fun = mean)
nrow(q_anual)                 #> 62 años completos


# 3. Condicionales: if / else ---------------------------------------------------

q_medio <- mean(q_anual$valor)                     #> 17.9
q_2023  <- q_anual$valor[q_anual$anio == 2023]     #> 4.75

if (q_2023 < q_medio) {
  print("2023 fue un año por debajo del promedio")
} else {
  print("2023 fue un año por encima del promedio")
}

# Umbrales: percentiles 20 y 80 del caudal medio anual
umbrales <- quantile(q_anual$valor, c(0.2, 0.8))
umbrales
#>   20%   80%
#> 11.48 24.41

clasificar <- function(q, p20, p80) {
  if (q < p20) {
    "seco"
  } else if (q > p80) {
    "húmedo"
  } else {
    "normal"
  }
}
clasificar(q_2023, umbrales[1], umbrales[2])       #> "seco"

# if() trabaja con UN valor. Para una columna completa se usa case_when()
q_anual <- q_anual |>
  mutate(tipo = case_when(
    valor < umbrales[1] ~ "seco",
    valor > umbrales[2] ~ "húmedo",
    .default            = "normal"
  ))

count(q_anual, tipo)
#>   tipo       n
#>   húmedo    13
#>   normal    36
#>   seco      13

q_anual |> filter(tipo == "seco") |> arrange(valor) |> head(5)
#> 2023 4.75 | 1983 7.23 | 1967 7.67 | 1990 8.23 | 2009 9.29


# 4. Bucles for -----------------------------------------------------------------

archivos <- list.files("data/raw/precipitacion", pattern = "\\.xlsx$",
                       full.names = TRUE)
archivos                      # las 5 estaciones

# Nombre de la estación a partir del nombre del archivo
nombres <- archivos |> basename() |> str_remove("\\.xlsx$") |> str_remove("^PP_\\d+_")
nombres                       #> "Munani" "Huancane" "Ananea" "Cojata" "Putina"

# Bucle sencillo: repetir una tarea para cada archivo
for (i in seq_along(archivos)) {
  datos <- leer_datos(archivos[i])
  cat(nombres[i], ":", nrow(datos), "días\n")
}

# Bucle que guarda resultados: se prepara una lista vacía y se llena
resumen <- vector("list", length(archivos))

for (i in seq_along(archivos)) {
  datos <- leer_datos(archivos[i])
  anual <- datos |> agregar_mensual() |> agregar_anual()

  resumen[[i]] <- tibble(
    estacion     = nombres[i],
    inicio       = min(datos$fecha),
    fin          = max(datos$fecha),
    pct_faltante = round(mean(is.na(datos$valor)) * 100, 1),
    anios_comp   = nrow(anual),
    pp_anual     = round(mean(anual$valor[anual$anio %in% 1991:2020]), 1)
  )
}

resumen <- bind_rows(resumen)
resumen
#>   estacion inicio     fin        pct_faltante anios_comp pp_anual
#>   Munani   1964-05-01 2026-08-18         14.8         46     647.
#>   Huancane 1963-11-01 2026-08-20         17           47     658.
#>   Ananea   1963-09-18 2024-12-30         14.3         49     624.
#>   Cojata   1956-05-21 2026-04-30          1.5         59     711.
#>   Putina   1970-06-01 2026-08-18         14.7         40     640.


# 5. Iteración con purrr::map ---------------------------------------------------

# map() aplica una función a cada elemento y devuelve una lista
pp_lista <- map(archivos, leer_datos)
names(pp_lista) <- nombres
length(pp_lista)              #> 5
pp_lista$Cojata               # una estación

# list_rbind() une la lista en UNA tabla larga, con columna "estacion"
pp_todas <- pp_lista |> list_rbind(names_to = "estacion")
nrow(pp_todas)                #> 114159

# Con la tabla larga, group_by() reemplaza al bucle
pp_todas |>
  group_by(estacion) |>
  summarise(pct_faltante = round(mean(is.na(valor)) * 100, 1),
            maximo       = max(valor, na.rm = TRUE))

# Anual por estación (map sobre la lista, sin escribir el for)
pp_anual <- pp_lista |>
  map(\(d) d |> agregar_mensual() |> agregar_anual()) |>
  list_rbind(names_to = "estacion")


# 6. Gráfico de todas las estaciones ----------------------------------------------

# Un color por estación (vector con nombre: cada estación su color)
colores <- c(Ananea = "#0B5394", Cojata = "#6AA84F", Huancane = "#E69138",
             Munani = "#8E7CC3", Putina = "#CC4125")

g_estaciones <- ggplot(pp_anual, aes(x = anio, y = valor, fill = estacion)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.8) +    # línea base en 0
  facet_wrap(~ estacion, ncol = 1) +
  scale_x_continuous(breaks = seq(1950, 2030, 10)) +     # cada 10 años
  scale_fill_manual(values = colores, guide = "none") +  # sin leyenda: ya está en el panel
  labs(title = "Precipitación anual por estación (años completos)",
       x = NULL, y = "Precipitación (mm)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"),
        strip.text = element_text(face = "bold", hjust = 0))
g_estaciones


# 7. Exportar ---------------------------------------------------------------------

write_csv(pp_todas, "data/processed/PP_diario_estaciones.csv")
write_csv(resumen,  "outputs/tablas/Resumen_estaciones_PP.csv")
write_csv(q_anual,  "outputs/tablas/Q_anual_clasificado.csv")
ggsave("outputs/figuras/PP_anual_estaciones.png", g_estaciones,
       width = 10, height = 8, dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Modifica clasificar() para usar 5 clases con los percentiles 10, 30, 70
#     y 90: "muy seco", "seco", "normal", "húmedo", "muy húmedo".

# E2. Escribe una función pp_anual_media(archivo, inicio, fin) que devuelva la
#     precipitación anual media de una estación para el periodo indicado.
#     Pruébala con Cojata en 1991-2020 y en 1961-1990.

# E3. Con un for, guarda una figura del caudal (o la precipitación) diaria de
#     cada estación en outputs/figuras/ con ggsave(). Pista: paste0().

# E4. Usa map_dbl() para obtener en un solo vector el máximo diario de cada
#     estación. Pista: map_dbl(pp_lista, \(d) max(d$valor, na.rm = TRUE)).

# E5. ¿En qué años coinciden un caudal "seco" en Huancané y una precipitación
#     anual por debajo del percentil 20 en la estación Huancané?
#     Pista: inner_join() por la columna anio.
