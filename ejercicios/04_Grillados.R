# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 04 - Datos climáticos grillados
# =============================================================================
#
# Lectura de NetCDF con terra (PISCOp v3.0, precipitación diaria 1981-2025),
# agregación diaria -> mensual, recorte con la cuenca del río Huancané,
# climatología, mapas, precipitación areal y comparación con la estación.
# Evapotranspiración potencial por Hargreaves-Samani con la temperatura de
# cada subcuenca y balance P - ETP.
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes -------------------------------------------------------------------

library(tidyverse)
library(terra)       # datos grillados (ráster) y vectoriales
library(sf)          # para dibujar vectores con ggplot2 (geom_sf)


# 2. Preparación: recorte del archivo nacional (solo el instructor, una vez) ----

# PISCOp v3.0 (SENAMHI; Gutiérrez et al., 2026): precipitación DIARIA del Perú,
# 1981-2025, resolución 0.1°. El archivo nacional pesa 1.5 GB (16 436 días).
# Recortar con terra::crop() tarda más de 10 minutos porque lee día por día;
# ncdf4 lee directamente la ventana de la cuenca en menos de 1 segundo.
# El resultado (5.9 MB) ya viene en el repositorio: los participantes NO
# necesitan ejecutar esta sección.

archivo_pisco <- "data/raw/pisco/PISCOp_v3_diario_Huancane.nc"

if (!file.exists(archivo_pisco)) {
  nc  <- ncdf4::nc_open("data/raw/pisco/PISCOp_d.nc")
  lon <- ncdf4::ncvar_get(nc, "longitude")
  lat <- ncdf4::ncvar_get(nc, "latitude")
  ix  <- which(lon > -70.3 & lon < -69.2)          # 11 columnas
  iy  <- which(lat > -15.5 & lat < -14.4)          # 11 filas
  a   <- ncdf4::ncvar_get(nc, "precipitation",
                          start = c(min(ix), min(iy), 1),
                          count = c(length(ix), length(iy), -1))
  ncdf4::nc_close(nc)

  recorte <- rast(aperm(a, c(2, 1, 3)),            # [lat, lon, tiempo]
                  extent = ext(-70.3, -69.2, -15.5, -14.4),
                  crs = "EPSG:4326")
  # El archivo nacional no trae fechas (eje "Z1" = 1 ... 16436)
  time(recorte) <- seq(as.Date("1981-01-01"), by = "day",
                       length.out = nlyr(recorte))
  writeCDF(recorte, archivo_pisco, varname = "pp", unit = "mm",
           longname = "PISCOp v3.0 precipitacion diaria", compression = 7)
}


# 3. Leer un NetCDF -------------------------------------------------------------

pp_dia <- rast(archivo_pisco)
pp_dia
#> class       : SpatRaster
#> size        : 11, 11, 16436  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : -70.3, -69.2, -15.5, -14.4  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> varname     : pp (PISCOp v3.0 precipitacion diaria)
#> unit        : mm
#> time (days) : 1981-01-01 to 2025-12-31 (16436 steps)

nlyr(pp_dia)                  #> 16436 capas = 16436 días
res(pp_dia)                   #> 0.1 0.1  (~11 km)
range(time(pp_dia))           #> "1981-01-01" "2025-12-31"

# Una capa es un mapa; se selecciona por posición con [[ ]]
plot(pp_dia[[1]], main = "PISCOp v3 - 1 de enero de 1981 (mm)")


# 4. De diario a mensual con tapp() ----------------------------------------------

# tapp() agrupa capas según su fecha y aplica una función:
# "yearmonths" = cada mes de cada año; sum = lluvia total del mes
pp <- tapp(pp_dia, "yearmonths", sum)
nlyr(pp)                      #> 540 meses = 45 años

fechas    <- seq(as.Date("1981-01-01"), by = "month", length.out = nlyr(pp))
time(pp)  <- fechas
names(pp) <- format(fechas, "%Y-%m")

plot(pp[["2025-01"]], main = "PISCOp v3 - enero 2025 (mm)")


# 5. La cuenca: datos vectoriales -----------------------------------------------

subcuencas <- vect("data/basin/Subbasins.shp")
rios       <- vect("data/rivers/Rivers.shp")
subcuencas
#> geometry    : polygons
#> dimensions  : 5, 3  (geometries, attributes)
#> coord. ref. : WGS 84 / UTM zone 19S (EPSG:32719)

# El ráster está en grados (lon/lat) y la cuenca en metros (UTM 19S):
# hay que llevarlos al MISMO sistema de coordenadas.
subcuencas <- project(subcuencas, crs(pp))
rios       <- project(rios, crs(pp))

# Unir (disolver) las 5 subcuencas en una sola cuenca.
# fillHoles() elimina huecos diminutos que quedan entre subcuencas vecinas.
cuenca <- aggregate(subcuencas) |> fillHoles()
expanse(cuenca, unit = "km")  #> 3545 km²

# Estaciones SENAMHI (coordenadas de data/raw/README.md)
estaciones <- tibble(
  estacion = c("Ananea", "Munani", "Putina", "Huancane", "Cojata"),
  lat      = c(-14.6763, -14.766944, -14.921, -15.20687, -15.0118),
  lon      = c(-69.5339, -69.951806, -69.8757, -69.7583, -69.3612)
)
est_v <- vect(estaciones, geom = c("lon", "lat"), crs = "EPSG:4326")

# Punto de salida: el tramo de río que no desemboca en otra subcuenca
# (SubbasinR = 0). Las líneas están digitalizadas desde aguas abajo, así que
# su primer vértice es la salida: estación hidrométrica Puente Carretera.
tramo_salida <- rios[rios$SubbasinR == 0]
salida <- crds(tramo_salida)[1, ]
salida
#>         x         y
#> -69.79255 -15.21564
salida_v <- vect(matrix(salida, ncol = 2), crs = crs(rios))

plot(pp[["2025-01"]])
lines(cuenca, lwd = 2)
lines(rios, col = "blue")
points(est_v, pch = 17)

# crop() recorta al rectángulo; mask = TRUE pone NA fuera del polígono
pp_cuenca <- crop(pp, cuenca, mask = TRUE)
plot(pp_cuenca[["2025-01"]])
lines(cuenca)


# 6. Climatología 1991-2020 -------------------------------------------------------

# Normal climatológica: periodo estándar de 30 años (OMM)
pp_normal <- pp[[year(fechas) %in% 1991:2020]]
nlyr(pp_normal)               #> 360

pp_anual <- tapp(pp_normal, "years", sum)    # 30 mapas: total de cada año
pp_media <- mean(pp_anual)                   # 1 mapa: media 1991-2020

global(crop(pp_media, cuenca, mask = TRUE), range, na.rm = TRUE)
#> 560 a 942 mm/año

# Régimen mensual: promedio de cada mes calendario (12 mapas)
pp_regimen <- tapp(pp_normal, "months", mean)
names(pp_regimen) <- month.abb
plot(pp_regimen, nc = 4)


# 7. Mapa con ggplot2 -------------------------------------------------------------

# ggplot2 necesita una tabla: as.data.frame(xy = TRUE) da x, y, valor
pp_df <- as.data.frame(pp_media, xy = TRUE) |>
  rename(pp = mean)

g_mapa <- ggplot() +
  # geom_tile() dibuja cada celda; color = borde -> se ve la grilla de PISCO
  geom_tile(data = pp_df, aes(x, y, fill = pp), color = "grey85", linewidth = 0.2) +
  geom_sf(data = st_as_sf(subcuencas), fill = NA, color = "white", linewidth = 0.3) +
  geom_sf(data = st_as_sf(cuenca), fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = st_as_sf(rios), color = "#00BFFF", linewidth = 0.6) +
  geom_sf(data = st_as_sf(est_v), shape = 17, size = 3) +
  geom_text(data = estaciones, aes(lon, lat, label = estacion),
            nudge_y = 0.05, size = 3.5, fontface = "bold") +
  # Punto de salida de la cuenca
  geom_sf(data = st_as_sf(salida_v), shape = 21, size = 4.5,
          fill = "#FF6732", color = "black", stroke = 1) +
  annotate("label", x = salida[1] + 0.03, y = salida[2] - 0.06,
           label = "Salida: Pte. Carretera", hjust = 0, size = 3.5,
           fontface = "bold", color = "#FF6732") +
  # Flecha del norte
  annotate("segment", x = -70.22, xend = -70.22, y = -14.66, yend = -14.50,
           arrow = arrow(length = unit(0.35, "cm"), type = "closed"),
           linewidth = 1.2) +
  annotate("text", x = -70.22, y = -14.46, label = "N",
           fontface = "bold", size = 6) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  labs(title = "Precipitación media anual 1991-2020 (PISCOp v3)",
       fill = "mm/año", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", color = "#073763"))
g_mapa


# 7.1 Climatología mensual en la cuenca (enero a diciembre) ---------------------

meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

# Solo las celdas de la cuenca (touches = TRUE incluye las del borde)
regimen_cuenca <- mask(crop(pp_regimen, cuenca), cuenca, touches = TRUE)

# Precipitación areal de cada mes, para el título de cada panel
pp_mes_areal <- extract(pp_regimen, cuenca, fun = mean, weights = TRUE,
                        na.rm = TRUE, ID = FALSE) |>
  unlist() |>
  round()
pp_mes_areal
#> Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 134 135 101  40  15   9   5   9  25  56  69 119

etiquetas <- paste0(meses, ": ", pp_mes_areal, " mm")

# 12 capas -> tabla larga: x, y, mes, pp
regimen_df <- as.data.frame(regimen_cuenca, xy = TRUE) |>
  pivot_longer(-c(x, y), names_to = "mes", values_to = "pp") |>
  mutate(mes = factor(mes, levels = month.abb, labels = etiquetas))

g_regimen <- ggplot() +
  geom_tile(data = regimen_df, aes(x, y, fill = pp), color = "grey85", linewidth = 0.1) +
  geom_sf(data = st_as_sf(cuenca), fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = st_as_sf(rios), color = "#00BFFF", linewidth = 0.4) +
  facet_wrap(~ mes, ncol = 4) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  labs(title = "Climatología mensual de la precipitación 1991-2020 (PISCOp v3)",
       fill = "mm/mes", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", color = "#073763"))
g_regimen


# 8. Precipitación areal ----------------------------------------------------------

# extract() con fun = mean promedia las celdas dentro del polígono.
# weights = TRUE pondera cada celda por la fracción que cae dentro de la cuenca.
pp_areal <- extract(pp, cuenca, fun = mean, weights = TRUE, na.rm = TRUE, ID = FALSE)
dim(pp_areal)                 #> 1 fila (la cuenca) x 540 columnas (meses)

# De "ancho" (una columna por mes) a una serie de tiempo
pp_cuenca_mes <- pp_areal |>
  pivot_longer(everything(), names_to = "mes", values_to = "pp") |>
  mutate(fecha = fechas) |>
  select(fecha, pp)

pp_cuenca_anual <- pp_cuenca_mes |>
  group_by(anio = year(fecha)) |>
  summarise(pp = sum(pp))

pp_cuenca_anual |> filter(anio %in% 1991:2020) |> summarise(pp_media = mean(pp))
#> 716 mm/año

# Años más secos y más lluviosos de 1981-2025
pp_cuenca_anual |> arrange(pp) |> slice(c(1:3, (n() - 2):n()))
#>   anio    pp
#>   1983   526     <- más secos
#>   2022   533
#>   1988   584
#>   2002   898
#>   1985   991
#>   1984  1080     <- más lluvioso

# Lo mismo para cada subcuenca: el código no cambia, solo el polígono
pp_sub <- extract(pp_media, subcuencas, fun = mean, weights = TRUE, na.rm = TRUE)
tibble(subcuenca = subcuencas$Subbasin,
       area_km2  = round(expanse(subcuencas, unit = "km")),
       pp_anual  = round(pp_sub$mean)) |>
  arrange(subcuenca)
#>   subcuenca area_km2 pp_anual
#>           1      892      737
#>           2      948      819
#>           3      231      689
#>           4     1106      650
#>           5      367      619


# 9. PISCO frente a la estación Huancané ----------------------------------------

# Valor de la celda que contiene a la estación
huancane_v  <- est_v[est_v$estacion == "Huancane"]
pisco_punto <- extract(pp, huancane_v, ID = FALSE) |>
  pivot_longer(everything(), values_to = "pisco") |>
  mutate(fecha = fechas) |>
  select(fecha, pisco)

# Serie mensual observada (guardada en 02_Procesamiento.R)
obs <- read_csv("data/processed/PP_mensual_Huancane.csv", show_col_types = FALSE) |>
  mutate(fecha = make_date(anio, mes, 1)) |>
  select(fecha, obs = pp)

comparacion <- inner_join(obs, pisco_punto, by = "fecha") |>
  drop_na()
nrow(comparacion)             #> 413 meses

comparacion |>
  summarise(r     = cor(obs, pisco),
            sesgo = mean(pisco - obs),
            pbias = 100 * sum(pisco - obs) / sum(obs))
#>       r sesgo pbias
#>   0.950 -2.36 -4.13

g_comp <- ggplot(comparacion, aes(obs, pisco)) +
  geom_abline(linetype = "dashed", color = "grey40") +
  geom_point(color = "#0B5394", alpha = 0.6) +
  coord_equal() +
  labs(title = "Huancané: estación vs PISCOp v3 (mensual)",
       x = "Observado (mm/mes)", y = "PISCOp v3 (mm/mes)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", color = "#073763"))
g_comp


# 10. Temperatura por subcuenca ---------------------------------------------------

# Tmax y Tmin diarias, promedio areal de cada subcuenca (Sb1 ... Sb5),
# obtenidas de PISCO temperatura diaria 1981-2020 (SENAMHI):
#   Tmax: https://doi.org/10.6084/m9.figshare.20522806
#   Tmin: https://doi.org/10.6084/m9.figshare.20533715
tmax <- read_csv("data/temp/Datos_tmax.csv", show_col_types = FALSE)
tmin <- read_csv("data/temp/Datos_tmin.csv", show_col_types = FALSE)
tmax
#>   Date         Sb1   Sb3   Sb2   Sb5   Sb4
#>   1981-01-01 13.9  15.8  14.0  15.9  14.7

# Formato largo y una sola tabla con ambas variables
a_largo <- function(datos, nombre) {
  datos |>
    pivot_longer(-Date, names_to = "subcuenca", values_to = nombre) |>
    mutate(subcuenca = parse_number(subcuenca)) |>
    rename(fecha = Date)
}

temp <- inner_join(a_largo(tmax, "tmax"), a_largo(tmin, "tmin"),
                   by = c("fecha", "subcuenca"))
nrow(temp)                    #> 73050 (14610 días x 5 subcuencas)

# Control de calidad: ¿hay días con tmin > tmax?
temp |> filter(tmin > tmax) |> nrow()
#> 0


# 11. ETP por Hargreaves-Samani -------------------------------------------------

# ETP = 0.0023 * Ra * (Tmedia + 17.8) * sqrt(Tmax - Tmin)
# Ra: radiación extraterrestre (mm/día), depende de la latitud y del día del año
# (FAO-56, ecuaciones 21 a 25)

radiacion_ext <- function(lat, fecha) {
  phi <- lat * pi / 180                          # latitud en radianes
  J   <- yday(fecha)                             # día del año
  dr  <- 1 + 0.033 * cos(2 * pi * J / 365)       # distancia Tierra-Sol
  dec <- 0.409 * sin(2 * pi * J / 365 - 1.39)    # declinación solar
  ws  <- acos(-tan(phi) * tan(dec))              # ángulo horario de puesta
  ra  <- 24 * 60 / pi * 0.0820 * dr *
    (ws * sin(phi) * sin(dec) + cos(phi) * cos(dec) * sin(ws))
  ra * 0.408                                     # MJ/m²/día -> mm/día
}

hargreaves <- function(tmax, tmin, lat, fecha) {
  tmedia <- (tmax + tmin) / 2
  0.0023 * radiacion_ext(lat, fecha) * (tmedia + 17.8) * sqrt(pmax(tmax - tmin, 0))
}

# Latitud del centroide de cada subcuenca
info_sub <- tibble(
  subcuenca = subcuencas$Subbasin,
  lat       = crds(centroids(subcuencas))[, "y"],
  area_km2  = expanse(subcuencas, unit = "km")
)
info_sub
#>   subcuenca   lat area_km2
#>           1 -14.7      892
#>           3 -15.0      231
#>           2 -14.9      948
#>           5 -15.1      367
#>           4 -15.2     1106

etp_dia <- temp |>
  left_join(info_sub, by = "subcuenca") |>
  mutate(etp = hargreaves(tmax, tmin, lat, fecha))

# Diaria -> mensual (suma) por subcuenca
etp_sub_mes <- etp_dia |>
  group_by(subcuenca, area_km2, fecha = floor_date(fecha, "month")) |>
  summarise(etp = sum(etp), .groups = "drop")

# Subcuencas -> cuenca: promedio ponderado por área
etp_cuenca_mes <- etp_sub_mes |>
  group_by(fecha) |>
  summarise(etp = weighted.mean(etp, area_km2))


# 12. Balance P - ETP 1991-2020 ---------------------------------------------------

# La temperatura llega a 2020: el balance usa la normal 1991-2020
balance <- inner_join(pp_cuenca_mes, etp_cuenca_mes, by = "fecha") |>
  filter(year(fecha) %in% 1991:2020)
range(balance$fecha)          #> "1991-01-01" "2020-12-01"

balance |>
  group_by(anio = year(fecha)) |>
  summarise(pp = sum(pp), etp = sum(etp)) |>
  summarise(pp = mean(pp), etp = mean(etp), indice_aridez = pp / etp)
#>      pp   etp indice_aridez
#>     716  1115         0.642

regimen <- balance |>
  group_by(mes = month(fecha)) |>
  summarise(pp = mean(pp), etp = mean(etp)) |>
  mutate(balance = pp - etp)
regimen
#>   mes    pp   etp balance
#>     1 134.  100.     34.0
#>     2 135.   89.4    46.1
#>     3 101.   94.1     7.0
#>   ...
#>     8   8.9  89.4   -80.4
#>   ...
#>    12 119.  107.     11.7

g_balance <- regimen |>
  pivot_longer(c(pp, etp), names_to = "variable", values_to = "mm") |>
  mutate(variable = recode(variable, pp = "Precipitación (PISCOp v3)",
                           etp = "ETP (Hargreaves)")) |>
  ggplot(aes(x = mes, y = mm, color = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = c("#FF6732", "#0B5394")) +
  labs(title = "Balance climático de la cuenca Huancané (1991-2020)",
       x = NULL, y = "mm/mes", color = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"))
g_balance


# 13. Exportar ----------------------------------------------------------------------

# Series mensuales areales de la cuenca: entrada del modelo hidrológico (Día 2)
pp_cuenca_mes |>
  left_join(etp_cuenca_mes, by = "fecha") |>
  write_csv("data/processed/PP_ETP_mensual_cuenca.csv")   # etp = NA desde 2021

writeRaster(pp_media, "outputs/PP_media_anual_PISCOv3.tif", overwrite = TRUE)

ggsave("outputs/figuras/PISCO_mapa_pp.png",     g_mapa,    width = 8, height = 7,   dpi = 150)
ggsave("outputs/figuras/PISCO_regimen_mensual.png", g_regimen, width = 10, height = 8.5, dpi = 150)
ggsave("outputs/figuras/PISCO_vs_estacion.png", g_comp,    width = 6, height = 6,   dpi = 150)
ggsave("outputs/figuras/Balance_P_ETP.png",     g_balance, width = 8, height = 4.5, dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Con pp_regimen, haz el mapa de enero y de julio. ¿Cuántas veces llueve
#     más en enero que en julio dentro de la cuenca?

# E2. Con tapp(pp_dia, "years", max) obtén el mapa de la lluvia máxima diaria
#     de cada año. ¿Cuál fue la máxima diaria areal de la cuenca en 1981-2025?

# E3. Repite la comparación de la sección 9 para Muñani y Putina.
#     ¿En cuál se parece más PISCO a lo observado? Pista: escribe una función.

# E4. Compara la precipitación areal de la cuenca calculada con el ráster
#     (pp_cuenca_mes) con la de data/pcp/Datos_pcp.csv ponderada por área.
#     ¿Coinciden? Pista: a_largo() y weighted.mean().

# E5. ¿Cuántos meses al año tiene la cuenca déficit (pp < etp) en promedio?
#     ¿En qué año hubo más meses con déficit?
