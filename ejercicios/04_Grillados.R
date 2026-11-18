# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 04 - Datos climáticos grillados
# =============================================================================
#
# Lectura de NetCDF con terra (PISCO precipitación), recorte con la cuenca
# del río Huancané, climatología, mapas, precipitación areal y comparación
# con la estación. Evapotranspiración potencial por Hargreaves-Samani con la
# temperatura de cada subcuenca y balance P - ETP.
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


# 2. Descarga de PISCO (solo la primera vez) ------------------------------------

# PISCOp v2.1 (SENAMHI): precipitación mensual, 1981-2016, resolución 0.1°.
# Se distribuye en la IRI Data Library. La URL permite pedir SOLO un recuadro
# (X = longitud, Y = latitud) alrededor de la cuenca: ~200 KB en lugar de GB.

url_pisco <- paste0(
  "https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/",
  ".Prec/.v2p1/.stable/.monthly/.Prec/",
  "X/(-70.3)/(-69.2)/RANGEEDGES/Y/(-15.5)/(-14.4)/RANGEEDGES/data.nc"
)
archivo_pp <- "data/raw/pisco/PISCOp_v2.1_mensual.nc"

if (!file.exists(archivo_pp)) {
  dir.create("data/raw/pisco", showWarnings = FALSE)
  download.file(url_pisco, archivo_pp, mode = "wb")
}


# 3. Leer un NetCDF -------------------------------------------------------------

pp <- rast(archivo_pp)
pp
#> class       : SpatRaster
#> size        : 11, 11, 432  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : -70.3, -69.2, -15.5, -14.4  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84)
#> varname     : Prec (Precipitation)
#> unit        : mm/month

nlyr(pp)                      #> 432 capas = 432 meses = 36 años
res(pp)                       #> 0.1 0.1  (~11 km)

# El tiempo viene como "meses desde 1960-01-01" y terra no lo convierte.
# Lo asignamos nosotros: 432 meses desde enero de 1981.
fechas <- seq(as.Date("1981-01-01"), by = "month", length.out = nlyr(pp))
range(fechas)                 #> "1981-01-01" "2016-12-01"

time(pp)  <- fechas
names(pp) <- format(fechas, "%Y-%m")

# Una capa es un mapa; se selecciona con [[ ]]
plot(pp[["2010-01"]], main = "PISCOp - enero 2010 (mm)")


# 4. La cuenca: datos vectoriales -----------------------------------------------

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

# Unir (disolver) las 5 subcuencas en una sola cuenca
cuenca <- aggregate(subcuencas)
expanse(cuenca, unit = "km")  #> 3545 km²

# Estaciones SENAMHI (coordenadas de data/raw/README.md)
estaciones <- tibble(
  estacion = c("Ananea", "Munani", "Putina", "Huancane", "Cojata"),
  lat      = c(-14.6763, -14.766944, -14.921, -15.20687, -15.0118),
  lon      = c(-69.5339, -69.951806, -69.8757, -69.7583, -69.3612)
)
est_v <- vect(estaciones, geom = c("lon", "lat"), crs = "EPSG:4326")

plot(pp[["2010-01"]])
lines(cuenca, lwd = 2)
lines(rios, col = "blue")
points(est_v, pch = 17)


# 5. Recortar con la cuenca -------------------------------------------------------

# crop() recorta al rectángulo; mask = TRUE pone NA fuera del polígono
pp_cuenca <- crop(pp, cuenca, mask = TRUE)
plot(pp_cuenca[["2010-01"]])
lines(cuenca)


# 6. Climatología ---------------------------------------------------------------

# tapp() agrupa capas por tiempo: "years" suma los 12 meses de cada año
pp_anual <- tapp(pp, "years", sum)
nlyr(pp_anual)                #> 36

# Media de los 36 años: un solo mapa
pp_media <- mean(pp_anual)

# Rango dentro de la cuenca
global(crop(pp_media, cuenca, mask = TRUE), range, na.rm = TRUE)
#> 525 a 890 mm/año

# Régimen mensual: promedio de cada mes calendario (12 capas)
pp_mes <- tapp(pp, "months", mean)
names(pp_mes) <- month.abb
plot(pp_mes, nc = 4)


# 7. Mapa con ggplot2 -------------------------------------------------------------

# ggplot2 necesita una tabla: as.data.frame(xy = TRUE) da x, y, valor
pp_df <- as.data.frame(pp_media, xy = TRUE) |>
  rename(pp = mean)

g_mapa <- ggplot() +
  geom_raster(data = pp_df, aes(x, y, fill = pp)) +
  geom_sf(data = st_as_sf(subcuencas), fill = NA, color = "white", linewidth = 0.3) +
  geom_sf(data = st_as_sf(cuenca), fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = st_as_sf(rios), color = "#00BFFF", linewidth = 0.6) +
  geom_sf(data = st_as_sf(est_v), shape = 17, size = 3) +
  geom_text(data = estaciones, aes(lon, lat, label = estacion),
            nudge_y = 0.05, size = 3.5, fontface = "bold") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  labs(title = "Precipitación media anual 1981-2016 (PISCOp v2.1)",
       fill = "mm/año", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", color = "#073763"))
g_mapa


# 8. Precipitación areal ----------------------------------------------------------

# extract() con fun = mean promedia las celdas dentro del polígono.
# weights = TRUE pondera cada celda por la fracción que cae dentro de la cuenca.
pp_areal <- extract(pp, cuenca, fun = mean, weights = TRUE, na.rm = TRUE, ID = FALSE)
dim(pp_areal)                 #> 1 fila (la cuenca) x 432 columnas (meses)

# De "ancho" (una columna por mes) a una serie de tiempo
pp_cuenca_mes <- pp_areal |>
  pivot_longer(everything(), names_to = "mes", values_to = "pp") |>
  mutate(fecha = fechas) |>
  select(fecha, pp)

pp_cuenca_mes |>
  group_by(anio = year(fecha)) |>
  summarise(pp = sum(pp)) |>
  summarise(pp_media = mean(pp))
#> 695 mm/año

# Lo mismo para cada subcuenca: el código no cambia, solo el polígono
pp_sub <- extract(pp_media, subcuencas, fun = mean, weights = TRUE, na.rm = TRUE)
tibble(subcuenca = subcuencas$Subbasin,
       area_km2  = round(expanse(subcuencas, unit = "km")),
       pp_anual  = round(pp_sub$mean)) |>
  arrange(subcuenca)
#>   subcuenca area_km2 pp_anual
#>           1      892      760
#>           2      948      782
#>           3      231      642
#>           4     1106      611
#>           5      367      602


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
nrow(comparacion)             #> 353 meses

comparacion |>
  summarise(r     = cor(obs, pisco),
            sesgo = mean(pisco - obs),
            pbias = 100 * sum(pisco - obs) / sum(obs))
#>       r sesgo pbias
#>   0.971 -3.70 -6.37

g_comp <- ggplot(comparacion, aes(obs, pisco)) +
  geom_abline(linetype = "dashed", color = "grey40") +
  geom_point(color = "#0B5394", alpha = 0.6) +
  coord_equal() +
  labs(title = "Huancané: estación vs PISCOp (mensual)",
       x = "Observado (mm/mes)", y = "PISCOp (mm/mes)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", color = "#073763"))
g_comp


# 10. Temperatura por subcuenca ---------------------------------------------------

# Tmax y Tmin diarias, promedio areal de cada subcuenca (Sb1 ... Sb5)
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

etp_cuenca_mes |>
  group_by(anio = year(fecha)) |>
  summarise(etp = sum(etp)) |>
  summarise(etp_media = mean(etp))
#> 1105 mm/año (1981-2020)


# 12. Balance P - ETP -----------------------------------------------------------

# PISCOp v2.1 llega a 2016 y la temperatura a 2020: se usa el periodo común
balance <- inner_join(pp_cuenca_mes, etp_cuenca_mes, by = "fecha")
range(balance$fecha)          #> "1981-01-01" "2016-12-01"

balance |>
  group_by(anio = year(fecha)) |>
  summarise(pp = sum(pp), etp = sum(etp)) |>
  summarise(pp = mean(pp), etp = mean(etp), indice_aridez = pp / etp)
#>      pp   etp indice_aridez
#>     695  1101         0.631

regimen <- balance |>
  group_by(mes = month(fecha)) |>
  summarise(pp = mean(pp), etp = mean(etp)) |>
  mutate(balance = pp - etp)
regimen
#>   mes    pp   etp balance
#>     1 143.   98.4    44.2
#>     2 112.   88.8    23.7
#>     3  92.6  93.6    -1.0
#>   ...
#>     7   5.3  75.9   -70.6
#>   ...
#>    12 119.  106.     13.2

g_balance <- regimen |>
  pivot_longer(c(pp, etp), names_to = "variable", values_to = "mm") |>
  mutate(variable = recode(variable, pp = "Precipitación (PISCOp)",
                           etp = "ETP (Hargreaves)")) |>
  ggplot(aes(x = mes, y = mm, color = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = c("#FF6732", "#0B5394")) +
  labs(title = "Balance climático de la cuenca Huancané (1981-2016)",
       x = NULL, y = "mm/mes", color = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"))
g_balance


# 13. Exportar ----------------------------------------------------------------------

# Serie mensual areal: será la entrada del modelo hidrológico del Día 2
write_csv(balance, "data/processed/PP_ETP_mensual_cuenca.csv")

writeRaster(pp_media, "outputs/PP_media_anual_PISCO.tif", overwrite = TRUE)

ggsave("outputs/figuras/PISCO_mapa_pp.png",     g_mapa,    width = 8, height = 7,   dpi = 150)
ggsave("outputs/figuras/PISCO_vs_estacion.png", g_comp,    width = 6, height = 6,   dpi = 150)
ggsave("outputs/figuras/Balance_P_ETP.png",     g_balance, width = 8, height = 4.5, dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Con pp_mes, haz el mapa de enero y de julio. ¿Cuántas veces llueve más
#     en enero que en julio dentro de la cuenca?

# E2. Calcula la precipitación areal de la cuenca para el año hidrológico
#     (septiembre a agosto) 2015-2016. Pista: filter() sobre pp_cuenca_mes.

# E3. Repite la comparación de la sección 9 para Muñani y Putina.
#     ¿En cuál se parece más PISCO a lo observado? Pista: escribe una función.

# E4. Calcula la ETP anual de cada subcuenca con etp_sub_mes.
#     ¿Coincide la subcuenca con más ETP con la de menor precipitación?

# E5. ¿Cuántos meses al año tiene la cuenca déficit (pp < etp) en promedio?
#     ¿En qué año hubo más meses con déficit?
