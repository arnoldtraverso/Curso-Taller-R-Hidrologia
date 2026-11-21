# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 07 - Modelamiento I: GR4J semidistribuido con airGRiwrm
# =============================================================================
#
# Modelo GR4J aplicado a las 5 subcuencas del río Huancané, conectadas como una
# red con el paquete airGRiwrm. Se calibra con el caudal de la estación
# Huancané (Pte. Carretera) y se valida en un periodo independiente.
#
# Datos de entrada (ya preparados en el Día 1):
#   data/pcp/Datos_pcp.csv       precipitación diaria por subcuenca (PISCOp v3)
#   data/temp/Datos_tmax.csv     Tmax diaria por subcuenca (PISCO, 1981-2020)
#   data/temp/Datos_tmin.csv     Tmin diaria por subcuenca
#   data/processed/Q_diario_Huancane.csv   caudal diario observado (m³/s)
#   data/basin, data/rivers      subcuencas y tramos de río
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes -------------------------------------------------------------------

library(tidyverse)
library(terra)
library(airGRiwrm)    # redes de subcuencas; carga también airGR (GR4J)
library(hydroGOF)     # métricas de desempeño (KGE, NSE, PBIAS)
select <- dplyr::select   # por si qmap/MASS (script 06) sigue cargado en la sesión

# Colores del curso
azul    <- "#0B5394"
celeste <- "#9FC5E8"
naranja <- "#FF6732"


# 2. La red de subcuencas ------------------------------------------------------

subcuencas <- vect("data/basin/Subbasins.shp")
rios       <- vect("data/rivers/Rivers.shp")

# La tabla de atributos de los ríos ya describe la red:
#   Subbasin  = subcuenca del tramo
#   SubbasinR = subcuenca a la que drena (0 = salida de la cuenca)
#   AreaC     = área acumulada aguas arriba (ha)
#   Len2      = longitud del tramo (m)
as.data.frame(rios)[, c("Subbasin", "SubbasinR", "AreaC", "Len2")]
#>   Subbasin SubbasinR  AreaC    Len2
#>          1         3  89179 20916.7
#>          2         3  94757 22737.4
#>          3         5 207029 36879.9
#>          4         5 110527 46629.2
#>          5         0 354240 31090.7

# airGRiwrm necesita una tabla con 5 columnas:
#   id     = nombre del nodo (la salida de cada subcuenca)
#   down   = nodo aguas abajo (NA en la salida de la cuenca)
#   length = distancia hasta el nodo aguas abajo (km) = largo del tramo de abajo
#   area   = área ACUMULADA en el nodo (km²)
#   model  = modelo del nodo: GR4J donde hay estación, "Ungauged" donde no
tramos <- as.data.frame(rios) |>
  as_tibble() |>
  mutate(largo_km = Len2 / 1000)

red <- tramos |>
  mutate(id   = as.character(Subbasin),
         down = as.character(SubbasinR),
         down = na_if(down, "0")) |>              # 0 = salida -> NA
  transmute(
    id, down,
    length = largo_km[match(down, id)],        # largo del tramo de abajo
    area   = AreaC / 100,                      # ha -> km²
    model  = if_else(is.na(down), "RunModel_GR4J", "Ungauged")
  )
red
#>   id    down  length  area model
#>   1     3       36.9   892 Ungauged
#>   2     3       36.9   948 Ungauged
#>   3     5       31.1  2070 Ungauged
#>   4     5       31.1  1105 Ungauged
#>   5     NA      NA    3542 RunModel_GR4J

griwrm <- CreateGRiwrm(red)
#> Ungauged node '1' automatically gets the node '5' as parameter donor
#> ...
griwrm
#>   id down length    area         model donor
#>    1    3 36.88  891.79      Ungauged     5
#>    2    3 36.88  947.57      Ungauged     5
#>    4    5 31.09 1105.27      Ungauged     5
#>    3    5 31.09 2070.29      Ungauged     5
#>    5 <NA>    NA 3542.40 RunModel_GR4J     5

# Solo la subcuenca 5 tiene estación: las otras 4 son "Ungauged" y reciben
# los parámetros de la 5 (donante). El X4 se ajusta por el área de cada una.
# Nota: plot(griwrm) dibuja la red con el servicio en línea mermaid.ink
# (necesita internet). Abajo hacemos el mismo esquema con ggplot2.


# 3. Gráficos: cómo se configura la cuenca -------------------------------------

# 3.1 Mapa de la red ---------------------------------------------------------

sub_ll  <- project(subcuencas, "EPSG:4326")
rios_ll <- project(rios, "EPSG:4326")

# Centroide de cada subcuenca (para las etiquetas y las flechas)
centros <- tibble(
  id   = as.character(sub_ll$Subbasin),
  x    = crds(centroids(sub_ll))[, "x"],
  y    = crds(centroids(sub_ll))[, "y"],
  area_local = expanse(sub_ll, unit = "km")
) |>
  left_join(as_tibble(griwrm) |> dplyr::select(id, down, model), by = "id")

# Nodo = salida de cada subcuenca = primer vértice de su tramo de río
nodos <- tibble(
  id = as.character(rios_ll$Subbasin),
  x  = map_dbl(seq_len(nrow(rios_ll)), \(i) geom(rios_ll[i])[1, "x"]),
  y  = map_dbl(seq_len(nrow(rios_ll)), \(i) geom(rios_ll[i])[1, "y"])
)

# Los nodos 1 y 2 caen en la misma confluencia, igual que los nodos 3 y 4
etiquetas_nodo <- nodos |>
  group_by(x, y) |>
  summarise(nombre = paste0("N", id, collapse = " · "), .groups = "drop")

# Flechas: cada subcuenca entrega su escorrentía a su nodo de salida
flechas <- centros |>
  left_join(dplyr::select(nodos, id, xend = x, yend = y), by = "id")

salida <- filter(nodos, id == "5")

mapa_sub <- sf::st_as_sf(sub_ll) |>
  mutate(id = as.character(Subbasin),
         tipo = if_else(id == "5", "Con estación: se calibra",
                        "Sin estación: hereda parámetros"))

g_red_mapa <- ggplot() +
  geom_sf(data = mapa_sub, aes(fill = tipo), color = "white", linewidth = 0.8) +
  geom_sf(data = sf::st_as_sf(rios_ll), color = azul, linewidth = 1.1) +
  geom_curve(data = flechas, aes(x, y, xend = xend, yend = yend),
             curvature = 0.25, linewidth = 0.9, color = naranja,
             arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  geom_label(data = centros, aes(x, y, label = paste0("SB ", id, "\n",
                                                      round(area_local), " km²")),
             size = 3.2, linewidth = 0, fill = alpha("white", 0.85)) +
  geom_point(data = nodos, aes(x, y), shape = 21, fill = "white",
             color = "black", size = 3, stroke = 1.2) +
  geom_point(data = salida, aes(x, y), shape = 24, fill = naranja, size = 4.5) +
  geom_text(data = etiquetas_nodo, aes(x, y, label = nombre),
            nudge_x = -0.035, hjust = 1, fontface = "bold", size = 3.6) +
  annotate("text", x = salida$x + 0.03, y = salida$y - 0.03,
           label = "Estación Huancané\n(Pte. Carretera)", hjust = 0, size = 3.2) +
  scale_fill_manual(values = c(azul, celeste), name = NULL) +
  labs(title = "Red de subcuencas del modelo",
       subtitle = "Flechas: cada subcuenca drena a su nodo  ·  N = nodo",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#073763"))
g_red_mapa


# 3.2 Esquema de la red (equivalente a plot(griwrm)) ------------------------

# Posición horizontal: cuántos pasos hay hasta la salida
pasos_salida <- function(id, red) {
  n <- 0
  while (!is.na(id)) {
    id <- red$down[red$id == id]
    n  <- n + 1
  }
  n - 1
}

esquema <- as_tibble(griwrm) |>
  mutate(pasos = map_int(id, pasos_salida, red = griwrm),
         x = max(pasos) - pasos) |>
  arrange(x, id) |>
  group_by(x) |>
  mutate(y = (n() + 1) / 2 - seq_len(n())) |>     # reparte los nodos en vertical
  ungroup() |>
  mutate(etiqueta = paste0("Nodo ", id, "\n", round(area), " km² acumulados\n",
                           if_else(model == "Ungauged", "sin estación (dona 5)",
                                   "GR4J + estación")))

uniones <- esquema |>
  filter(!is.na(down)) |>
  left_join(dplyr::select(esquema, down = id, xend = x, yend = y), by = "down")

g_red_esquema <- ggplot(esquema, aes(x, y)) +
  geom_segment(data = uniones, aes(xend = xend - 0.28, yend = yend),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "grey40", linewidth = 0.8) +
  geom_label(data = uniones, aes(x = (x + xend) / 2, y = (y + yend) / 2,
                                 label = paste(round(length, 1), "km")),
             size = 3.2, linewidth = 0, color = "grey30", fill = "white") +
  geom_label(aes(label = etiqueta, fill = model), size = 3.6,
             label.padding = unit(0.5, "lines"), lineheight = 0.95) +
  scale_fill_manual(values = c("RunModel_GR4J" = "#B6D7A8", "Ungauged" = "#DDE8F5"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(add = 0.45)) +
  scale_y_continuous(expand = expansion(add = 0.6)) +
  labs(title = "Esquema de la red en airGRiwrm",
       subtitle = "El caudal de cada nodo = escorrentía de su subcuenca + caudal que llega de arriba") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#073763"))
g_red_esquema


# 4. Forzantes por subcuenca: P y ETP diarias -------------------------------------

a_largo <- function(archivo, nombre) {
  read_csv(archivo, show_col_types = FALSE) |>
    pivot_longer(-Date, names_to = "id", values_to = nombre) |>
    mutate(id = as.character(parse_number(id))) |>
    rename(fecha = Date)
}

# ETP de Hargreaves-Samani (las mismas funciones del script 04)
radiacion_ext <- function(lat, fecha) {
  phi <- lat * pi / 180
  J   <- yday(fecha)
  dr  <- 1 + 0.033 * cos(2 * pi * J / 365)
  dec <- 0.409 * sin(2 * pi * J / 365 - 1.39)
  ws  <- acos(-tan(phi) * tan(dec))
  ra  <- 24 * 60 / pi * 0.0820 * dr *
    (ws * sin(phi) * sin(dec) + cos(phi) * cos(dec) * sin(ws))
  ra * 0.408
}
hargreaves <- function(tmax, tmin, lat, fecha) {
  tmedia <- (tmax + tmin) / 2
  0.0023 * radiacion_ext(lat, fecha) * (tmedia + 17.8) * sqrt(pmax(tmax - tmin, 0))
}

latitudes <- dplyr::select(centros, id, lat = y)

# La temperatura llega a 2020: el modelo trabaja 1981-2020
forzantes <- a_largo("data/pcp/Datos_pcp.csv", "pp") |>
  inner_join(a_largo("data/temp/Datos_tmax.csv", "tmax"), by = c("fecha", "id")) |>
  inner_join(a_largo("data/temp/Datos_tmin.csv", "tmin"), by = c("fecha", "id")) |>
  left_join(latitudes, by = "id") |>
  mutate(etp = hargreaves(tmax, tmin, lat, fecha))
range(forzantes$fecha)
#> "1981-01-01" "2020-12-31"

# Gráfico: ¿cuánta lluvia y ETP recibe cada subcuenca?
g_forzantes <- forzantes |>
  group_by(id, anio = year(fecha)) |>
  summarise(Precipitación = sum(pp), ETP = sum(etp), .groups = "drop") |>
  group_by(id) |>
  summarise(across(c(Precipitación, ETP), mean)) |>
  pivot_longer(-id, names_to = "variable", values_to = "mm") |>
  ggplot(aes(paste("SB", id), mm, fill = variable)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(mm)), position = position_dodge(0.7),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("ETP" = naranja, "Precipitación" = azul), name = NULL) +
  labs(title = "Forzantes de cada subcuenca (media anual 1981-2020)",
       x = NULL, y = "mm/año") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"))
g_forzantes

# airGRiwrm pide matrices: una fila por día, una columna por nodo
a_matriz <- function(datos, variable) {
  datos |>
    dplyr::select(fecha, id, all_of(variable)) |>
    pivot_wider(names_from = id, values_from = all_of(variable)) |>
    arrange(fecha) |>
    dplyr::select(all_of(griwrm$id)) |>           # mismo orden que la red
    as.matrix()
}

P   <- a_matriz(forzantes, "pp")
ETP <- a_matriz(forzantes, "etp")
fechas <- sort(unique(forzantes$fecha))
dim(P)
#> 14610     5


# 5. Caudal observado ---------------------------------------------------------------

q_obs <- tibble(fecha = fechas) |>
  left_join(read_csv("data/processed/Q_diario_Huancane.csv", show_col_types = FALSE),
            by = "fecha")

# GR4J trabaja con lámina de agua (mm/día), no con m³/s:
#   mm/día = m³/s * 86400 s / (área km² * 1e6 m²) * 1000 mm/m = m³/s * 86.4 / área
area_salida <- griwrm$area[griwrm$id == "5"]
Qobs <- cbind("5" = q_obs$q * 86.4 / area_salida)
summary(Qobs[, 1])
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#>   0.000   0.070   0.140   0.453   0.513   8.990     199


# 6. Entradas del modelo y periodos --------------------------------------------------

InputsModel <- CreateInputsModel(griwrm,
                                 DatesR  = as.POSIXct(fechas, tz = "UTC"),
                                 Precip  = P,
                                 PotEvap = ETP)

# Calentamiento (warm-up): 1981-1983 -> llena los reservorios del modelo
# Calibración: 1984-2004       Validación: 2005-2019 (2020 tiene 119 días sin Q)
i_warm <- which(fechas >= as.Date("1981-01-01") & fechas <= as.Date("1983-12-31"))
i_sim  <- which(fechas >= as.Date("1984-01-01") & fechas <= as.Date("2019-12-31"))
i_cal  <- which(fechas >= as.Date("1984-01-01") & fechas <= as.Date("2004-12-31"))
i_val  <- which(fechas >= as.Date("2005-01-01") & fechas <= as.Date("2019-12-31"))


# 7. Calibración ----------------------------------------------------------------------

RunOptions_cal <- CreateRunOptions(InputsModel,
                                   IndPeriod_WarmUp = i_warm,
                                   IndPeriod_Run    = i_cal)

# Función objetivo: KGE (Kling-Gupta) sobre el caudal de la salida
InputsCrit <- CreateInputsCrit(InputsModel,
                               FUN_CRIT   = ErrorCrit_KGE2,
                               RunOptions = RunOptions_cal,
                               Obs        = Qobs[i_cal, , drop = FALSE])

CalibOptions <- CreateCalibOptions(InputsModel)

# Tarda ~2 minutos: búsqueda en grilla + descenso por gradiente (Michel, 1991)
OutputsCalib <- Calibration(InputsModel, RunOptions_cal, InputsCrit, CalibOptions)
#>   Calibration completed (450 iterations, 4957 runs)
#>        Param =   19.990,   47.587,    0.322,  203.548,    1.641
#>        Crit. KGE2[Q]      = 0.8563

Param <- extractParam(OutputsCalib)
Param[["5"]]
#> 19.990  47.587  0.322  203.548  1.641

# Parámetros de la subcuenca 5 (la calibrada):
#   C   velocidad de tránsito por el río (m/s) -- solo en nodos con río arriba
#       (19.99 m/s es el límite superior: el tránsito es casi instantáneo,
#        con tramos de ~30 km el retraso es menor a un día)
#   X1  capacidad del reservorio de producción (mm)
#   X2  intercambio con acuíferos vecinos (mm/día)
#   X3  capacidad del reservorio de tránsito (mm)
#   X4  tiempo base del hidrograma unitario (días)
param_tabla <- map(Param, \(p) tail(p, 4)) |>
  as_tibble() |>
  mutate(parametro = c("X1 (mm)", "X2 (mm/d)", "X3 (mm)", "X4 (d)"), .before = 1) |>
  select(parametro, all_of(as.character(1:5)))
param_tabla
#>   parametro      1      2      3      4      5
#>   X1 (mm)    47.59  47.59  47.59  47.59  47.59
#>   X2 (mm/d)   0.32   0.32   0.32   0.32   0.32
#>   X3 (mm)   203.5  203.5  203.5  203.5  203.5
#>   X4 (d)      2.14   2.18   1.43   2.28   1.64
# X1, X2 y X3 son iguales en todas: solo X4 cambia con el área (Lobligeois, 2014)


# 8. Simulación 1984-2019 y desempeño ---------------------------------------------------

RunOptions_sim <- CreateRunOptions(InputsModel,
                                   IndPeriod_WarmUp = i_warm,
                                   IndPeriod_Run    = i_sim)
OutputsModel <- RunModel(InputsModel, RunOptions_sim, Param)

# Caudal simulado en m³/s en TODOS los nodos
q_sim <- attr(OutputsModel, "Qm3s") |>
  as_tibble() |>
  mutate(fecha = as.Date(DatesR), .before = 1) |>
  select(fecha, all_of(as.character(1:5)))
q_sim
#>   fecha          1      2      3      4      5
#>   1984-01-01  1.26   2.58   4.76  0.675   5.96
#>   1984-01-02  1.97   4.14   7.02  0.732   8.36

resultado <- q_sim |>
  select(fecha, sim = `5`) |>
  left_join(select(q_obs, fecha, obs = q), by = "fecha") |>
  mutate(periodo = if_else(fecha <= as.Date("2004-12-31"),
                           "Calibración 1984-2004", "Validación 2005-2019"))

desempeno <- resultado |>
  group_by(periodo) |>
  summarise(KGE   = KGE(sim, obs),
            NSE   = NSE(sim, obs),
            r     = cor(sim, obs, use = "complete.obs"),
            PBIAS = pbias(sim, obs),
            Q_obs = mean(obs, na.rm = TRUE),
            Q_sim = mean(sim[!is.na(obs)]))
desempeno
#>   periodo                 KGE   NSE     r PBIAS Q_obs Q_sim
#>   Calibración 1984-2004  0.856 0.711 0.856   0.3  20.2  20.2
#>   Validación 2005-2019   0.813 0.756 0.870  -4.1  16.2  15.6


# 9. Gráficos de resultados ----------------------------------------------------------

# 9.1 Hidrograma mensual (promedio de los caudales diarios)
mensual <- resultado |>
  group_by(periodo, fecha = floor_date(fecha, "month")) |>
  summarise(Observado = mean(obs, na.rm = TRUE),
            Simulado  = mean(sim), .groups = "drop") |>
  pivot_longer(c(Observado, Simulado), names_to = "serie", values_to = "q")

g_hidrograma <- ggplot(mensual, aes(fecha, q, color = serie)) +
  annotate("rect", xmin = as.Date("2005-01-01"), xmax = as.Date("2019-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("text", x = as.Date(c("1994-07-01", "2012-07-01")), y = Inf,
           label = c("Calibración", "Validación"), vjust = 1.5, fontface = "bold",
           color = "grey40") +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c(Observado = "black", Simulado = naranja), name = NULL) +
  labs(title = "Caudal medio mensual en la estación Huancané",
       x = NULL, y = "Q (m³/s)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"))
g_hidrograma

# 9.2 Zoom diario a un año hidrológico (setiembre a agosto)
g_zoom <- resultado |>
  filter(fecha >= as.Date("2011-09-01"), fecha <= as.Date("2012-08-31")) |>
  ggplot(aes(fecha)) +
  geom_line(aes(y = obs, color = "Observado"), linewidth = 0.6) +
  geom_line(aes(y = sim, color = "Simulado"), linewidth = 0.8) +
  scale_color_manual(values = c(Observado = "black", Simulado = naranja), name = NULL) +
  labs(title = "Caudal diario, año hidrológico 2011-2012 (validación)",
       x = NULL, y = "Q (m³/s)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"))
g_zoom

# 9.3 Régimen mensual observado y simulado
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

g_regimen <- mensual |>
  group_by(periodo, serie, mes = month(fecha)) |>
  summarise(q = mean(q, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(mes, q, color = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ periodo) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  scale_color_manual(values = c(Observado = "black", Simulado = naranja), name = NULL) +
  labs(title = "Régimen mensual de caudales", x = NULL, y = "Q (m³/s)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"))
g_regimen

# 9.4 Cómo se arma el caudal a lo largo de la red
regimen_nodos <- q_sim |>
  pivot_longer(-fecha, names_to = "nodo", values_to = "q") |>
  group_by(nodo, mes = month(fecha)) |>
  summarise(q = mean(q), .groups = "drop") |>
  mutate(nodo = factor(paste("Nodo", nodo), levels = paste("Nodo", 1:5)))

g_nodos <- ggplot(regimen_nodos, aes(mes, q, color = nodo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  scale_color_manual(values = c("#9FC5E8", "#6FA8DC", "#3D85C6", "#93C47D", azul),
                     name = NULL) +
  labs(title = "Caudal simulado en cada nodo de la red (1984-2019)",
       subtitle = "Nodo 3 = SB 1 + SB 2 + SB 3  ·  Nodo 5 = Nodo 3 + SB 4 + SB 5",
       x = NULL, y = "Q (m³/s)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"))
g_nodos

# 9.5 Curva de duración de caudales (validación)
# Los caudales altos y medios se reproducen bien; en estiaje (>50 % del tiempo)
# el modelo SOBRESTIMA: simula ~2.5 m³/s cuando se observan ~1.5 m³/s.
# El KGE da más peso a las crecidas (ver ejercicio E2).
g_duracion <- resultado |>
  filter(periodo == "Validación 2005-2019", obs > 0) |>    # sin NA ni ceros (escala log)
  pivot_longer(c(obs, sim), names_to = "serie", values_to = "q") |>
  mutate(serie = if_else(serie == "obs", "Observado", "Simulado")) |>
  group_by(serie) |>
  mutate(excedencia = 100 * rank(-q, ties.method = "first") / n()) |>
  ggplot(aes(excedencia, q, color = serie)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  scale_color_manual(values = c(Observado = "black", Simulado = naranja), name = NULL) +
  labs(title = "Curva de duración de caudales (validación 2005-2019)",
       x = "% del tiempo que se iguala o supera", y = "Q (m³/s, escala log)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"))
g_duracion


# 10. Exportar --------------------------------------------------------------------

# La red, los parámetros y las funciones de forzantes se reutilizan en el
# Modelamiento II (simulación futura con SSP2-4.5)
saveRDS(list(griwrm = griwrm, Param = Param), "outputs/modelo_GR4J_Huancane.rds")
write_csv(q_sim,      "outputs/tablas/Q_simulado_nodos_1984_2019.csv")
write_csv(desempeno,  "outputs/tablas/Desempeno_GR4J.csv")

ggsave("outputs/figuras/GR4J_red_mapa.png",     g_red_mapa,    width = 8,  height = 7,   dpi = 150)
ggsave("outputs/figuras/GR4J_red_esquema.png",  g_red_esquema, width = 10, height = 5,   dpi = 150)
ggsave("outputs/figuras/GR4J_forzantes.png",    g_forzantes,   width = 9,  height = 5,   dpi = 150)
ggsave("outputs/figuras/GR4J_hidrograma.png",   g_hidrograma,  width = 11, height = 4.5, dpi = 150)
ggsave("outputs/figuras/GR4J_zoom_diario.png",  g_zoom,        width = 10, height = 4.5, dpi = 150)
ggsave("outputs/figuras/GR4J_regimen.png",      g_regimen,     width = 10, height = 4.5, dpi = 150)
ggsave("outputs/figuras/GR4J_nodos.png",        g_nodos,       width = 9,  height = 5,   dpi = 150)
ggsave("outputs/figuras/GR4J_duracion.png",     g_duracion,    width = 8,  height = 5,   dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Calibra con NSE en lugar de KGE (FUN_CRIT = ErrorCrit_NSE). ¿Cambian los
#     parámetros? ¿Qué periodo simula mejor los caudales bajos?

# E2. Calibra con el KGE del caudal transformado, que da más peso a los
#     estiajes: CreateInputsCrit(..., transfo = "sqrt"). Compara la curva de
#     duración de la validación.

# E3. Intercambia los periodos: calibra en 2000-2019 y valida en 1984-1999.
#     ¿El modelo es estable en el tiempo?

# E4. Construye un modelo AGREGADO (una sola cuenca de 3542 km²) con la
#     precipitación y ETP promedio ponderado. ¿Mejora o empeora el KGE en la
#     salida? ¿Qué información pierdes respecto al semidistribuido?
