# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 08 - Modelamiento II: simulación futura (SSP2-4.5)
# =============================================================================
#
# Se usa el GR4J semidistribuido calibrado en el script 07 con los forzantes
# CMIP6 corregidos en el script 06 (mapeo de cuantiles y método delta), y se
# compara el régimen de caudales del futuro cercano con el de referencia.
#
#   Referencia: 1985-2014        Futuro cercano: 2020-2049
#
# Requiere haber ejecutado antes los scripts 06 y 07.
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes ---------------------------------------------------------------------

library(tidyverse)
library(terra)
library(airGRiwrm)

ref <- 1985:2014
fut <- 2020:2049

colores <- c("Observado" = "black", "ACCESS-CM2" = "#0B5394",
             "MIROC6" = "#8E7CC3", "MPI-ESM1-2-HR" = "#6AA84F")
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
tema <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"),
        strip.text = element_text(face = "bold", hjust = 0))


# 2. El modelo calibrado (script 07) --------------------------------------------------

modelo <- readRDS("outputs/modelo_GR4J_Huancane.rds")
griwrm <- modelo$griwrm
Param  <- modelo$Param
Param[["5"]]
#> 19.990  47.587  0.322  203.548  1.641


# 3. Una función que simula caudales a partir de cualquier forzante ---------------------

# ETP de Hargreaves-Samani (mismas funciones de los scripts 04 y 07)
radiacion_ext <- function(lat, fecha) {
  phi <- lat * pi / 180
  J   <- yday(fecha)
  dr  <- 1 + 0.033 * cos(2 * pi * J / 365)
  dec <- 0.409 * sin(2 * pi * J / 365 - 1.39)
  ws  <- acos(-tan(phi) * tan(dec))
  24 * 60 / pi * 0.0820 * dr *
    (ws * sin(phi) * sin(dec) + cos(phi) * cos(dec) * sin(ws)) * 0.408
}
hargreaves <- function(tmax, tmin, lat, fecha) {
  0.0023 * radiacion_ext(lat, fecha) * ((tmax + tmin) / 2 + 17.8) *
    sqrt(pmax(tmax - tmin, 0))
}

subcuencas <- project(vect("data/basin/Subbasins.shp"), "EPSG:4326")
latitudes  <- tibble(subcuenca = subcuencas$Subbasin,
                     lat = crds(centroids(subcuencas))[, "y"])

# forzantes: tabla con fecha, subcuenca, pp, tmax, tmin (diaria y continua)
# Devuelve el caudal diario simulado (m³/s) en la salida (nodo 5) o, con
# nodos = griwrm$id, en todos los nodos de la red
simular <- function(forzantes, anios_warmup = 3, nodos = "5") {
  f <- forzantes |>
    left_join(latitudes, by = "subcuenca") |>
    mutate(etp = hargreaves(tmax, tmin, lat, fecha),
           subcuenca = as.character(subcuenca))
  a_matriz <- function(v) {
    f |>
      select(fecha, subcuenca, all_of(v)) |>
      pivot_wider(names_from = subcuenca, values_from = all_of(v)) |>
      arrange(fecha) |>
      select(all_of(griwrm$id)) |>
      as.matrix()
  }
  fechas <- sort(unique(f$fecha))

  InputsModel <- suppressMessages(
    CreateInputsModel(griwrm, DatesR = as.POSIXct(fechas, tz = "UTC"),
                      Precip = a_matriz("pp"), PotEvap = a_matriz("etp")))
  n_warm <- 365 * anios_warmup
  RunOptions <- suppressMessages(
    CreateRunOptions(InputsModel,
                     IndPeriod_WarmUp = 1:n_warm,
                     IndPeriod_Run    = (n_warm + 1):length(fechas)))
  salida <- suppressMessages(RunModel(InputsModel, RunOptions, Param))

  # Qm3s trae una columna por nodo: nos quedamos con los pedidos
  q <- as_tibble(attr(salida, "Qm3s")[nodos]) |>
    mutate(fecha = fechas[(n_warm + 1):length(fechas)], .before = 1)
  if (length(nodos) == 1) return(rename(q, q = all_of(nodos)))
  pivot_longer(q, -fecha, names_to = "nodo", values_to = "q")
}


# 4. Simulación con los forzantes corregidos por cuantiles -------------------------------

cmip_qm <- read_csv("data/processed/CMIP6_QM_subcuencas.csv", show_col_types = FALSE)

# Un modelo climático = una corrida de 1981 a 2049 (calentamiento 1981-1983)
q_qm <- cmip_qm |>
  nest(forzantes = -modelo) |>
  mutate(q = map(forzantes, simular)) |>
  select(modelo, q) |>
  unnest(q) |>
  mutate(metodo = "Cuantiles")
q_qm
#>   modelo     fecha          q metodo
#>   ACCESS-CM2 1984-01-01  20.8 Cuantiles
#>   ACCESS-CM2 1984-01-02  19.0 Cuantiles
#>   ... 72,321 filas (3 modelos x 1984-2049)


# 4.1 ¿Cómo cambian la lluvia y la ETP en cada subcuenca? ---------------------------------

# Totales anuales medios de referencia y futuro, por modelo y subcuenca
cambio_sb <- cmip_qm |>
  left_join(latitudes, by = "subcuenca") |>
  mutate(etp = hargreaves(tmax, tmin, lat, fecha),
         periodo = case_when(year(fecha) %in% ref ~ "ref",
                             year(fecha) %in% fut ~ "fut")) |>
  filter(!is.na(periodo)) |>
  group_by(modelo, subcuenca, periodo) |>
  summarise(pp  = sum(pp)  / n_distinct(year(fecha)),     # mm/año
            etp = sum(etp) / n_distinct(year(fecha)), .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = c(pp, etp)) |>
  transmute(modelo, subcuenca,
            pp  = 100 * (pp_fut  / pp_ref  - 1),             # cambio en %
            etp = 100 * (etp_fut / etp_ref - 1))

# El ensamble es el promedio de los 3 modelos
cambio_sb <- bind_rows(
  cambio_sb,
  cambio_sb |>
    group_by(subcuenca) |>
    summarise(across(c(pp, etp), mean)) |>
    mutate(modelo = "Ensamble"))
cambio_sb |> mutate(across(c(pp, etp), \(x) round(x, 1))) |> print(n = 20)
#>   modelo        subcuenca    pp   etp
#>   ACCESS-CM2            1   4.1   5.7
#>   ACCESS-CM2            4   0.6   4.8
#>   MIROC6                3  11.2   3.1
#>   MPI-ESM1-2-HR         3  13.0   2.2
#>   Ensamble              1   6.3   4.1
#>   Ensamble              3   9.9   3.4
#>   ... (20 filas: 4 x 5 subcuencas)

# Mapa: una columna por modelo, mismo rango de colores para todos
mapa_cambio <- function(variable, titulo, bajo, alto) {
  datos <- sf::st_as_sf(subcuencas) |>
    select(subcuenca = Subbasin) |>
    inner_join(cambio_sb, by = "subcuenca") |>
    mutate(modelo = fct_relevel(modelo, "Ensamble", after = Inf),
           valor  = .data[[variable]])
  limite <- max(abs(datos$valor))

  ggplot(datos) +
    geom_sf(aes(fill = valor), color = "grey30", linewidth = 0.3) +
    geom_sf_text(aes(label = sprintf("%+.1f", valor)), size = 3.6, fontface = "bold") +
    facet_wrap(~ modelo, nrow = 1) +
    scale_fill_gradient2(low = bajo, mid = "white", high = alto,
                         limits = c(-limite, limite), name = "Cambio (%)") +
    labs(title = titulo,
         subtitle = "2020-2049 frente a 1985-2014, SSP2-4.5, forzantes corregidos por cuantiles",
         x = NULL, y = NULL) +
    tema +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          legend.position = "right")
}

g_mapa_pp  <- mapa_cambio("pp",  "Cambio de la precipitación anual por subcuenca",
                          bajo = "#8C510A", alto = "#01665E")
g_mapa_etp <- mapa_cambio("etp", "Cambio de la evapotranspiración potencial anual (Hargreaves)",
                          bajo = "#2166AC", alto = "#B2182B")
g_mapa_pp
g_mapa_etp


# 4.2 ¿Y el caudal en cada tramo de río? -------------------------------------------------

# El modelo semidistribuido da caudal en los 5 nodos: cada nodo es la salida
# de un tramo del río (Rivers.shp, campo Subbasin)
q_nodos <- cmip_qm |>
  nest(forzantes = -modelo) |>
  mutate(q = map(forzantes, \(f) simular(f, nodos = griwrm$id))) |>
  select(modelo, q) |>
  unnest(q)

cambio_tramos <- q_nodos |>
  mutate(periodo = case_when(year(fecha) %in% ref ~ "ref",
                             year(fecha) %in% fut ~ "fut")) |>
  filter(!is.na(periodo)) |>
  group_by(modelo, nodo, periodo) |>
  summarise(q = mean(q), .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = q) |>
  mutate(cambio = 100 * (fut / ref - 1))

cambio_tramos <- bind_rows(
  cambio_tramos,
  cambio_tramos |>
    group_by(nodo) |>
    summarise(across(c(ref, fut, cambio), mean)) |>
    mutate(modelo = "Ensamble"))
cambio_tramos |> mutate(across(where(is.numeric), \(x) round(x, 1))) |> print(n = 20)
#>   modelo        nodo    fut   ref cambio
#>   ACCESS-CM2    1       4.8   4.7    3.2
#>   ACCESS-CM2    2       6.4   6.7   -4.8
#>   ACCESS-CM2    5      17.4  17.5   -0.8
#>   MIROC6        4       4.2   3.4   22.7
#>   MPI-ESM1-2-HR 4       5.2   3.8   37.7
#>   MPI-ESM1-2-HR 5      22.7  17.4   30.3
#>   Ensamble      5      19.6  17.1   14.7
#>   ... (20 filas: 4 x 5 nodos)

rios <- project(vect("data/rivers/Rivers.shp"), "EPSG:4326")

limite <- max(abs(cambio_tramos$cambio))
g_mapa_q <- sf::st_as_sf(rios) |>
  transmute(nodo = as.character(Subbasin)) |>
  inner_join(cambio_tramos, by = "nodo") |>
  mutate(modelo = fct_relevel(modelo, "Ensamble", after = Inf)) |>
  ggplot() +
  geom_sf(data = sf::st_as_sf(subcuencas), fill = "grey96", color = "grey70",
          linewidth = 0.3) +
  geom_sf(aes(color = cambio, linewidth = ref), lineend = "round") +
  geom_sf_label(aes(label = if_else(abs(cambio) < 0.5, "0", sprintf("%+.0f", cambio))),
                size = 3.2,
                fontface = "bold", label.padding = unit(0.12, "lines"),
                linewidth = 0, alpha = 0.85) +
  facet_wrap(~ modelo, nrow = 1) +
  scale_color_gradient2(low = "#8C510A", mid = "grey60", high = "#01665E",
                        limits = c(-limite, limite), name = "Cambio (%)") +
  scale_linewidth(range = c(0.6, 2), name = "Q medio ref.\n(m³/s)") +
  labs(title = "Cambio del caudal medio anual por tramo de río",
       subtitle = "2020-2049 frente a 1985-2014, SSP2-4.5, GR4J semidistribuido (airGRiwrm)",
       x = NULL, y = NULL) +
  tema +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        legend.position = "right")
g_mapa_q


# 5. Simulación con el método delta ------------------------------------------------------

# Referencia = PISCO observado; futuro = PISCO perturbado con los factores delta.
# Ambas series están fechadas en 1985-2014: se antepone 1982-1984 observado
# como calentamiento.
a_largo <- function(archivo, nombre) {
  read_csv(archivo, show_col_types = FALSE) |>
    pivot_longer(-Date, names_to = "subcuenca", values_to = nombre) |>
    mutate(subcuenca = parse_number(subcuenca)) |>
    rename(fecha = Date)
}
obs <- a_largo("data/pcp/Datos_pcp.csv", "pp") |>
  inner_join(a_largo("data/temp/Datos_tmax.csv", "tmax"), by = c("fecha", "subcuenca")) |>
  inner_join(a_largo("data/temp/Datos_tmin.csv", "tmin"), by = c("fecha", "subcuenca"))

calentamiento <- obs |> filter(year(fecha) %in% 1982:1984)

fut_delta <- read_csv("data/processed/CMIP6_delta_subcuencas.csv", show_col_types = FALSE)

q_ref_obs <- obs |>
  filter(year(fecha) %in% 1982:2014) |>
  simular()

q_delta <- fut_delta |>
  nest(forzantes = -modelo) |>
  mutate(q = map(forzantes, \(f) simular(bind_rows(calentamiento, f)))) |>
  select(modelo, q) |>
  unnest(q) |>
  mutate(metodo = "Delta")


# 6. Régimen de referencia y futuro -------------------------------------------------------

# Cuantiles: referencia y futuro salen de la MISMA corrida (mismo modelo climático)
reg_qm <- q_qm |>
  mutate(periodo = case_when(year(fecha) %in% ref ~ "Referencia 1985-2014",
                             year(fecha) %in% fut ~ "Futuro 2020-2049")) |>
  filter(!is.na(periodo))

# Delta: referencia = simulación con PISCO; futuro = simulación perturbada
reg_delta <- bind_rows(
  q_delta |> mutate(periodo = "Futuro 2020-2049"),
  map(unique(q_delta$modelo), \(m) mutate(q_ref_obs, modelo = m,
                                          metodo = "Delta",
                                          periodo = "Referencia 1985-2014")) |>
    list_rbind()) |>
  filter(year(fecha) %in% ref)

regimen <- bind_rows(reg_qm, reg_delta) |>
  group_by(metodo, modelo, periodo, mes = month(fecha)) |>
  summarise(q = mean(q), .groups = "drop")

# Caudal observado 1985-2014 (estación Huancané), como referencia visual
q_obs <- read_csv("data/processed/Q_diario_Huancane.csv", show_col_types = FALSE) |>
  filter(year(fecha) %in% ref) |>
  group_by(mes = month(fecha)) |>
  summarise(q = mean(q, na.rm = TRUE))

g_regimen <- regimen |>
  mutate(periodo = fct_rev(periodo)) |>
  ggplot(aes(mes, q)) +
  geom_line(data = q_obs, aes(linetype = "Q observado"), color = "grey40", linewidth = 0.8) +
  geom_line(aes(color = modelo, alpha = periodo), linewidth = 1) +
  facet_grid(metodo ~ modelo) +
  scale_x_continuous(breaks = c(1, 4, 7, 10), labels = meses[c(1, 4, 7, 10)]) +
  scale_color_manual(values = colores, guide = "none") +
  scale_alpha_manual(values = c(0.35, 1), name = NULL) +
  scale_linetype_manual(values = "dashed", name = NULL) +
  labs(title = "Régimen de caudales en la estación Huancané",
       subtitle = "Línea clara: referencia 1985-2014  ·  línea intensa: futuro 2020-2049 (SSP2-4.5)",
       x = NULL, y = "Q (m³/s)") +
  tema
g_regimen


# 7. ¿Cuánto cambia el caudal? -------------------------------------------------------------

cambio_mes <- regimen |>
  pivot_wider(names_from = periodo, values_from = q) |>
  mutate(cambio_pct = 100 * (`Futuro 2020-2049` / `Referencia 1985-2014` - 1))

# Cambio del caudal medio anual
cambio_anual <- regimen |>
  group_by(metodo, modelo, periodo) |>
  summarise(q = mean(q), .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = q) |>
  mutate(cambio_pct = 100 * (`Futuro 2020-2049` / `Referencia 1985-2014` - 1)) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))
cambio_anual
#>   metodo    modelo        Futuro 2020-2049 Referencia 1985-2014 cambio_pct
#>   Cuantiles ACCESS-CM2                17.6                 17.8       -1.1
#>   Cuantiles MIROC6                    19.0                 16.6       14.5
#>   Cuantiles MPI-ESM1-2-HR             22.9                 17.6       30.1
#>   Delta     ACCESS-CM2                17.4                 18.5       -6.1
#>   Delta     MIROC6                    21.3                 18.5       15.3
#>   Delta     MPI-ESM1-2-HR             24.2                 18.5       31.1

# Cambio por época: lluvias (dic-mar) y estiaje (jun-set)
cambio_epoca <- bind_rows(reg_qm, reg_delta) |>
  mutate(epoca = case_when(month(fecha) %in% c(12, 1:3) ~ "Lluvias (dic-mar)",
                           month(fecha) %in% 6:9 ~ "Estiaje (jun-set)")) |>
  filter(!is.na(epoca)) |>
  group_by(metodo, modelo, epoca, periodo) |>
  summarise(q = mean(q), .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = q) |>
  mutate(cambio_pct = round(100 * (`Futuro 2020-2049` / `Referencia 1985-2014` - 1), 1)) |>
  select(metodo, modelo, epoca, cambio_pct) |>
  pivot_wider(names_from = epoca, values_from = cambio_pct)
cambio_epoca
#>   metodo    modelo        Estiaje (jun-set) Lluvias (dic-mar)
#>   Cuantiles ACCESS-CM2                  3.8              -4.1
#>   Cuantiles MIROC6                      3.9              15.8
#>   Cuantiles MPI-ESM1-2-HR              -3.1              41.2
#>   Delta     ACCESS-CM2                 -0.5              -9.2
#>   Delta     MIROC6                      6.3              15.5
#>   Delta     MPI-ESM1-2-HR              -4.2              41.6

g_cambio <- ggplot(cambio_mes, aes(mes, cambio_pct, fill = modelo)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ metodo, ncol = 1) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  scale_fill_manual(values = colores, name = NULL) +
  labs(title = "Cambio del caudal medio mensual: 2020-2049 frente a 1985-2014",
       x = NULL, y = "Cambio (%)") +
  tema
g_cambio


# 8. Curva de duración: referencia y futuro (cuantiles) ---------------------------------

duracion <- reg_qm |>
  group_by(modelo, periodo) |>
  mutate(excedencia = 100 * rank(-q, ties.method = "first") / n()) |>
  ungroup()

# Caudales característicos: Q10 (altos), Q50 (medianos), Q90 (bajos)
caudales_tipicos <- reg_qm |>
  group_by(modelo, periodo) |>
  summarise(Q10 = quantile(q, 0.90), Q50 = quantile(q, 0.50), Q90 = quantile(q, 0.10),
            .groups = "drop") |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))
caudales_tipicos
#>   modelo        periodo                Q10   Q50   Q90
#>   ACCESS-CM2    Futuro 2020-2049     47.4  6.23  2.69
#>   ACCESS-CM2    Referencia 1985-2014 46.0  6.19  2.59
#>   MIROC6        Futuro 2020-2049     52.3  6.73  2.79
#>   MIROC6        Referencia 1985-2014 46.2  6.42  2.65
#>   MPI-ESM1-2-HR Futuro 2020-2049     66.8  6.94  2.63
#>   MPI-ESM1-2-HR Referencia 1985-2014 48.0  5.98  2.59

g_duracion <- duracion |>
  mutate(periodo = fct_rev(periodo)) |>
  ggplot(aes(excedencia, q, color = modelo, linetype = periodo)) +
  geom_line(linewidth = 0.9) +
  scale_y_log10() +
  scale_color_manual(values = colores, name = NULL) +
  scale_linetype_manual(values = c("dashed", "solid"), name = NULL) +
  labs(title = "Curva de duración de caudales (cuantiles)",
       x = "% del tiempo que se iguala o supera", y = "Q (m³/s, escala log)") +
  tema +
  theme(legend.box = "vertical")
g_duracion


# 9. Exportar --------------------------------------------------------------------------------

write_csv(bind_rows(q_qm, q_delta), "outputs/tablas/Q_futuro_SSP245.csv")
write_csv(cambio_anual, "outputs/tablas/Cambio_caudal_anual.csv")
write_csv(cambio_epoca, "outputs/tablas/Cambio_caudal_epocas.csv")

ggsave("outputs/figuras/Futuro_regimen.png",  g_regimen,  width = 11, height = 6,   dpi = 150)
ggsave("outputs/figuras/Futuro_cambio.png",   g_cambio,   width = 10, height = 6,   dpi = 150)
ggsave("outputs/figuras/Futuro_duracion.png", g_duracion, width = 9,  height = 5.5, dpi = 150)
ggsave("outputs/figuras/Futuro_mapa_pp.png",  g_mapa_pp,  width = 12, height = 4.2, dpi = 150)
ggsave("outputs/figuras/Futuro_mapa_etp.png", g_mapa_etp, width = 12, height = 4.2, dpi = 150)
write_csv(cambio_sb, "outputs/tablas/Cambio_P_ETP_subcuencas.csv")
write_csv(cambio_tramos, "outputs/tablas/Cambio_caudal_tramos.csv")
ggsave("outputs/figuras/Futuro_mapa_q.png", g_mapa_q, width = 12, height = 4.2, dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Calcula el promedio del ensamble (los 3 modelos) del cambio mensual.
#     ¿En qué meses los 3 modelos coinciden en el signo del cambio?

# E2. Repite la simulación sin corregir el sesgo (forzantes CMIP6 crudos,
#     data/cmip6). ¿Cuánto se equivoca el caudal de referencia?

# E3. ¿Qué pesa más en el cambio de caudal: la lluvia o la temperatura?
#     Simula el futuro cambiando solo la temperatura (pp de la referencia).

# E4. Calcula el cambio del caudal en el nodo 3 (subcuencas 1, 2 y 3).
#     Pista: en simular(), devuelve attr(salida, "Qm3s")[["3"]].
