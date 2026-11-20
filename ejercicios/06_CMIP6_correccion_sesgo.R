# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 06 - Datos CMIP6 y corrección de sesgo
# =============================================================================
#
# Proyecciones NEX-GDDP-CMIP6 (SSP2-4.5) de 3 modelos por subcuenca, descargadas
# de Google Earth Engine con ejercicios/Descarga_CMIP6_GEE.js. Se comparan con
# PISCO, se corrige el sesgo con dos métodos (delta y mapeo de cuantiles) y se
# calculan las señales de cambio para el futuro 2030-2050.
#
#   Periodo de referencia (histórico): 1981-2014     Futuro: 2030-2050
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Paquetes y periodos -----------------------------------------------------------

# qmap carga el paquete MASS, que tiene su propia select(). Si tidyverse ya
# estaba cargado en la sesión (por ejemplo, del script 05), la select() de MASS
# queda por encima y aparece el error "unused argument (-modelo)".
library(qmap)         # mapeo de cuantiles
library(tidyverse)
library(terra)        # áreas de las subcuencas
select <- dplyr::select   # asegura la select() de dplyr, sin importar el orden

ref <- 1981:2014      # referencia: todo el histórico CMIP6 que cubre PISCO
fut <- 2030:2050      # futuro (los datos CMIP6 llegan a 2050)

colores <- c("PISCO" = "black", "ACCESS-CM2" = "#0B5394",
             "MIROC6" = "#8E7CC3", "MPI-ESM1-2-HR" = "#6AA84F")
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
tema <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763"),
        strip.text = element_text(face = "bold", hjust = 0))


# 2. Leer los datos CMIP6 --------------------------------------------------------

# Un archivo por modelo: map() lee todos y list_rbind() los une
archivos <- list.files("data/cmip6", pattern = "ssp245\\.csv$", full.names = TRUE)
basename(archivos)
#> "Huancane_ACCESS-CM2_ssp245.csv"    "Huancane_MIROC6_ssp245.csv"
#> "Huancane_MPI-ESM1-2-HR_ssp245.csv"

cmip <- archivos |>
  map(\(f) read_csv(f, show_col_types = FALSE)) |>
  list_rbind() |>
  transmute(fecha = Fecha, subcuenca = Subcuenca, modelo = Modelo,
            pp = Pr_mm, tmax = Tmax, tmin = Tmin)
cmip
#>   fecha      subcuenca modelo        pp  tmax  tmin
#>   1981-01-01         1 ACCESS-CM2  9.06  14.7  1.76
#>   1981-01-01         3 ACCESS-CM2  8.85  16.8  4.05
#>   ... 383,505 filas (3 modelos x 5 subcuencas x 25567 días)

count(cmip, modelo)
#>   ACCESS-CM2    127835
#>   MIROC6        127835
#>   MPI-ESM1-2-HR 127835


# 3. Datos observados (PISCO) por subcuenca ---------------------------------------

a_largo <- function(archivo, nombre) {
  read_csv(archivo, show_col_types = FALSE) |>
    pivot_longer(-Date, names_to = "subcuenca", values_to = nombre) |>
    mutate(subcuenca = parse_number(subcuenca)) |>
    rename(fecha = Date)
}

obs <- a_largo("data/pcp/Datos_pcp.csv", "pp") |>
  inner_join(a_largo("data/temp/Datos_tmax.csv", "tmax"), by = c("fecha", "subcuenca")) |>
  inner_join(a_largo("data/temp/Datos_tmin.csv", "tmin"), by = c("fecha", "subcuenca")) |>
  mutate(modelo = "PISCO")
range(obs$fecha)
#> "1981-01-01" "2020-12-31"

# Áreas para pasar de subcuenca a cuenca (promedio ponderado)
subcuencas <- vect("data/basin/Subbasins.shp")
areas <- tibble(subcuenca = subcuencas$Subbasin,
                area_km2  = expanse(subcuencas, unit = "km"))


# 4. ¿Cuánto sesgo tienen los modelos? -------------------------------------------------

# Régimen mensual de un periodo: por subcuenca o para toda la cuenca
regimen <- function(datos, anios, cuenca = TRUE) {
  r <- datos |>
    filter(year(fecha) %in% anios) |>
    group_by(modelo, subcuenca, anio = year(fecha), mes = month(fecha)) |>
    summarise(pp = sum(pp), tmax = mean(tmax), tmin = mean(tmin), .groups = "drop") |>
    group_by(modelo, subcuenca, mes) |>
    summarise(across(c(pp, tmax, tmin), mean), .groups = "drop")
  if (!cuenca) return(r)
  r |>
    left_join(areas, by = "subcuenca") |>
    group_by(modelo, mes) |>
    summarise(across(c(pp, tmax, tmin), \(x) weighted.mean(x, area_km2)),
              .groups = "drop")
}

reg_crudo <- bind_rows(regimen(obs, ref), regimen(cmip, ref))

# Sesgo anual de la cuenca (modelo - PISCO), 1981-2014
sesgo <- reg_crudo |>
  group_by(modelo) |>
  summarise(pp = sum(pp), tmax = mean(tmax), tmin = mean(tmin)) |>
  mutate(sesgo_pp_pct = 100 * (pp / pp[modelo == "PISCO"] - 1),
         sesgo_tmax   = tmax - tmax[modelo == "PISCO"],
         sesgo_tmin   = tmin - tmin[modelo == "PISCO"])
sesgo
#>   modelo           pp  tmax  tmin sesgo_pp_pct sesgo_tmax sesgo_tmin
#>   ACCESS-CM2      755  16.0  1.71         4.1       2.20       2.90
#>   MIROC6          755  15.9  1.54         4.2       2.13       2.74
#>   MPI-ESM1-2-HR   761  15.9  1.77         5.0       2.11       2.97
#>   PISCO           725  13.8 -1.19         0         0          0

grafico_regimen <- function(datos, titulo) {
  datos |>
    pivot_longer(c(pp, tmax, tmin), names_to = "variable", values_to = "valor") |>
    mutate(variable = recode(variable, pp = "Precipitación (mm/mes)",
                             tmax = "Tmax (°C)", tmin = "Tmin (°C)")) |>
    ggplot(aes(mes, valor, color = modelo, linewidth = modelo == "PISCO")) +
    geom_line() +
    facet_wrap(~ variable, scales = "free_y") +
    scale_x_continuous(breaks = 1:12, labels = substr(meses, 1, 1)) +
    scale_color_manual(values = colores, name = NULL) +
    scale_linewidth_manual(values = c(0.8, 1.6), guide = "none") +
    labs(title = titulo, x = NULL, y = NULL) +
    tema
}

g_sesgo <- grafico_regimen(reg_crudo, "Modelos SIN corregir frente a PISCO (1981-2014)")
g_sesgo


# 5. Método delta (factor de cambio) ----------------------------------------------------

# Idea: el modelo solo aporta el CAMBIO entre sus propios periodos;
# la serie futura se construye perturbando lo observado.
#   Precipitación:  P_fut = P_obs * (P_mod_fut / P_mod_ref)     (razón)
#   Temperatura:    T_fut = T_obs + (T_mod_fut - T_mod_ref)     (diferencia)
deltas <- inner_join(regimen(cmip, ref, cuenca = FALSE),
                     regimen(cmip, fut, cuenca = FALSE),
                     by = c("modelo", "subcuenca", "mes"),
                     suffix = c("_ref", "_fut")) |>
  transmute(modelo, subcuenca, mes,
            f_pp   = pp_fut / pp_ref,
            d_tmax = tmax_fut - tmax_ref,
            d_tmin = tmin_fut - tmin_ref)
deltas |> filter(subcuenca == 1, modelo == "ACCESS-CM2")
#>   mes  f_pp d_tmax d_tmin
#>     1  1.02   1.86   1.73
#>     2  1.02   1.80   1.67
#>     3  1.26   1.76   1.43
#>   ...

# Serie "futura" = observado 1981-2014 con los factores de cada mes
# (conserva la variabilidad observada; solo cambia la media mensual)
fut_delta <- obs |>
  filter(year(fecha) %in% ref) |>
  select(-modelo) |>
  mutate(mes = month(fecha)) |>
  inner_join(deltas, by = c("subcuenca", "mes"), relationship = "many-to-many") |>
  transmute(fecha, subcuenca, modelo,
            pp   = pp * f_pp,
            tmax = tmax + d_tmax,
            tmin = tmin + d_tmin)


# 6. Mapeo de cuantiles (qmap) ----------------------------------------------------------

# Idea: se corrige TODA la distribución, no solo la media. Para cada cuantil,
# el valor del modelo se reemplaza por el valor observado del mismo cuantil.

# 6.1 Un caso: subcuenca 1, ACCESS-CM2, febrero, precipitación
o <- obs  |> filter(subcuenca == 1, year(fecha) %in% ref, month(fecha) == 2) |> pull(pp)
m <- cmip |> filter(subcuenca == 1, modelo == "ACCESS-CM2", month(fecha) == 2)
m_ref <- m |> filter(year(fecha) %in% ref) |> pull(pp)

ajuste <- fitQmap(o, m_ref, method = "QUANT", wet.day = TRUE, qstep = 0.01)
m_cor  <- doQmap(m_ref, ajuste)

tibble(PISCO = o, Modelo = m_ref, Corregido = m_cor) |>
  summarise(across(everything(), list(media = mean, p95 = \(x) quantile(x, 0.95),
                                      secos = \(x) mean(x < 0.1))))
#>               media   p95  secos
#>   PISCO        4.55  12.3  0.037
#>   Modelo       4.99  11.8  0.057
#>   Corregido    4.57  12.4  0.040

g_cuantiles <- tibble(PISCO = o, `Modelo sin corregir` = m_ref, Corregido = m_cor) |>
  pivot_longer(everything(), names_to = "serie", values_to = "pp") |>
  group_by(serie) |>
  mutate(prob = rank(pp, ties.method = "first") / n()) |>
  mutate(serie = factor(serie, c("Modelo sin corregir", "PISCO", "Corregido"))) |>
  ggplot(aes(pp, prob, color = serie, linetype = serie)) +
  geom_step(linewidth = 1.1) +
  scale_linetype_manual(values = c("solid", "solid", "22"), name = NULL) +
  scale_color_manual(values = c(PISCO = "black", `Modelo sin corregir` = "#0B5394",
                                Corregido = "#FF6732"), name = NULL) +
  labs(title = "Mapeo de cuantiles: subcuenca 1, febrero, ACCESS-CM2",
       x = "Precipitación diaria (mm)", y = "Probabilidad acumulada") +
  tema
g_cuantiles

# 6.2 Todos los casos: 3 modelos x 5 subcuencas x 3 variables x 12 meses
# Se ajusta en la referencia y se aplica a TODA la serie del modelo (1981-2050)
corregir <- function(o, m, variable) {
  m_ref  <- m$valor[year(m$fecha) %in% ref]
  ajuste <- fitQmap(o$valor, m_ref, method = "QUANT",
                    wet.day = variable == "pp", qstep = 0.01)
  tibble(fecha = m$fecha, valor = doQmap(m$valor, ajuste))
}

obs_mes <- obs |>
  filter(year(fecha) %in% ref) |>
  pivot_longer(c(pp, tmax, tmin), names_to = "variable", values_to = "valor") |>
  mutate(mes = month(fecha)) |>
  select(-modelo) |>
  nest(o = c(fecha, valor))

cmip_qm <- cmip |>
  pivot_longer(c(pp, tmax, tmin), names_to = "variable", values_to = "valor") |>
  mutate(mes = month(fecha)) |>
  nest(m = c(fecha, valor)) |>
  left_join(obs_mes, by = c("subcuenca", "variable", "mes")) |>
  mutate(cor = pmap(list(o, m, variable), corregir)) |>     # ~540 ajustes
  select(modelo, subcuenca, variable, cor) |>
  unnest(cor) |>
  pivot_wider(names_from = variable, values_from = valor) |>
  arrange(modelo, subcuenca, fecha)
cmip_qm
#>   modelo     subcuenca fecha         pp  tmax  tmin
#>   ACCESS-CM2         1 1981-01-01  9.01  12.5 0.760
#>   ACCESS-CM2         1 1981-01-02  3.28  11.3 1.05

# Control: la Tmin corregida no debe superar a la Tmax corregida
cmip_qm |> filter(tmin >= tmax) |> nrow()
#> 0


# 7. ¿Funcionó la corrección? ------------------------------------------------------------

reg_qm <- bind_rows(regimen(obs, ref), regimen(cmip_qm, ref))

g_corregido <- grafico_regimen(reg_qm, "Modelos CORREGIDOS (cuantiles) frente a PISCO (1981-2014)")
g_corregido

reg_qm |>
  group_by(modelo) |>
  summarise(pp = sum(pp), tmax = mean(tmax), tmin = mean(tmin))
#>   modelo           pp  tmax  tmin
#>   ACCESS-CM2      729  13.8 -1.19
#>   MIROC6          729  13.8 -1.19
#>   MPI-ESM1-2-HR   732  13.8 -1.19
#>   PISCO           725  13.8 -1.19


# 8. Señales de cambio: futuro 2030-2050 frente a 1981-2014 --------------------------------

senal <- function(datos, metodo) {
  inner_join(regimen(datos, ref), regimen(datos, fut),
             by = c("modelo", "mes"), suffix = c("_ref", "_fut")) |>
    group_by(modelo) |>
    summarise(dP_pct  = 100 * (sum(pp_fut) / sum(pp_ref) - 1),
              dTmax   = mean(tmax_fut - tmax_ref),
              dTmin   = mean(tmin_fut - tmin_ref)) |>
    mutate(metodo = metodo, .before = 1)
}

# Para el método delta, la serie futura está fechada en 1981-2014: se compara
# directamente con el observado de ese periodo
senal_delta <- inner_join(
  regimen(fut_delta, ref),
  regimen(obs, ref) |> select(-modelo),
  by = "mes", suffix = c("_fut", "_ref")) |>
  group_by(modelo) |>
  summarise(dP_pct = 100 * (sum(pp_fut) / sum(pp_ref) - 1),
            dTmax  = mean(tmax_fut - tmax_ref),
            dTmin  = mean(tmin_fut - tmin_ref)) |>
  mutate(metodo = "Delta", .before = 1)

senales <- bind_rows(senal(cmip, "Sin corregir"),
                     senal(cmip_qm, "Cuantiles"),
                     senal_delta) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))
senales
#>   metodo       modelo        dP_pct dTmax dTmin
#>   Sin corregir ACCESS-CM2      3.82  1.81  1.62
#>   Sin corregir MIROC6          9.77  1.30  1.09
#>   Sin corregir MPI-ESM1-2-HR  11.47  1.09  1.11
#>   Cuantiles    ACCESS-CM2      4.74  1.44  1.48
#>   Cuantiles    MIROC6          8.41  0.98  1.12
#>   Cuantiles    MPI-ESM1-2-HR   9.89  0.78  0.94
#>   Delta        ACCESS-CM2      3.37  1.81  1.62
#>   Delta        MIROC6          9.62  1.30  1.09
#>   Delta        MPI-ESM1-2-HR  12.58  1.09  1.11

# Promedio del ensamble (cuantiles)
senales |>
  filter(metodo == "Cuantiles") |>
  summarise(across(c(dP_pct, dTmax, dTmin), mean))
#>   dP_pct dTmax dTmin
#>     7.68  1.07  1.18

# Señal mensual (cuantiles) por modelo
senal_mes <- inner_join(regimen(cmip_qm, ref), regimen(cmip_qm, fut),
                        by = c("modelo", "mes"), suffix = c("_ref", "_fut")) |>
  transmute(modelo, mes,
            `Precipitación (%)` = 100 * (pp_fut / pp_ref - 1),
            `Tmax (°C)` = tmax_fut - tmax_ref,
            `Tmin (°C)` = tmin_fut - tmin_ref) |>
  pivot_longer(-c(modelo, mes), names_to = "variable", values_to = "cambio")

g_senal <- ggplot(senal_mes, aes(mes, cambio, fill = modelo)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  scale_fill_manual(values = colores, name = NULL) +
  labs(title = "Señal de cambio 2030-2050 frente a 1981-2014 (SSP2-4.5, cuantiles)",
       x = NULL, y = NULL) +
  tema
g_senal

# Serie anual de la cuenca: PISCO y modelos corregidos
anual <- bind_rows(obs, cmip_qm) |>
  left_join(areas, by = "subcuenca") |>
  group_by(modelo, fecha) |>
  summarise(pp = weighted.mean(pp, area_km2), tmax = weighted.mean(tmax, area_km2),
            .groups = "drop") |>
  group_by(modelo, anio = year(fecha)) |>
  summarise(`Precipitación anual (mm)` = sum(pp),
            `Tmax media (°C)` = mean(tmax), .groups = "drop") |>
  pivot_longer(-c(modelo, anio), names_to = "variable", values_to = "valor")

g_anual <- ggplot(anual, aes(anio, valor, color = modelo)) +
  annotate("rect", xmin = 2030, xmax = 2050, ymin = -Inf, ymax = Inf, fill = "grey93") +
  geom_line(aes(linewidth = modelo == "PISCO")) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_color_manual(values = colores, name = NULL) +
  scale_linewidth_manual(values = c(0.6, 1.1), guide = "none") +
  labs(title = "Cuenca Huancané: PISCO y modelos corregidos (SSP2-4.5)",
       subtitle = "Fondo gris: futuro 2030-2050", x = NULL, y = NULL) +
  tema
g_anual


# 9. Exportar --------------------------------------------------------------------------

# Forzantes corregidos por subcuenca: entrada del Modelamiento II (script 08)
write_csv(cmip_qm,   "data/processed/CMIP6_QM_subcuencas.csv")
write_csv(fut_delta, "data/processed/CMIP6_delta_subcuencas.csv")
write_csv(deltas,    "outputs/tablas/Factores_delta.csv")
write_csv(senales,   "outputs/tablas/Senales_cambio.csv")

ggsave("outputs/figuras/CMIP6_sesgo.png",      g_sesgo,     width = 11, height = 4.5, dpi = 150)
ggsave("outputs/figuras/CMIP6_cuantiles.png",  g_cuantiles, width = 8,  height = 5,   dpi = 150)
ggsave("outputs/figuras/CMIP6_corregido.png",  g_corregido, width = 11, height = 4.5, dpi = 150)
ggsave("outputs/figuras/CMIP6_senal.png",      g_senal,     width = 10, height = 7,   dpi = 150)
ggsave("outputs/figuras/CMIP6_anual.png",      g_anual,     width = 10, height = 6,   dpi = 150)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. Repite el caso de la sección 6.1 para julio (época seca). ¿Qué pasa con
#     los días secos del modelo después de la corrección?

# E2. Cambia method = "QUANT" por method = "RQUANT" (cuantiles robustos) o
#     "PTF" (función paramétrica). ¿Cambia la señal de precipitación?

# E3. Compara la señal de cambio de Tmax "Sin corregir" y "Cuantiles".
#     ¿El mapeo de cuantiles conserva la señal del modelo?

# E4. Calcula la señal para el horizonte 2041-2050 en lugar de 2030-2050.
#     ¿El cambio crece con el tiempo?
