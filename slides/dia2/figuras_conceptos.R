# =============================================================================
# Figuras conceptuales del Día 2, bloque 02 (Escenarios futuros: conceptos)
# Genera assets/img/s08_*.png. Ejecutar desde la raíz del proyecto.
# =============================================================================

library(tidyverse)

azul    <- "#0B5394"
naranja <- "#FF6732"
tema <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#073763"))


# 1. Calentamiento global por escenario (IPCC AR6 WG1, SPM, Tabla SPM.1) -------
# Mejor estimación del cambio de temperatura global respecto a 1850-1900 (°C)
ar6 <- tribble(
  ~ssp,       ~h2021_2040, ~h2041_2060, ~h2081_2100, ~min2100, ~max2100,
  "SSP1-1.9",  1.5,         1.4,         1.4,         1.0,      1.8,
  "SSP1-2.6",  1.5,         1.7,         1.8,         1.3,      2.4,
  "SSP2-4.5",  1.5,         2.0,         2.7,         2.1,      3.5,
  "SSP3-7.0",  1.5,         2.1,         3.6,         2.8,      4.6,
  "SSP5-8.5",  1.6,         2.4,         4.4,         3.3,      5.7
)

g_ssp <- ar6 |>
  pivot_longer(starts_with("h"), names_to = "horizonte", values_to = "dT") |>
  mutate(horizonte = recode(horizonte, h2021_2040 = "2021-2040",
                            h2041_2060 = "2041-2060", h2081_2100 = "2081-2100"),
         curso = ssp == "SSP2-4.5") |>
  ggplot(aes(horizonte, dT, fill = curso)) +
  geom_col(width = 0.7) +
  geom_errorbar(data = \(d) filter(d, horizonte == "2081-2100"),
                aes(ymin = min2100, ymax = max2100), width = 0.25, color = "grey30") +
  geom_text(aes(label = dT, y = dT / 2), size = 3.8, color = "white", fontface = "bold") +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "grey40") +
  facet_wrap(~ ssp, nrow = 1) +
  scale_fill_manual(values = c("FALSE" = "#B7C9DD", "TRUE" = naranja), guide = "none") +
  labs(title = "Calentamiento global según escenario (respecto a 1850-1900)",
       subtitle = "Mejor estimación; barra de error = rango muy probable a fin de siglo. Línea: 1.5 °C",
       x = NULL, y = "°C") +
  tema +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))
ggsave("assets/img/s08_ssp_calentamiento.png", g_ssp, width = 11, height = 5, dpi = 150)


# 2. Línea de tiempo de los datos y periodos del curso -------------------------
periodos <- tribble(
  ~fila,                     ~inicio, ~fin, ~tipo,
  "PISCO precipitación",       1981, 2025, "Observado",
  "PISCO temperatura",         1981, 2020, "Observado",
  "CMIP6 histórico",           1981, 2014, "Modelo",
  "CMIP6 SSP2-4.5",            2015, 2050, "Modelo",
  "Periodo de referencia",     1981, 2014, "Análisis",
  "Futuro",                    2030, 2050, "Análisis"
) |>
  mutate(fila = fct_rev(fct_inorder(fila)))

g_linea <- ggplot(periodos, aes(y = fila, color = tipo)) +
  geom_segment(aes(x = inicio, xend = fin + 1, yend = fila), linewidth = 9) +
  geom_text(aes(x = (inicio + fin + 1) / 2, label = paste0(inicio, "-", fin)),
            color = "white", fontface = "bold", size = 3.8) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2015.5, y = 6.45, label = "inicio de los escenarios",
           hjust = 0, size = 3.5, color = "grey30") +
  scale_color_manual(values = c(Observado = "grey45", Modelo = azul, Análisis = naranja),
                     name = NULL) +
  scale_x_continuous(breaks = seq(1980, 2050, 10)) +
  labs(title = "Datos y periodos que usaremos", x = NULL, y = NULL) +
  tema +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
ggsave("assets/img/s08_linea_tiempo.png", g_linea, width = 10, height = 4.5, dpi = 150)


# 3. Lo que trae un GCM: serie cruda frente a lo observado ------------------------
# Promedio simple de las 5 subcuencas (para comparar con PISCO de la misma forma)
gcm <- list.files("data/cmip6", full.names = TRUE) |>
  map(\(f) read_csv(f, show_col_types = FALSE)) |>
  list_rbind() |>
  group_by(Modelo, anio = year(Fecha)) |>
  summarise(pp = sum(Pr_mm) / 5, tmax = mean(Tmax), .groups = "drop")

obs_anual <- function(archivo, nombre, fun) {
  read_csv(archivo, show_col_types = FALSE) |>
    mutate(v = rowMeans(across(-Date))) |>
    group_by(anio = year(Date)) |>
    summarise({{ nombre }} := fun(v))
}
pisco <- obs_anual("data/pcp/Datos_pcp.csv", pp, sum) |>
  full_join(obs_anual("data/temp/Datos_tmax.csv", tmax, mean), by = "anio") |>
  filter(anio <= 2025) |>
  mutate(Modelo = "PISCO (observado)")

g_gcm <- bind_rows(gcm, pisco) |>
  pivot_longer(c(pp, tmax), names_to = "variable", values_to = "valor") |>
  filter(!is.na(valor)) |>
  mutate(variable = recode(variable, pp = "Precipitación anual (mm)",
                           tmax = "Temperatura máxima media (°C)")) |>
  ggplot(aes(anio, valor, color = Modelo)) +
  annotate("rect", xmin = 2015, xmax = 2050, ymin = -Inf, ymax = Inf,
           fill = "grey93") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("ACCESS-CM2" = azul, "MPI-ESM1-2-HR" = "#6AA84F",
                                "MIROC6" = "#8E7CC3", "PISCO (observado)" = "black"),
                     name = NULL) +
  labs(title = "Serie cruda de los modelos frente a lo observado (cuenca Huancané)",
       subtitle = "Fondo gris: escenario SSP2-4.5", x = NULL, y = NULL) +
  tema +
  theme(legend.position = "top", strip.text = element_text(face = "bold", hjust = 0))
ggsave("assets/img/s08_gcm_crudo.png", g_gcm, width = 10, height = 6, dpi = 150)

# Sesgo medio 1981-2014 (modelo - observado), para el texto de la diapositiva
bind_rows(gcm, pisco) |>
  filter(anio %in% 1981:2014) |>
  group_by(Modelo) |>
  summarise(pp = mean(pp), tmax = mean(tmax)) |>
  print()
