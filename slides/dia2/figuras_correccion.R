# =============================================================================
# Figuras conceptuales del Día 2, bloque 03 (corrección de sesgo)
# Genera assets/img/s09_concepto_delta.png y s09_concepto_qm.png.
# Ejecutar desde la raíz del proyecto. Caso: subcuenca 1, ACCESS-CM2,
# el mismo que se usa en ejercicios/06_CMIP6_correccion_sesgo.R
# =============================================================================

library(qmap)          # antes que tidyverse (MASS::select)
library(tidyverse)
library(patchwork)

ref <- 1981:2014
fut <- 2030:2050
meses <- c("E", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

azul    <- "#0B5394"
naranja <- "#FF6732"
tema <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.title = element_text(face = "bold", color = "#073763", size = 13))


# Datos: subcuenca 1 -----------------------------------------------------------------
obs <- read_csv("data/pcp/Datos_pcp.csv", show_col_types = FALSE) |>
  pivot_longer(-Date, names_to = "subcuenca", values_to = "pp") |>
  mutate(subcuenca = parse_number(subcuenca)) |>
  filter(subcuenca == 1) |>
  transmute(fecha = Date, pp)

mod <- read_csv("data/cmip6/Huancane_ACCESS-CM2_ssp245.csv", show_col_types = FALSE) |>
  filter(Subcuenca == 1) |>
  transmute(fecha = Fecha, pp = Pr_mm)

regimen <- function(d, anios) {
  d |>
    filter(year(fecha) %in% anios) |>
    group_by(anio = year(fecha), mes = month(fecha)) |>
    summarise(pp = sum(pp), .groups = "drop") |>
    group_by(mes) |>
    summarise(pp = mean(pp))
}


# 1. Método delta -------------------------------------------------------------------------
delta <- regimen(mod, ref) |>
  rename(mod_ref = pp) |>
  inner_join(regimen(mod, fut) |> rename(mod_fut = pp), by = "mes") |>
  inner_join(regimen(obs, ref) |> rename(obs_ref = pp), by = "mes") |>
  mutate(f = mod_fut / mod_ref, obs_fut = obs_ref * f)
print(delta, n = 12)

g_a <- delta |>
  pivot_longer(c(mod_ref, mod_fut), names_to = "serie", values_to = "pp") |>
  mutate(serie = recode(serie, mod_ref = "Modelo 1981-2014", mod_fut = "Modelo 2030-2050")) |>
  ggplot(aes(mes, pp, color = serie, linetype = serie)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = c(azul, naranja), name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  labs(title = "1. El modelo en sus dos periodos", x = NULL, y = "mm/mes") +
  tema

g_b <- ggplot(delta, aes(mes, f, fill = f >= 1)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, color = "grey30") +
  geom_text(aes(label = ifelse(round(100 * (f - 1)) == 0, "0%",
                               sprintf("%+.0f%%", 100 * (f - 1))),
                vjust = ifelse(f >= 1, -0.4, 1.3)), size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = naranja, `FALSE` = "#9AA9BC"), guide = "none") +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  coord_cartesian(ylim = c(0, max(delta$f) * 1.12)) +
  labs(title = "2. Factor de cambio: f = futuro / referencia", x = NULL, y = "f") +
  tema

g_c <- delta |>
  pivot_longer(c(obs_ref, obs_fut), names_to = "serie", values_to = "pp") |>
  mutate(serie = recode(serie, obs_ref = "PISCO 1981-2014", obs_fut = "PISCO × f (futuro)")) |>
  ggplot(aes(mes, pp, color = serie, linetype = serie)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2) +
  scale_color_manual(values = c("black", naranja), name = NULL,
                     breaks = c("PISCO 1981-2014", "PISCO × f (futuro)")) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL,
                        breaks = c("PISCO 1981-2014", "PISCO × f (futuro)")) +
  scale_x_continuous(breaks = 1:12, labels = meses) +
  labs(title = "3. El factor se aplica a lo observado", x = NULL, y = "mm/mes") +
  tema

# Día a día: marzo de 2001, f de marzo
f_mar <- delta$f[delta$mes == 3]
dia <- obs |>
  filter(year(fecha) == 2001, month(fecha) == 3) |>
  mutate(`PISCO marzo 2001` = pp, `PISCO × f` = pp * f_mar) |>
  pivot_longer(c(`PISCO marzo 2001`, `PISCO × f`), names_to = "serie", values_to = "valor") |>
  mutate(serie = fct_inorder(serie))

g_d <- ggplot(dia, aes(day(fecha), valor, fill = serie)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_fill_manual(values = c("grey35", naranja), name = NULL) +
  labs(title = sprintf("4. Día a día: la misma secuencia × %.2f", f_mar),
       x = "Día de marzo", y = "mm/día") +
  tema

g_delta <- (g_a + g_b) / (g_c + g_d) &
  theme(text = element_text(size = 15), plot.title = element_text(size = 15))
ggsave("assets/img/s09_concepto_delta.png", g_delta, width = 12, height = 7, dpi = 150)


# 2. Mapeo de cuantiles ------------------------------------------------------------------
o     <- obs |> filter(year(fecha) %in% ref, month(fecha) == 2) |> pull(pp)
m_ref <- mod |> filter(year(fecha) %in% ref, month(fecha) == 2) |> pull(pp)
ajuste <- fitQmap(o, m_ref, method = "QUANT", wet.day = TRUE, qstep = 0.01)

curvas <- bind_rows(
  tibble(serie = "Modelo (sin corregir)", pp = m_ref),
  tibble(serie = "PISCO (observado)",     pp = o)) |>
  group_by(serie) |>
  mutate(prob = ecdf(pp)(pp)) |>
  ungroup()

# Dos ejemplos: la mediana y el percentil 75 del modelo
ejemplos <- tibble(p = c(0.5, 0.75)) |>
  mutate(x_mod = quantile(m_ref, p, names = FALSE),
         x_cor = doQmap(x_mod, ajuste))
print(ejemplos)

g_cdf <- ggplot(curvas, aes(pp, prob, color = serie)) +
  geom_step(linewidth = 1.1) +
  geom_segment(data = ejemplos, aes(x = x_mod, xend = x_mod, y = 0, yend = p),
               inherit.aes = FALSE, color = azul, linetype = "22") +
  geom_segment(data = ejemplos, aes(x = x_mod, xend = x_cor, y = p, yend = p),
               inherit.aes = FALSE, color = naranja, linewidth = 1,
               arrow = arrow(length = unit(0.18, "cm"))) +
  geom_segment(data = ejemplos, aes(x = x_cor, xend = x_cor, y = p, yend = 0),
               inherit.aes = FALSE, color = naranja, linewidth = 1,
               arrow = arrow(length = unit(0.18, "cm"))) +
  geom_text(data = ejemplos, aes(x = 10.5, y = p - 0.04,
                                 label = sprintf("P%d del modelo: %.1f mm → %.1f mm",
                                                 100 * p, x_mod, x_cor)),
            inherit.aes = FALSE, hjust = 0, size = 3.6, color = "grey20") +
  scale_color_manual(values = c(azul, "black"), name = NULL) +
  coord_cartesian(xlim = c(0, 20)) +
  labs(title = "1. Misma probabilidad → valor observado",
       x = "Precipitación diaria (mm)", y = "Probabilidad acumulada") +
  tema

transf <- tibble(x = seq(0, 20, 0.05)) |> mutate(y = doQmap(x, ajuste))

g_tf <- ggplot(transf, aes(x, y)) +
  geom_abline(slope = 1, intercept = 0, color = "grey60", linetype = "22") +
  geom_line(color = naranja, linewidth = 1.3) +
  geom_point(data = ejemplos, aes(x_mod, x_cor), size = 3, color = naranja) +
  annotate("text", x = 4.6, y = 6.2, label = "1:1 (sin corrección)",
           color = "grey40", size = 3.5, angle = 45) +
  annotate("text", x = 12, y = 4.5, label = "bajo la línea:\nel modelo llueve de más",
           color = "grey30", size = 3.4, hjust = 0.5) +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20)) +
  labs(title = "2. Función de transferencia",
       x = "Valor del modelo (mm)", y = "Valor corregido (mm)") +
  tema

g_qm <- g_cdf + g_tf + plot_layout(widths = c(1.5, 1))
ggsave("assets/img/s09_concepto_qm.png", g_qm, width = 12, height = 5.5, dpi = 150)
