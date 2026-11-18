# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 01 - Fundamentos
# =============================================================================
#
# Objetos, tipos de datos, vectores, data frames, listas, fechas y paquetes.
# Datos de ejemplo: cuenca del río Huancané (ANA).
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter y observa la consola
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Objetos y asignación ------------------------------------------------------

caudal   <- 12.69          # m³/s, Puente Carretera Huancané el 01/01/2024
estacion <- "Huancané"

caudal
#> [1] 12.69

# R como calculadora hidrológica
vol_dia <- caudal * 86400  # m³ que pasan en un día (86 400 s)
vol_dia
#> [1] 1096416

vol_dia / 1e6              # en hm³ (millones de m³)
#> [1] 1.096416

# R distingue mayúsculas: la siguiente línea da error
# Caudal


# 2. Tipos de datos ------------------------------------------------------------

class(caudal)              #> "numeric"
class(estacion)            #> "character"
class(caudal > 10)         #> "logical"
class(as.Date("2024-01-01"))  #> "Date"

caudal > 10                #> TRUE
is.na(NA)                  #> TRUE   (NA = dato faltante)


# 3. Vectores ------------------------------------------------------------------

# Precipitación mensual promedio (mm), estación Huancané, periodo 1991-2020
pp_mensual <- c(134.0, 104.9, 104.2, 36.8, 11.2, 7.0,
                4.2, 8.8, 25.0, 56.4, 58.6, 105.8)
names(pp_mensual) <- month.abb

pp_mensual
length(pp_mensual)         #> 12

# Funciones de resumen
sum(pp_mensual)            #> 656.9  (precipitación anual, mm)
mean(pp_mensual)           #> 54.74
max(pp_mensual)            #> 134
min(pp_mensual)            #> 4.2
which.max(pp_mensual)      #> Jan 1  (mes más lluvioso)
which.min(pp_mensual)      #> Jul 7  (mes más seco)

# Operaciones vectorizadas: se aplican a los 12 meses a la vez
pp_mensual / 10                                  # de mm a cm
round(pp_mensual / sum(pp_mensual) * 100, 1)     # % del total anual

# Seleccionar elementos
pp_mensual[1]                  # enero
pp_mensual[c(12, 1, 2, 3)]     # temporada de lluvias DEFM
pp_mensual["Jul"]              # por nombre

# Condiciones lógicas
pp_mensual > 100
pp_mensual[pp_mensual > 100]   # meses con más de 100 mm
sum(pp_mensual > 100)          #> 4   (TRUE cuenta como 1)

# ¿Qué porcentaje de la lluvia anual cae en DEFM?
sum(pp_mensual[c(12, 1, 2, 3)]) / sum(pp_mensual) * 100
#> 68.3


# 4. Data frames ---------------------------------------------------------------

# Estaciones pluviométricas de la cuenca Huancané y alrededores
estaciones <- data.frame(
  nombre  = c("Ananea", "Muñani", "Putina", "Huancané", "Cojata"),
  codigo  = c("000826", "000785", "157414", "000786", "000827"),
  lat     = c(-14.676, -14.767, -14.921, -15.207, -15.012),
  lon     = c(-69.534, -69.952, -69.876, -69.758, -69.361),
  altitud = c(4660, 3932, 3861, 3842, 4347)
)

estaciones
str(estaciones)
nrow(estaciones)               #> 5
names(estaciones)
summary(estaciones)

# Acceder a columnas, filas y celdas
estaciones$altitud
mean(estaciones$altitud)       #> 4128.4
estaciones[2, ]                # fila 2
estaciones[ , c("nombre", "altitud")]
estaciones[4, "altitud"]       #> 3842

# Filtrar filas con una condición
estaciones[estaciones$altitud > 4000, ]

# Ordenar por altitud
estaciones[order(estaciones$altitud, decreasing = TRUE), ]

# Crear columnas nuevas
estaciones$zona <- ifelse(estaciones$altitud > 4000, "alta", "media")
estaciones$altitud_km <- estaciones$altitud / 1000
estaciones


# 5. Listas --------------------------------------------------------------------

huancane <- list(
  nombre     = "Huancané",
  codigo     = "000786",
  coord      = c(lat = -15.207, lon = -69.758),
  pp_mensual = pp_mensual
)

str(huancane)
huancane$nombre                #> "Huancané"
huancane$coord["lat"]          #> -15.207
huancane[["pp_mensual"]]
sum(huancane$pp_mensual)       #> 656.9

# Agregar un elemento nuevo a la lista
huancane$cuenca <- "Huancané"
names(huancane)


# 6. Fechas con lubridate ------------------------------------------------------

library(lubridate)

# SENAMHI entrega la fecha como texto día/mes/año
fecha_txt <- "04/05/2026"
class(fecha_txt)               #> "character"

fecha <- dmy(fecha_txt)
fecha                          #> "2026-05-04"
class(fecha)                   #> "Date"

year(fecha)                    #> 2026
month(fecha)                   #> 5
month(fecha, label = TRUE)     # nombre del mes
yday(fecha)                    #> 124  (día del año)
fecha + 30                     #> "2026-06-03"

# Otras funciones según el orden del texto
ymd("2024-01-31")
mdy("01/31/2024")

# Secuencias de fechas
dias <- seq(ymd("2024-01-01"), ymd("2024-12-31"), by = "day")
length(dias)                   #> 366  (2024 es bisiesto)

meses <- seq(ymd("1991-01-01"), ymd("2020-12-01"), by = "month")
length(meses)                  #> 360

# ¿Cuántos días abarca el registro de caudal?
inicio <- ymd("1956-01-01")
fin    <- ymd("2026-05-04")
fin - inicio                   #> Time difference of 25691 days
as.numeric(fin - inicio) + 1   #> 25692 días (incluyendo ambos extremos)


# 7. Paquetes y el pipe |> -----------------------------------------------------

# install.packages("tidyverse")  # solo una vez (ya lo hiciste con 00_Instalacion_paquetes.R)
library(tidyverse)
library(readxl)

# El pipe |> pasa el resultado a la siguiente función ("y luego...")
round(mean(pp_mensual), 1)
pp_mensual |> mean() |> round(1)        #> 54.7


# 8. Primer contacto con los datos reales --------------------------------------

q <- read_excel("data/raw/caudal/Q_210201_PteCarretera_Huancane.xlsx",
                skip = 13)               # saltar las 13 filas de metadatos
head(q)
nrow(q)                                  #> 25208
class(q$FECHA)                           #> "character"  -> hay que convertirla

# Convertir la fecha y ver el rango del registro
fechas_q <- dmy(q$FECHA)
range(fechas_q)                          #> "1956-01-01" "2026-05-04"

# Esto lo resolveremos en 02_Procesamiento.R:
#   - FECHA como texto, orden descendente
#   - días faltantes sin fila
#   - días duplicados en precipitación (2017-2024)


# =============================================================================
# EJERCICIOS
# =============================================================================

# E1. El caudal medio del registro es 18.27 m³/s. Calcula el volumen anual
#     escurrido en hm³ (1 año = 365 días).

# E2. Con pp_mensual, calcula la precipitación del periodo seco (mayo a
#     agosto) y su porcentaje respecto al total anual.

# E3. ¿Cuántos meses tienen menos de 30 mm? ¿Cuáles son?

# E4. Agrega al data frame "estaciones" una columna "cuenca" con los valores:
#     Azángaro, Huancané, Huancané, Huancané, Suches.
#     Luego muestra solo las estaciones de la cuenca Huancané.

# E5. ¿Qué día de la semana fue el caudal máximo del registro (27/02/2016)?
#     Pista: wday(..., label = TRUE)

# E6. Lee un archivo de precipitación de data/raw/precipitacion/ con
#     read_excel() y responde: ¿cuántas filas tiene? ¿cuál es su rango de fechas?
