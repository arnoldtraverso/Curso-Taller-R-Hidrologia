# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 09 - R + Python con reticulate (demostración)
# =============================================================================
#
# El mismo NetCDF de PISCOp v3 de la sección 05 del Día 1, leído con xarray
# (Python) desde R. Se calcula la precipitación anual promedio de la grilla
# recortada (11 x 11 celdas) y se compara con terra: ambos dan lo mismo.
#
# Requiere (solo para quien quiera reproducirlo):
#   - Python 3 instalado (https://www.python.org)
#   - En una terminal:  python -m pip install xarray netCDF4 pandas
#   - En R:             install.packages("reticulate")
#
# Cómo usar este script:
#   - Abre el proyecto Curso-Taller-R-Hidrologia.Rproj
#   - Ejecuta línea por línea con Ctrl + Enter
#   - Las líneas que empiezan con "#>" muestran el resultado esperado
# =============================================================================


# 1. Conectar R con Python ---------------------------------------------------------

library(tidyverse)
library(terra)
library(reticulate)

use_python(Sys.which("python"))   # el Python instalado en la computadora
py_config()$version
#> [1] "3.13"

xr <- import("xarray")            # como "import xarray as xr" en Python


# 2. Leer el NetCDF con xarray ---------------------------------------------------

archivo <- "data/raw/pisco/PISCOp_v3_diario_Huancane.nc"

ds <- xr$open_dataset(archivo)    # el "$" de R reemplaza al "." de Python
ds                                # xarray sí reconoce las fechas del archivo
#> Dimensions:  (time: 16436, latitude: 11, longitude: 11)
#> Coordinates:
#>   * time       (time) datetime64[ns] 1981-01-01 ... 2025-12-31
#>   * latitude   (latitude) float64 -14.45 -14.55 ... -15.45
#>   * longitude  (longitude) float64 -70.25 -70.15 ... -69.25
#> Data variables:
#>     pp         (time, latitude, longitude) float32 ...


# 3. Precipitación anual con xarray ----------------------------------------------

# Python:  ds.pp.resample(time="YE").sum().mean(["latitude", "longitude"])
anual_py <- ds$pp$
  resample(time = "YE")$sum()$          # diario -> anual (suma)
  mean(c("latitude", "longitude"))$     # promedio de las 121 celdas
  to_pandas()                           # serie de pandas -> vector de R

pp_py <- tibble(anio = 1981:2025, pp_python = as.numeric(anual_py))
head(pp_py, 3)
#>    anio pp_python
#> 1  1981      835.
#> 2  1982      683.
#> 3  1983      501.


# 4. Lo mismo con terra -------------------------------------------------------------

r <- rast(archivo)
time(r) <- seq(as.Date("1981-01-01"), by = "day", length.out = nlyr(r))

pp_r <- tapp(r, "years", sum) |>
  global("mean", na.rm = TRUE) |>
  as_tibble() |>
  transmute(anio = 1981:2025, pp_r = mean)

comparar <- left_join(pp_py, pp_r, by = "anio") |>
  mutate(diferencia = pp_python - pp_r)
head(comparar, 3)
#>    anio pp_python  pp_r  diferencia
#> 1  1981      835.  835. -0.0000522
#> 2  1982      683.  683.  0.0000120
#> 3  1983      501.  501.  0.0000479

max(abs(comparar$diferencia))
#> [1] 0.0001372217      (mm/año: solo redondeo de float32)


# 5. Escribir Python directamente ------------------------------------------------

py_run_string("
import xarray as xr
ds = xr.open_dataset(r'data/raw/pisco/PISCOp_v3_diario_Huancane.nc')
n = ds.pp.sel(time=slice('1991', '2020'))      # normal 1991-2020
c = n.groupby('time.month').sum() / 30         # mm/mes en cada celda
clim = c.mean(['latitude', 'longitude'])       # promedio de la grilla
")

# Lo creado en Python se lee desde R con py$
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
tibble(mes = meses, pp = round(as.numeric(py$clim$values), 1))
#>    mes      pp
#>  1 Ene   127.7
#>  2 Feb   121.7
#>  3 Mar    99
#>  4 Abr    40.9
#>  5 May    14.9
#>  6 Jun     6.4
#>  7 Jul     6
#>  8 Ago     9.7
#>  9 Set    22.9
#> 10 Oct    59.4
#> 11 Nov    66.1
#> 12 Dic   111.9
