# Datos originales (`data/raw/`)

Estos archivos **no se modifican**. Todo procesamiento se hace con scripts y los
resultados se guardan en `data/processed/` u `outputs/`.

- **Fuente:** SENAMHI — Servicio Nacional de Meteorología e Hidrología del Perú
  (descarga de datos hidrometeorológicos).
- **Fecha de descarga:** 12/11/2026.
- **Advertencia de la fuente:** los datos corresponden a información primaria
  **sin control de calidad**.

## Formato de los archivos Excel

Todos los archivos tienen la misma estructura (hoja `Reporte`):

| Filas | Contenido |
|---|---|
| 1–13 | Metadatos: estación, variable, operador, coordenadas, tipo, ámbito, cuenca |
| 14 | Encabezados: `FECHA`, `HORA`, `VALOR` |
| 15 en adelante | Datos diarios |

- `FECHA` está como **texto** en formato `DD/MM/AAAA`.
- Las filas están ordenadas de la fecha **más reciente a la más antigua**.
- Los días sin dato **no tienen fila**, así que no aparecen como `NA`.

Lectura básica en R:

```r
library(readxl)
q <- read_excel("data/raw/caudal/Q_210201_PteCarretera_Huancane.xlsx", skip = 13)
```

## Caudal (`caudal/`)

| Archivo | Nombre original | Estación | Código | Lat | Lon | Altitud (msnm) | Variable | Periodo |
|---|---|---|---|---|---|---|---|---|
| `Q_210201_PteCarretera_Huancane.xlsx` | `Reporte12112026080220.xlsx` | Puente Carretera Huancané | 210201 | -15.215889 | -69.792611 | 3821 | Caudal promedio diario (m³/s) | 1956-01-01 a 2026-05-04 |

- Hay 484 días sin registro. Los años más incompletos son 1975, 2020, 1985 y 1993.
- La columna `HORA` siempre es `00:00`.

## Precipitación (`precipitacion/`)

Variable: precipitación acumulada en 1 día (mm). Todas son estaciones convencionales climáticas.

| Archivo | Nombre original | Estación | Código | Lat | Lon | Altitud (msnm) | Cuenca (SENAMHI) | Periodo |
|---|---|---|---|---|---|---|---|---|
| `PP_000826_Ananea.xlsx` | `Reporte12112026214956.xlsx` | Ananea | 000826 | -14.6763 | -69.5339 | 4660 | Azángaro | 1963-09-18 a 2024-12-30 |
| `PP_000785_Munani.xlsx` | `Reporte12112026215035.xlsx` | Muñani | 000785 | -14.766944 | -69.951806 | 3932 | Huancané | 1964-05-01 a 2026-08-18 |
| `PP_157414_Putina.xlsx` | `Reporte12112026215042.xlsx` | Putina | 157414 | -14.921 | -69.8757 | 3861 | Huancané | 1970-06-01 a 2026-08-18 |
| `PP_000786_Huancane.xlsx` | `Reporte12112026215049.xlsx` | Huancané | 000786 | -15.20687 | -69.7583 | 3842 | Huancané | 1963-11-01 a 2026-08-20 |
| `PP_000827_Cojata.xlsx` | `Reporte12112026215105.xlsx` | Cojata | 000827 | -15.0118 | -69.3612 | 4347 | Suches | 1956-05-21 a 2026-04-30 |

Ananea y Cojata pertenecen a cuencas vecinas. Se incluyen como estaciones de apoyo
de la parte alta.

### ⚠ Registros duplicados (2017–2024)

En ese periodo, muchos días tienen **dos registros**: uno con `HORA = 00:00` y otro con `05:00`.

- Los valores coinciden casi siempre. Difieren en 1 a 15 días por estación, sobre todo a fin de mes.
- En Cojata hay unos 2 450 días que **solo** tienen el registro `05:00`.
- Si se suman sin tratar, la precipitación mensual y anual de esos años sale casi duplicada.

**Regla adoptada en el curso:** por cada fecha se usa el registro `00:00`; si no existe, el de `05:00`.


## PISCO (`pisco/`)

| Archivo | Contenido | Periodo | Resolución | En el repo |
|---|---|---|---|---|
| `PISCOp_d.nc` | PISCOp v3.0 (SENAMHI; Gutiérrez et al., 2026), precipitación diaria (mm), todo el Perú, 1.5 GB | 1981-01-01 a 2025-12-31 | 0.1° | No (`.gitignore`) |
| `PISCOp_v3_diario_Huancane.nc` | Recorte de lon -70.3 a -69.2 y lat -15.5 a -14.4 (11 × 11 celdas), 5.9 MB | 1981-01-01 a 2025-12-31 | 0.1° | Sí |

- El archivo nacional no trae fechas: su eje `Z1` va de 1 a 16436 (días desde el 1981-01-01).
  El recorte ya incluye las fechas.
- El recorte se hace en la sección 2 de `ejercicios/04_Grillados.R` con `ncdf4` (menos de 1 s).
  Con `terra::crop()` tarda más de 10 minutos por la forma en que el archivo está fragmentado.
- Verificación: la precipitación areal del recorte por subcuenca coincide con `../pcp/Datos_pcp.csv`
  (r = 0.97 a 0.998 con desfase 0 días; r ≈ 0.5 con ±1 día).

**ETP de PISCO descartada:** el producto `PISCO/.PET/.v1p1` de la IRI Data Library viene etiquetado
como `mm/day`, pero los valores están en mm/mes. Además, su ciclo estacional se desplaza unos 2 meses
por año. La ETP se calcula por Hargreaves-Samani con la temperatura de `../temp/`.

## Precipitación por subcuenca (`../pcp/`)

`Datos_pcp.csv`: precipitación diaria PISCOp v3 (mm), promedio areal de cada subcuenca (`Sb1` a `Sb5`),
1981-01-01 a 2025-12-31. Se usa en el ejercicio E4 de `04_Grillados.R`.
## Temperatura (`../temp/`)

`Datos_tmax.csv` y `Datos_tmin.csv`: temperatura máxima y mínima diaria (°C), promedio areal
de cada subcuenca (columnas `Sb1` a `Sb5` = campo `Subbasin` del shapefile), 1981-01-01 a 2020-12-31.

Fuente: PISCO temperatura diaria 1981-2020 (SENAMHI), promediada por subcuenca:

- Tmax: https://doi.org/10.6084/m9.figshare.20522806
- Tmin: https://doi.org/10.6084/m9.figshare.20533715

## Cuenca y ríos (`../basin/`, `../rivers/`)

`Subbasins.shp`: 5 subcuencas del río Huancané hasta la estación Puente Carretera (3545 km²).
`Rivers.shp`: red de drenaje principal. Ambos en WGS 84 / UTM 19S (EPSG:32719).

## ../cmip6/ — proyecciones NEX-GDDP-CMIP6 (SSP2-4.5)

Archivos `Huancane_<MODELO>_ssp245.csv` descargados de Google Earth Engine (colección `NASA/GDDP-CMIP6`) con `ejercicios/Descarga_CMIP6_GEE.js`: promedio diario por subcuenca, 1981-01-01 a 2049-12-31 (histórico hasta 2014, SSP2-4.5 desde 2015).

Columnas: `Fecha`, `Subcuenca`, `Modelo`, `Escenario` (historical / ssp245), `Pr_mm` (mm/día), `Tmax` y `Tmin` (°C).

Modelos: ACCESS-CM2, MPI-ESM1-2-HR y MIROC6.

Referencia: Thrasher, B. et al. (2022). NASA Global Daily Downscaled Projections, CMIP6. *Scientific Data*, 9, 262.
