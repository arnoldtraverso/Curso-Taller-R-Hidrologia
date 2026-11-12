# =============================================================================
# Curso-Taller: Programación en R Aplicada a la Hidrología,
#               Servicios Climáticos y Adaptación al Cambio Climático
# 00 - Instalación y verificación de paquetes
# =============================================================================
#
# EJECUTAR ANTES DEL CURSO (se necesita conexión a internet)
#
# Requisitos previos:
#   1. R versión 4.3 o superior   -> https://cran.r-project.org
#   2. RStudio Desktop            -> https://posit.co/download/rstudio-desktop
#
# Instrucciones:
#   1. Abre este archivo en RStudio.
#   2. Presiona el botón "Source" (o Ctrl + Shift + S) para ejecutarlo completo.
#   3. Espera a que termine. Puede tardar entre 10 y 30 minutos según tu conexión.
#   4. Revisa el mensaje final en la consola:
#        "TODO LISTO"  -> ya puedes cerrar RStudio.
#        "ATENCIÓN"    -> vuelve a ejecutar el script; si el problema continúa,
#                         envía una captura de la consola al instructor.
#
# Si aparece una ventana preguntando por una "biblioteca personal"
# (personal library), responde "Sí / Yes".
# =============================================================================


# 1. Versión de R -------------------------------------------------------------

version_r <- getRversion()
cat("Versión de R instalada:", as.character(version_r), "\n")

if (version_r < "4.3.0") {
  stop("Tu versión de R es anterior a la 4.3. ",
       "Instala la versión actual desde https://cran.r-project.org ",
       "y vuelve a ejecutar este script.", call. = FALSE)
}


# 2. Paquetes del curso -------------------------------------------------------

paquetes <- c(
  "tidyverse",   # readr, dplyr, tidyr, ggplot2, purrr, lubridate
  "readxl",      # lectura de archivos Excel
  "terra",       # datos grillados (NetCDF, ráster)
  "trend",       # Mann-Kendall y pendiente de Sen
  "modifiedmk",  # Mann-Kendall modificado
  "qmap",        # corrección de sesgo por mapeo de cuantiles
  "airGR",       # modelos hidrológicos GR4J / GR2M
  "hydroGOF",    # métricas de desempeño NSE, KGE
  "reticulate"   # conexión con Python
)


# 3. Instalación --------------------------------------------------------------

options(
  repos   = c(CRAN = "https://cloud.r-project.org"),  # servidor de descarga
  timeout = 600,                                      # conexiones lentas
  install.packages.compile.from.source = "never"      # usar versiones ya compiladas
)

# Instala solo los paquetes que faltan
faltantes <- setdiff(paquetes, rownames(installed.packages()))

if (length(faltantes) > 0) {
  cat("Paquetes por instalar:", paste(faltantes, collapse = ", "), "\n")
  install.packages(faltantes)
} else {
  cat("Todos los paquetes ya estaban instalados.\n")
}


# 4. Verificación -------------------------------------------------------------

# Comprueba que cada paquete se puede cargar
carga <- sapply(paquetes, function(p) {
  suppressWarnings(suppressPackageStartupMessages(
    requireNamespace(p, quietly = TRUE)
  ))
})

resumen <- data.frame(
  paquete = paquetes,
  estado  = ifelse(carga, "OK", "FALTA"),
  version = sapply(paquetes, function(p) {
    if (carga[[p]]) as.character(packageVersion(p)) else "-"
  }),
  row.names = NULL
)

cat("\n=============================================================\n")
cat(" RESUMEN DE INSTALACIÓN\n")
cat("=============================================================\n")
cat(" R", as.character(version_r), "|", R.version$platform, "\n\n")
print(resumen, row.names = FALSE, right = FALSE)
cat("-------------------------------------------------------------\n")

if (all(carga)) {
  cat(" TODO LISTO: tu computadora está preparada para el curso.\n")
} else {
  cat(" ATENCIÓN: no se pudo instalar o cargar:",
      paste(paquetes[!carga], collapse = ", "), "\n")
  cat(" Vuelve a ejecutar el script. Si el problema continúa,\n")
  cat(" envía una captura de esta consola al instructor.\n")
}
cat("=============================================================\n")

