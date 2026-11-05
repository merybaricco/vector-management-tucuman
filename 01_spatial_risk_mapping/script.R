# ==============================================================================
# Proyecto: Vector Management Tucumán
# Análisis: Mapeo de Riesgo Espacial y Variables Ambientales (SIG)
# Descripción: Procesamiento de capas vectoriales y ráster (basurales, espacios 
#              verdes, NDVI, hacinamiento), cálculo de distancias, generación de 
#              mapas predictivos de abundancia y análisis con datos epidemiológicos.
# ==============================================================================

# 1. Carga de librerías necesarias ---------------------------------------------
library(sf)
library(terra)
library(tidyverse)

# Nota: Asegúrate de ajustar las rutas según tu directorio de trabajo o usar un RStudio Project (.Rproj)

# ==============================================================================
# 2. PROCESAMIENTO DE VARIABLES ESPACIALES Y DISTANCIAS EN YERBA BUENA
# ==============================================================================

# A. Distancia a basurales
bas_yb <- st_read("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/basurales/basurales_yb.shp")
puntos <- st_read("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/yb/sensores_yb.shp")
ndvi   <- rast("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/yb/ndvi_yb.tif")

# Homogeneizar sistemas de referencia (CRS) con el ráster base (ndvi)
bas <- st_transform(bas_yb, crs(ndvi))
basr <- rasterize(bas, ndvi)
a <- distance(ndvi, bas) # Mapa de distancias a basurales

# B. Distancia a espacios verdes
ev_yb <- st_read("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/espacios verdes/ev_yb.shp")
ev <- st_transform(ev_yb, crs(ndvi))
evr <- rasterize(ev, ndvi)
b <- distance(ndvi, ev) # Mapa de distancias a espacios verdes

# C. Variables sociodemográficas (Hacinamiento y densidad)
rc <- st_read("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/yb/rc_yb_atributos.shp")
hac <- st_transform(rc, crs(ndvi))

hacr <- rasterize(hac, ndvi, field = "phacin")
densidad <- rasterize(hac, ndvi, field = "densidad")


# ==============================================================================
# 3. GENERACIÓN DE MAPAS PREDICTIVOS DE ABUNDANCIA / RIESGO
# ==============================================================================

# Nota: Se asume que los objetos de modelos lineales (ml3, ml4, ml5) y la 
# tabla 'atributos' están cargados en el entorno de R.

# Ejemplo con modelo 3
huevos2 <- coef(ml3)[1] + a * coef(ml3)[2] + b * coef(ml3)[4] + hacr * coef(ml3)[3]

# Ejemplo con modelo 4
huevos3 <- coef(ml4)[1] + a * coef(ml4)[2] + densidad * coef(ml4)[3]

# Ejemplo con modelo 5 (ml5)
# ml5 <- lm(formula = abundancia ~ dist.bas + densidad + as.numeric(ndvi) + categoria, data = atributos)
huevos4 <- coef(ml5)[1] + a * coef(ml5)[2] + densidad * coef(ml5)[3] + ndvi * coef(ml5)[4] 

# Exportar mapa ráster resultante
writeRaster(huevos4, "mapa_yb_densidad.tif", overwrite = TRUE)


# ==============================================================================
# 4. INTEGRACIÓN CON DATOS EPIDEMIOLÓGICOS (Dengue y COVID)
# ==============================================================================

covid_gsmt <- rast("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/Enfermedad/gsmt/covid_gsmt.tif")
dengue_tuc <- rast("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/Enfermedad/gsmt/dengue_16_20.tif")

# Remuestreo bilinear para alinear resoluciones
covid <- resample(covid_gsmt, ndvi, method = "bilinear")
dengue <- resample(dengue_tuc, ndvi, method = "bilinear")

# Análisis espacial a nivel departamental / radios censales (GSMT)
smt <- st_read("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/departamentos/gsmt.shp")
ext_comun <- intersect(ext(dengue_tuc), ext(covid_gsmt))
c2 <- crop(covid_gsmt, ext_comun)
d2 <- crop(dengue_tuc, ext_comun)

ratio <- d2 / c2
ratio2 <- ceiling(d2) / ceiling(c2)

rc_indec <- st_read("C:/Users/meryb/Documents/doctorado/Datos/Espaciales/indec/gsmt_RC_indec.shp")
rc_indec$ratio <- unlist(extract(ratio2, as(rc_indec, "SpatVector"), fun = mean)[, 2])

writeRaster(ratio, "ratio.tif", overwrite = TRUE)


# ==============================================================================
# 5. MODELACIÓN ESTADÍSTICA DE INCIDENCIA Y RIESGO
# ==============================================================================

riesgo_yb <- read.csv("C:/Users/meryb/Documents/doctorado/eze/DATOS/csv/riesgo_rc_yb.csv", sep = ";", dec = ",")

# Modelo lineal simple: Incidencia vs Riesgo
modelo <- lm(riesgo_yb$ratio ~ riesgo_yb$riesgo)
summary(modelo)

# Análisis de residuos con proporción de hacinamiento
datos_limpios <- na.omit(riesgo_yb[, c("ratio", "riesgo", "phacin")])
modelo2 <- lm(ratio ~ riesgo, data = datos_limpios)

ajuste_lineal <- lm(residuals(modelo2) ~ datos_limpios$phacin)

# Modelo lineal múltiple incluyendo hacinamiento
modelo3 <- lm(ratio ~ riesgo + phacin, data = datos_limpios)
summary(modelo3)
