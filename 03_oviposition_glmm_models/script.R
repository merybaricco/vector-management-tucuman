# ==============================================================================
# Proyecto: Vector Management Tucumán
# Análisis: Modelos Lineales Generalizados Mixtos (GLMM - Binomial Negativa)
# Descripción: Modelado de la abundancia de Aedes aegypti (años 2024, 2025 y 2026)
#              evaluando variables climáticas, urbanas, espaciales y categóricas.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
paquetes <- c("Matrix", "lme4", "rlang")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Nota: Se asume que 'datos_completos' ya se encuentra cargado en el entorno de R.

# 2. Preparación de datos -----------------------------------------------------
# Establecer "Vivienda" como la categoría de referencia para el factor
datos_completos$categoria <- relevel(factor(datos_completos$categoria), ref = "Vivienda")


# ==============================================================================
# 3. MODELOS SEMANALES "GANADORES" (Completos)
# ==============================================================================

glmm_nb24 <- glmer.nb(ab24 ~ scale(temp_sup24) + scale(ndvi) + scale(phacin) +
                        scale(temp24) + scale(humr24) + scale(prec24) + 
                        scale(ab24_previa) + scale(dist.bas) + scale(dist.can) + 
                        scale(dist.cem) + scale(dist.ev) + categoria +
                        (1 | sitio), 
                      data = datos_completos)
summary(glmm_nb24)


glmm_nb25 <- glmer.nb(ab25 ~ scale(temp_sup25) + scale(ndvi) + scale(phacin) +
                        scale(temp25) + scale(humr25) + scale(prec25) + 
                        scale(ab25_previa) + scale(dist.bas) + scale(dist.can) +
                        scale(dist.cem) + scale(dist.ev) + categoria +
                        (1 | sitio), 
                      data = datos_completos)
summary(glmm_nb25)


glmm_nb26 <- glmer.nb(ab26 ~ scale(temp_sup26) + scale(ndvi) + scale(phacin) +
                        scale(temp26) + scale(humr26) + scale(prec26) + 
                        scale(ab26_previa) + scale(dist.bas) + scale(dist.can) +
                        scale(dist.cem) + scale(dist.ev) + categoria +
                        (1 | sitio), 
                      data = datos_completos)
summary(glmm_nb26)


# ==============================================================================
# 4. MODELOS CLIMÁTICOS
# ==============================================================================

glmm_nb1 <- glmer.nb(ab24 ~ scale(temp24) + scale(humr24) + scale(prec24) + 
                       (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb1)


glmm_nb2 <- glmer.nb(ab25 ~ scale(temp25) + scale(humr25) + scale(prec25) + 
                       (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb2)


glmm_nb3 <- glmer.nb(ab26 ~ scale(temp26) + scale(humr26) + scale(prec26) + 
                       (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb3)


# ==============================================================================
# 5. MODELOS URBANOS / MICROAMBIENTALES
# ==============================================================================

glmm_nb4 <- glmer.nb(ab24 ~ scale(ab24_previa) + scale(temp_sup24) + scale(ndvi) + scale(phacin) 
                     + (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb4)


glmm_nb5 <- glmer.nb(ab25 ~ scale(ab25_previa) + scale(temp_sup25) + scale(ndvi) + scale(phacin) 
                     + (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb5)


glmm_nb6 <- glmer.nb(ab26 ~ scale(ab26_previa) + scale(temp_sup26) + scale(ndvi) + scale(phacin) 
                     + (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb6)


# ==============================================================================
# 6. MODELOS ESPACIALES (Distancias a hitos)
# ==============================================================================

glmm_nb7 <- glmer.nb(ab24 ~ scale(dist.bas) + scale(dist.can) + scale(dist.cem) +
                       scale(dist.ev) +
                       (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb7)


glmm_nb8 <- glmer.nb(ab25 ~ scale(dist.bas) + scale(dist.can) + scale(dist.cem) +
                       scale(dist.ev) +
                       (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb8)


glmm_nb9 <- glmer.nb(ab26 ~ scale(dist.bas) + scale(dist.can) + scale(dist.cem) +
                       scale(dist.ev) +
                       (1 | sitio), 
                     data = datos_completos)
summary(glmm_nb9)


# ==============================================================================
# 7. MODELOS DE INTERACCIÓN (Temperatura x Categoría)
# ==============================================================================

glmm_nb10 <- glmer.nb(ab24 ~ scale(temp24) * categoria + (1 | sitio),
                      data = datos_completos)
summary(glmm_nb10)


glmm_nb11 <- glmer.nb(ab25 ~ scale(temp25) * categoria + (1 | sitio),
                      data = datos_completos)
summary(glmm_nb11)


glmm_nb12 <- glmer.nb(ab26 ~ scale(temp26) * categoria + (1 | sitio),
                      data = datos_completos)
summary(glmm_nb12)
