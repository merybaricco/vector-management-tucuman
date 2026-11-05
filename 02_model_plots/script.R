# ==============================================================================
# Proyecto: Vector Management Tucumán
# Análisis: Visualización de Modelos (Forest Plots)
# Descripción: Generación de gráficos de Razones de Tasas de Incidencia (IRR) 
#              con intervalos de confianza del 95% usando broom.mixed y ggplot2.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(tidyverse)

# Verificar e instalar broom.mixed si es necesario
if (!requireNamespace("broom.mixed", quietly = TRUE)) {
  install.packages("broom.mixed", dependencies = TRUE)
}
library(broom.mixed)

# ==============================================================================
# 2. PARTE A: Comparación de modelos saturados (Temporadas 23/24, 24/25 y 25/26)
# ==============================================================================

# Lista con los modelos ganadores de cada temporada (asume que ya están cargados)
modelos <- list(
  "23/24" = glmm_nb24,
  "24/25" = glmm_nb25,
  "25/26" = glmm_nb26
)

# Extracción y limpieza de coeficientes (exponenciados para obtener IRR)
coefs <- imap_dfr(modelos, function(m, temporada) {
  broom.mixed::tidy(m, conf.int = TRUE, exponentiate = TRUE,
                    effects = "fixed", conf.method = "Wald") |>
    mutate(temporada = temporada)
}) |>
  filter(term != "(Intercept)")

# Normalización de nombres (remover scale() y sufijos de año)
coefs <- coefs |>
  mutate(term_base = term |>
           str_remove("^scale\\(") |>
           str_remove("\\)$") |>
           str_replace("2[456]", ""))

# Tabla de etiquetas y asignación de grupos temáticos
etiquetas <- tribble(
  ~term_base,              ~variable,                     ~grupo,
  "temp",                  "Temperatura media",           "Meteorológicas",
  "humr",                  "Humedad relativa",            "Meteorológicas",
  "prec",                  "Precipitación",               "Meteorológicas",
  "temp_sup",              "Temperatura superficial",     "Ambientales",
  "ndvi",                  "NDVI",                        "Ambientales",
  "phacin",                "Hacinamiento (%)",            "Ambientales",
  "ab_previa",             "Abundancia semana previa",    "Ambientales",
  "dist.bas",              "Dist. a basurales",           "Espaciales",
  "dist.can",              "Dist. a canales",             "Espaciales",
  "dist.cem",              "Dist. a cementerios",         "Espaciales",
  "dist.ev",               "Dist. a espacios verdes",     "Espaciales",
  "categoriaCanal",        "Canal (vs. basural)",         "Categoría",
  "categoriaCAPS",         "CAPS (vs. basural)",          "Categoría",
  "categoriaCementerio",   "Cementerio (vs. basural)",    "Categoría",
  "categoriaEspacio verde","Espacio verde (vs. basural)", "Categoría"
)

datos <- coefs |>
  inner_join(etiquetas, by = "term_base") |>
  mutate(
    significativo = p.value < 0.05,
    grupo = factor(grupo, levels = c("Meteorológicas", "Ambientales",
                                     "Espaciales", "Categoría")),
    variable = factor(variable, levels = rev(etiquetas$variable))
  )

# Función personalizada para graficar los forest plots comparativos
pos <- position_dodge(width = 0.7)

forest_fun <- function(d) {
  ggplot(d, aes(x = estimate, y = variable,
                color = temporada, shape = significativo)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.7, position = pos) +
    geom_point(size = 2.8, position = pos, fill = "white", stroke = 1) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                       labels = c(`TRUE` = "p < 0.05", `FALSE` = "n.s."),
                       name = NULL) +
    scale_color_manual(values = c("23/24" = "#1b9e77",
                                  "24/25" = "#d95f02",
                                  "25/26" = "#7570b3"),
                       name = "Temporada") +
    scale_x_log10() +
    facet_grid(grupo ~ ., scales = "free_y", space = "free_y",
               drop = TRUE) +
    labs(x = "Razón de tasas de incidencia (IRR, escala log) e IC 95%",
         y = NULL,
         title = "Comparación de modelos saturados") +
    theme_bw(base_size = 12) +
    theme(strip.text.y = element_text(angle = 0, face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}

# Generar y guardar las figuras separadas para continuas y categorías
p_cont <- forest_fun(filter(datos, grupo != "Categoría"))
p_cat  <- forest_fun(filter(datos, grupo == "Categoría"))

ggsave("forest_continuas.png", p_cont, width = 8, height = 7, dpi = 300)
ggsave("forest_categorias.png", p_cat,  width = 8, height = 3.5, dpi = 300)


# ==============================================================================
# 3. PARTE B: Forest Plot específico para el mejor modelo de la temporada 25/26
# ==============================================================================

coef_nb6 <- broom.mixed::tidy(glmm_nb6, effects = "fixed",
                            conf.int = TRUE, exponentiate = TRUE,
                            conf.method = "Wald") |>
  filter(term != "(Intercept)")

etiquetas_nb6 <- tribble(
  ~term,               ~variable,
  "scale(temp_sup26)", "Temperatura superficial",
  "scale(ab26_previa)","Abundancia semana previa",
  "scale(ndvi)",       "NDVI",
  "scale(phacin)",     "Hacinamiento (%)"
)

d_nb6 <- coef_nb6 |>
  inner_join(etiquetas_nb6, by = "term") |>
  mutate(
    significativo = p.value < 0.05,
    etiqueta = sprintf("IRR = %.2f [%.2f–%.2f]", estimate, conf.low, conf.high),
    variable = fct_reorder(variable, estimate)
  )

p_nb6 <- ggplot(d_nb6, aes(x = estimate, y = variable, shape = significativo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 color = "#7570b3", linewidth = 0.9) +
  geom_point(color = "#7570b3", size = 3.5, fill = "white", stroke = 1.2) +
  geom_text(aes(x = conf.high, label = etiqueta),
            hjust = -0.08, vjust = -0.9, size = 3.3, color = "grey20") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "n.s."),
                     name = NULL) +
  scale_x_log10(breaks = c(0.5, 1, 2, 3, 5),
                labels = c(0.5, 1, 2, 3, 5),
                expand = expansion(mult = c(0.05, 0.45))) +
  labs(x = "Razón de tasas de incidencia (IRR, escala log) e IC 95%",
       y = NULL,
       title = "Modelo de mejor ajuste, temporada 25/26") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# Guardar figura individual
ggsave("forest_nb6_2526.png", p_nb6, width = 8, height = 3.8, dpi = 300)
ggsave("forest_nb6_2526.pdf", p_nb6, width = 8, height = 3.8)
