# Vector Management Tucumán

Repositorio dedicado al análisis espacial, modelado estadístico y visualización de la dinámica poblacional de *Aedes aegypti* y su relación con variables ambientales y epidemiológicas en Tucumán, Argentina.

## 📌 Estructura del Repositorio

El proyecto se encuentra organizado en las siguientes carpetas funcionales:

* **`01_spatial_risk_mapping/`**: Procesamiento SIG de capas vectoriales y ráster (basurales, espacios verdes, NDVI, hacinamiento), cálculo de distancias euclidianas, modelado espacial de abundancia e integración con datos epidemiológicos (dengue y COVID-19).
* **`02_model_plots/`**: Generación de *forest plots* y visualización gráfica de las Razones de Tasas de Incidencia (IRR) e intervalos de confianza para los modelos ajustados.
* **`03_oviposition_glmm_models/`**: Ajuste de Modelos Lineales Generalizados Mixtos (GLMM con distribución binomial negativa) para evaluar la abundancia de oviposición en las temporadas 2023/2024, 2024/2025 y 2026.

## 🛠️ Requisitos e Instalación

* **Lenguaje:** R (versión >= 4.2.0)
* **Entorno:** RStudio / R Projects
* **Paquetes principales:** `sf`, `terra`, `tidyverse`, `lme4`, `broom.mixed`

## 👤 Autora
* **Mariana Baricco**
