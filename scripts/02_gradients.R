# ============================================================
# Script_02: Gradientes Espaciales
# ============================================================
# Objetivos:
# - Calcular distancia de propiedades al Centro Internacional
# - Estimar gradientes de precio y densidad vs distancia
# - Graficar los gradientes espaciales
# =============================================================================

# Cargar la configuración base
source(here::here("scripts", "00_Config.R"))

# Cargar los datos procesados del Script_01
load(here::here("stores", "datos_completos.RData"))

# -----------------------------------------------------------------
# 1. Calcular distancia al Centro Internacional
# -----------------------------------------------------------------

# Transformar propiedades y centro al mismo CRS proyectado (para distancias en metros)

propiedades_bog <- propiedades %>% st_transform(crs_bog)
centro_bog <- centro_internacional_ctr %>% st_transform(crs_bog)

# Calcular distancia de cada propiedad al centro (en metros)

propiedades_bog <- propiedades_bog %>%
  mutate(
    dist_centro_m = as.numeric(st_distance(geometry, centro_bog)),
    dist_centro_km = dist_centro_m / 1000  # Convertir a kilómetros
  )

# Verificar rango de distancias

summary(propiedades_bog$dist_centro_km)

# Separar por tipo de operación

propiedades_venta <- propiedades_bog %>%
  filter(str_to_lower(operation) == "venta")

propiedades_arriendo <- propiedades_bog %>%
  filter(str_to_lower(operation) == "alquiler" | str_to_lower(operation) == "arriendo")

# -----------------------------------------------------------------
# 2. Densidad poblacional por UPZ con distancia al centro
# -----------------------------------------------------------------

# Calcular centroide de cada UPZ y su distancia al centro

upz_bog <- upz %>% 
  st_transform(crs_bog) %>%
  mutate(
    centroide = st_centroid(geometry),
    dist_centro_km = as.numeric(st_distance(centroide, centro_bog)) / 1000
  )

# Crear dataframe para análisis

datos_densidad <- upz_bog %>%
  st_drop_geometry() %>%
  filter(!is.na(densidad_promedio), densidad_promedio > 0)

# -----------------------------------------------------------------
# 3. Estimación de Gradientes
# -----------------------------------------------------------------

# Forma funcional: log-log (elasticidad constante)

# Gradiente de precio de venta

modelo_venta <- lm(log(price_m2) ~ log(dist_centro_km), 
                   data = propiedades_venta %>% 
                     filter(price_m2 > 0, dist_centro_km > 0))

summary(modelo_venta)

# Gradiente de precio de arriendo

modelo_arriendo <- lm(log(price_m2) ~ log(dist_centro_km), 
                      data = propiedades_arriendo %>% 
                        filter(price_m2 > 0, dist_centro_km > 0))

summary(modelo_arriendo)

# Gradiente de densidad poblacional

modelo_densidad <- lm(log(densidad_promedio) ~ log(dist_centro_km), 
                      data = datos_densidad %>%
                        filter(dist_centro_km > 0))

summary(modelo_densidad)



