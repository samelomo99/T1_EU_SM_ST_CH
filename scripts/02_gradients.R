# ============================================================
# Script_02: Gradientes espaciales (precio m² y densidad)
# ============================================================
# Objetivos:
# - Calcular distancia de propiedades al Centro Internacional
# - Estimar gradientes de precio y densidad vs distancia
# - Graficar los gradientes espaciales
# =============================================================

# Config y paquetes
here::i_am("scripts/02_gradients.R")
source(here::here("scripts", "00_Config.R"))

# Paths a procesados
fp_gpkg       <- here::here("data", "processed", "bogota_processed.gpkg")

# Definir CRS de trabajo

crs_wgs <- 4326
crs_bog <- 3116 ## MAGNA SIRGAS de Bogotá

# Cargar capas necesarias
upz                      <- st_read(fp_gpkg, layer = "upz",           quiet = TRUE)
manzanas                 <- st_read(fp_gpkg, layer = "manzanas",      quiet = TRUE)
propiedades              <- st_read(fp_gpkg, layer = "propiedades",   quiet = TRUE)
centro_internacional_ctr <- st_read(fp_gpkg, layer = "ci_centroid",   quiet = TRUE)

# --- Generar variables para la estimación ---

centro_internacional_ctr <- st_transform(centro_internacional_ctr, 3116)
propiedades    <- st_transform(propiedades, 3116)

props <- propiedades %>%
  mutate(
    dist_ci_km = as.numeric(st_distance(., centro_internacional_ctr))/ 1000,
    ln_st = log(surface_total),
    ln_sc = log(pmax(1, surface_covered))
  ) %>%
  # pega UPZ 
  st_transform(4326) %>%
  st_join(st_transform(upz, 4326) %>% dplyr::select(cod_upz), join = st_within, left = TRUE) %>%
  st_transform(crs_bog) %>%
  filter(!is.na(cod_upz)) %>%
  mutate(cod_upz = factor(cod_upz))

# Verificar rango de distancias
summary(props$dist_ci_km)

# Verificamos variación en los controles 
sum(props$rooms != props$bedrooms, na.rm = TRUE)
sum(props$surface_total != props$surface_covered, na.rm= TRUE)
  # Como vemos que no hay prácticamente diferencias a lo largo del data frame entre las variables "rooms" y "bedrooms" elegimos una sola para las regresiones para evitar colinealidad. 

# Data frames separados
sale  <- props %>% filter(operation == "Venta") %>% rename(price_m2 = price_m2_venta)
rent  <- props %>% filter(operation == "Alquiler") %>% rename(price_m2 = rent_m2_mo)


# -----------------------------------------------------------------
# Gradiente Precio distancia
# -----------------------------------------------------------------

  ### Primero estimamos asumiendo la forma de la relación entre la distancia y el precio (lineal)

# --- VENTAS ---

m_sale_lin <- gam(
  log(price_m2) ~
    dist_ci_km +                     # gradiente vs distancia
    ln_st + ln_sc +                  # tamaños
    bathrooms + rooms +   # controles
    s(cod_upz, bs = "re"),           # efecto aleatorio UPZ
  data = sale, method = "REML"
)

# --- ALQUILER ---
m_rent_lin <- gam(
  log(price_m2) ~
    dist_ci_km+
    ln_st+ ln_sc +
    bathrooms + rooms +   
    s(cod_upz, bs = "re"),
  data = rent, method = "REML"
)

summary(m_sale_lin)
summary(m_rent_lin)


  ### Luego damos espacio para no linealidades en la relación entre precio y distancia

# --- VENTAS ---

m_sale <- gam(
  log(price_m2) ~
    s(dist_ci_km, bs="ad", k=30) +           # gradiente (curva) vs distancia
    ln_st + ln_sc +                  # tamaños
    bathrooms + rooms +   # lineales
    s(cod_upz, bs = "re"),           # efecto aleatorio UPZ
  data = sale, method = "REML"
)

# --- ALQUILER ---
m_rent <- gam(
  log(price_m2) ~
    s(dist_ci_km, bs="ad", k=30) +
    ln_st + ln_sc +                  # tamaños
    bathrooms + rooms +   # lineales
    s(cod_upz, bs = "re"),
  data = rent, method = "REML"
)


summary(m_sale)
summary(m_rent)

gam.check(m_sale)  # chequeo de k
gam.check(m_rent)

  ### Ahora estimamos por medio de efectos fijos por UPZ

# VENTA
m_sale_fe <- feols(
  log(price_m2) ~ dist_ci_km + log(surface_total) + log(pmax(1, surface_covered)) +
    bathrooms + rooms | cod_upz,
  data = sale,
)

# ALQUILER
m_rent_fe <- feols(
  log(price_m2) ~ dist_ci_km + log(surface_total) + log(pmax(1, surface_covered)) +
    bathrooms + rooms | cod_upz,
  data = rent,
)

etable(m_sale_fe, m_rent_fe)


# Gráficos de gradientes

  ### No incluimos un gráfico para la estimación por efectos fijos pues es muy similar a la lineal. 

p_venta   <- ggpredict(m_sale, terms = "dist_ci_km", ci.lvl = 0.95)  # predicciones + IC
p_alquiler<- ggpredict(m_rent, terms = "dist_ci_km", ci.lvl = 0.95)

plot(p_venta)    + labs(x="Distancia al CI (km)", y="Precio predicho (log-COP)", title="Venta: gradiente con IC")
plot(p_alquiler) + labs(x="Distancia al CI (km)", y="Precio predicho (log-COP)", title="Alquiler: gradiente con IC")

p_venta   <- ggpredict(m_sale_lin, terms = "dist_ci_km", ci.lvl = 0.95)  # predicciones + IC
p_alquiler<- ggpredict(m_rent_lin, terms = "dist_ci_km", ci.lvl = 0.95)

plot(p_venta)    + labs(x="Distancia al CI (km)", y="Precio predicho (log-COP)", title="Venta: gradiente con IC")
plot(p_alquiler) + labs(x="Distancia al CI (km)", y="Precio predicho (log-COP)", title="Alquiler: gradiente con IC")

# -----------------------------------------------------------------
# Gradiente Densidad poblacional distancia
# -----------------------------------------------------------------


mzn <- st_transform(manzanas, crs_bog) %>%
  mutate(
    dist_ci_km = as.numeric(st_distance(., centro_internacional_ctr)) / 1000
  )

# pega UPZ (join espacial) para FE/RE
mzn_w_upz <- mzn %>%
  st_transform(crs_bog) %>%
  st_join(st_transform(upz, crs_bog) %>% dplyr::select(cod_upz),
          join = st_within, left = TRUE) %>%
  filter(!is.na(cod_upz)) %>%
  mutate(
    cod_upz = factor(cod_upz),
    # log-densidad (evita log(0))
    ln_dens = log(pmax(1, densidad_hab_km2))
  )

# Verificar valores de distancia 
summary(mzn_w_upz$dist_ci_km)

df_den <- st_drop_geometry(mzn_w_upz) %>%
  filter(is.finite(dist_ci_km), is.finite(ln_dens))

# Modelo lineal
m_den_lin <- gam(
  ln_dens ~ dist_ci_km + s(cod_upz, bs = "re") + s(area_km2, k=4),
  data = df_den, method = "REML"
)

# Modelo no lineal
m_den_gam <- gam(ln_dens ~ s(dist_ci_km, k=5) + s(area_km2, k=4) + s(cod_upz, bs="re"),
                 data = df_den, method = "REML")

summary(m_den_lin)
summary(m_den_gam)
gam.check(m_den_gam)  # chequeo de k

# OLS con FE por UPZ
m_den_fe <- feols(
  ln_dens ~ dist_ci_km + area_km2 | cod_upz,
  data = df_den,
  cluster = ~ cod_upz
)

etable(m_den_fe)


# Curva predicha con IC 95%

  # No incluimos gráficos de Efectos Fijos por falta de significancia estadística

grad_den_lin   <- ggpredict(m_den_lin, terms = "dist_ci_km", ci.lvl = 0.95)  # predicciones + IC
grad_den_gam   <- ggpredict(m_den_gam, terms = "dist_ci_km", ci.lvl = 0.95)

plot(grad_den_lin)    + labs(x="Distancia al CI (km)", y="Densidad poblacional predicha (hab/km²)", title="Densidad: gradiente con IC")
plot(grad_den_gam) + labs(x="Distancia al CI (km)", y="Densidad poblacional predicha (hab/km²)", title="Densidad: gradiente con IC")

# -----------------------------------------------------------------
# Exportar nuevas bases
# -----------------------------------------------------------------

props <- props %>%
  st_transform(crs_bog)

props <- props %>%
  mutate(.,
  id_prop = dplyr::row_number()) %>%
  filter(!is.na(operation)) %>%
  dplyr::select(
    id_prop, operation, price_m2_venta, rent_m2_mo,
    dist_ci_km, ln_st, ln_sc,
    bedrooms, bathrooms, rooms,
    cod_upz, geom
  )

mzn_w_upz <- mzn_w_upz %>%
  st_transform(crs_bog)

# Actualizar capas en el GPKG
# - props_grad: propiedades con dist_ci_km, price_m2, controles y FE listos (EPSG:3116)
# - mzn_grad: manzanas con dist_ci_km, area_km2, densidad y ln_dens (EPSG:3116)
st_write(props,     fp_gpkg, layer = "props_grad", delete_layer = TRUE, quiet = TRUE)
st_write(mzn_w_upz,fp_gpkg, layer = "mzn_grad",  delete_layer = TRUE, quiet = TRUE)
