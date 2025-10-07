# ============================================================
# Script_03: Distancia de Espacios Abiertos
# ============================================================
# Objetivos:
# - Operacionalizar definiciones de cercanía a espacio abierto
# - Estimar efecto de cercanía sobre precios por metro cuadrado (alquiler y venta)
# =============================================================

here::i_am("scripts/03_espacio_abierto.R")
source(here::here("scripts", "00_Config.R"))

fp_gpkg <- here::here("data","processed","bogota_processed.gpkg")

propiedades <- st_read(fp_gpkg, layer = "props_grad",         quiet = TRUE)
upz          <- st_read(fp_gpkg, layer = "upz",               quiet = TRUE)
espacios_abiertos <- st_read(fp_gpkg, layer = "espacios_abiertos", quiet = TRUE)
cent_ci      <- st_read(fp_gpkg, layer = "ci_centroid", quiet = TRUE)

# Asegurar CRS y mínimos
crs_bog <- 3116
propiedades <- propiedades %>% st_transform(crs_bog)
espacios_abiertos <- espacios_abiertos %>% st_make_valid() %>% st_transform(crs_bog) %>%
  mutate(area_m2 = as.numeric(sf::st_area(sf::st_geometry(.))))

# Parámetros
crs_bog     <- 3116
FE_VAR      <- "cod_upz"
SFD_CALIPER <- 300      # metros para SFD
CONLEY_KM   <- 5        # km para EE Conley

propiedades <- propiedades %>% filter(!is.na(operation), !is.na(cod_upz)) %>% 
  mutate(cod_upz = factor(cod_upz),
         y_log_m2 = log(price_m2))

# --- Centroides de parques y plazas ---
ea_cent <- st_centroid(espacios_abiertos)  # puntos para medir distancia

# -----------------------------------------------------------------
# Variables de distancia al espacio abierto 
# -----------------------------------------------------------------

### Tenemos varias variables que miden la cercanía a los espacios abiertos
### En primer lugar utilizamos la distancia a espacios abiertos como variable continua
### También usamos una binaria que indica si la propiedad está a menos de 300 metros de un espacio abierto de área mayor a una hectárea. 
### Finalmente usamos una binaria que indica si la propiedad está a menos de 800 metros de un espacio abierto de área mayor a una hectárea

# Distancia mínima (m) a cualquier centroide de espacio abierto
idx_min  <- st_nearest_feature(propiedades, ea_cent)
propiedades$dist_abiertos <- as.numeric(st_distance(propiedades, ea_cent[idx_min, ], by_element = TRUE))
propiedades$l_dist_abiert <- log1p(propiedades$dist_abiertos)

# Indicador ≤ 300 m al centroide de EA con 1 ha o más
abiertos_1ha <- ea_cent[espacios_abiertos$area_m2 >= 1e4, ]
near300_list <- st_is_within_distance(propiedades, abiertos_1ha, dist = 300)
propiedades$near300 <- lengths(near300_list) > 0

# Indicador ≤ 800 m al centroide de EA con 1 ha o más
near800_list <- st_is_within_distance(propiedades, abiertos_1ha, dist = 800)
propiedades$near800 <- lengths(near800_list) > 0

# Chequeo de variación
propiedades %>% st_drop_geometry() %>%
  summarise(
    N = n(),
    dist_m_p50 = median(dist_abiertos, na.rm = TRUE),
    p_near300  = mean(near300, na.rm = TRUE),
    p_near800  = mean(near800, na.rm = TRUE)
  )

### La gran mayoría de las propiedades tienen un centroide de un espacio abierto 1 ha o más a 800 metros o menos. 


# --- Dependiente y controles ---

# Data frames por mercado (mantén id_prop para alinear pares/coords)
sale <- propiedades %>% filter(operation == "Venta")    %>% st_drop_geometry()
rent <- propiedades %>% filter(operation == "Alquiler") %>% st_drop_geometry()

# Controles
rhs_ctrl <- paste(
  "bs(dist_ci_km, df=4)", "ln_st + ln_sc", "bathrooms", "rooms",
  sep = " + " )
rhs_ctrl <- gsub("\\+ \\+", "+", rhs_ctrl)

# Volver a traer longitud y latitud para el estimador con errores Conley

sale_coords <- propiedades %>%
  dplyr::filter(operation == "Venta") %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(id_prop, lon, lat)

rent_coords <- propiedades %>%
  dplyr::filter(operation == "Alquiler") %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(id_prop, lon, lat)

sale <- dplyr::left_join(sale, sale_coords, by = "id_prop")
rent <- dplyr::left_join(rent, rent_coords, by = "id_prop")

# ============================================================
# 1) OLS con FE por UPZ (cluster UPZ)
# ============================================================

## Creamos lista con las diferentes medidas de cercanía. 
treats <- c("l_dist_abiert", "near300", "near800")

# --- Venta ---
for (tv in treats) {
  f_venta <- as.formula(paste0("y_log_m2 ~ ", tv, " + ", rhs_ctrl, " | ", FE_VAR))
  obj <- paste0("m_sale_fe_", tv)
  assign(obj, feols(f_venta, data = sale, cluster = ~ cod_upz))
}

# --- Alquiler ---
for (tv in treats) {
  f_alq <- as.formula(paste0("y_log_m2 ~ ", tv, " + ", rhs_ctrl, " | ", FE_VAR))
  obj <- paste0("m_rent_fe_", tv)   # p.ej. m_rent_fe_near300
  assign(obj, feols(f_alq, data = rent, cluster = ~ cod_upz))
}


etable(list(m_rent_fe_l_dist_abiert, m_rent_fe_near300, m_rent_fe_near800),
       headers = c("log dist", "≤300 m (≥1 ha)", "≤800 m"))

# ============================================================
# 2) DIFERENCIAS ESPACIALES (SFD)
#    (pares dentro de la MISMA UPZ)
# ============================================================

# --- Venta ---
props_sale_sf <- propiedades %>% filter(operation == "Venta") %>% arrange(id_prop)
coords_sale   <- st_coordinates(props_sale_sf)
knn_sale      <- FNN::get.knn(coords_sale, k = 1)
pares_sale <- tibble::tibble(
  i = seq_len(nrow(props_sale_sf)),
  j = knn_sale$nn.index[,1],
  d = knn_sale$nn.dist[,1],
  upz_i = props_sale_sf$cod_upz,
  upz_j = props_sale_sf$cod_upz[knn_sale$nn.index[,1]]
) %>%
  filter(d <= SFD_CALIPER, i < j, upz_i == upz_j)  # caliper y misma UPZ

if (nrow(pares_sale) == 0) message("SFD venta: no hay pares dentro del caliper y misma UPZ.")

# --- Alquiler ---
props_rent_sf <- propiedades %>% filter(operation == "Alquiler") %>% arrange(id_prop)
coords_rent   <- st_coordinates(props_rent_sf)
knn_rent      <- FNN::get.knn(coords_rent, k = 1)
pares_rent <- tibble::tibble(
  i = seq_len(nrow(props_rent_sf)),
  j = knn_rent$nn.index[,1],
  d = knn_rent$nn.dist[,1],
  upz_i = props_rent_sf$cod_upz,
  upz_j = props_rent_sf$cod_upz[knn_rent$nn.index[,1]]
) %>%
  filter(d <= SFD_CALIPER, i < j, upz_i == upz_j)

if (nrow(pares_rent) == 0) message("SFD alquiler: no hay pares dentro del caliper y misma UPZ.")

# Estimar por cada tratamiento
for (tv in treats) {
  # Venta
  df <- sale %>% arrange(id_prop)
  dy  <- df$y_log_m2[pares_sale$i] - df$y_log_m2[pares_sale$j]
  dlns<- df$ln_st[pares_sale$i]    - df$ln_st[pares_sale$j]
  dlnc<- df$ln_sc[pares_sale$i]    - df$ln_sc[pares_sale$j]
  ddc <- df$dist_ci_km[pares_sale$i]-df$dist_ci_km[pares_sale$j]
  dtv <- df[[tv]][pares_sale$i]    - df[[tv]][pares_sale$j]
  datos <- data.frame(dy = dy, dtv = dtv, dlns = dlns, dlnc = dlnc, ddc = ddc)
  cat("\n[VENTA][SFD] Tratamiento:", tv, " | N pares:", nrow(datos), "\n")
  m_sfd_sale <- feols(dy ~ dtv + dlns + dlnc + ddc, data = datos)
  print(etable(m_sfd_sale))
  
  # Alquiler
  if (nrow(pares_rent) > 0) {
    df <- rent %>% arrange(id_prop)
    dy  <- df$y_log_m2[pares_rent$i] - df$y_log_m2[pares_rent$j]
    dlns<- df$ln_st[pares_rent$i]    - df$ln_st[pares_rent$j]
    dlnc<- df$ln_sc[pares_rent$i]    - df$ln_sc[pares_rent$j]
    ddc <- df$dist_ci_km[pares_rent$i]-df$dist_ci_km[pares_rent$j]
    dtv <- df[[tv]][pares_rent$i]    - df[[tv]][pares_rent$j]
    datos <- data.frame(dy = dy, dtv = dtv, dlns = dlns, dlnc = dlnc, ddc = ddc)
    cat("\n[ALQUILER][SFD] Tratamiento:", tv, " | N pares:", nrow(datos), "\n")
    m_sfd_rent <- feols(dy ~ dtv + dlns + dlnc + ddc, data = datos)
    print(etable(m_sfd_rent))
  }
}



# 3) var-cov Conley (kernel Bartlett, 5 km)
model_names <- c("m_rent_fe_l_dist_abiert","m_rent_fe_near300","m_rent_fe_near800",
                 "m_sale_fe_l_dist_abiert","m_sale_fe_near300","m_sale_fe_near800")

mods <- mget(model_names, inherits = TRUE)
datas <- list(rent, rent, rent,
              sale, sale, sale)

vcs <- Map(function(mod, df)
  vcov_conley(mod, lon = "lon" , lat = "lat", cutoff = 5),
  mods, datas)

etable(mods, vcov = vcs)







