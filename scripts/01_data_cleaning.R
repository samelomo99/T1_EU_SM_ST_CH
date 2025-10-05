# ============================================================
# Script_01: Carga y Limpieza de Datos
# ============================================================
# Objetivos:
# - Cargar las bases de bloque neón 
# - Explorar y limpiar datos (nombres, tipos, valores faltantes)
# - Generar dataset listo para análisis y modelación
# =============================================================================

rm(list = ls()) #limpiamos el entorno

# Cargar la configuración base
here::i_am("scripts/01_data_cleaning.R")
source(here::here("scripts", "00_Config.R"))

# Definir paths de los insumos de datos

path_censo_personas <- here::here("data", "Bogota", "11Bogota", "11_Bogota_CSV", "CNPV2018_5PER_A2_11.CSV")
path_censo_marco    <- here::here("data", "Bogota", "11Bogota", "11_Bogota_CSV", "CNPV2018_MGN_A2_11.CSV")
path_upz            <- here::here("data", "UPZ", "UPZ", "TAprobacionNOfUPZ_2023.shp")
path_manzanas       <- here::here("data", "Manzanas_CNPV", "Manzanas_CNPV.shp")
path_propiedades    <- here::here("data", "dataTaller01_Amenidades.Rds")

# Igualar sistema de coordenadas para todo el código
# Definir CRS de trabajo

crs_wgs <- 4326
crs_bog <- 3116 ## MAGNA SIRGAS de Bogotá

# -----------------------------------------------------------------
# Importar y limpiar base de propiedades
# -----------------------------------------------------------------

propiedades <- import(path_propiedades) %>% as_tibble()
head(propiedades)

# Limpiar datos de propiedades

bbox_bog <- c(xmin = -74.35, ymin = 4.45, xmax = -73.95, ymax = 4.85)

bbox_bog <- c(xmin = -74.35, ymin = 4.45, xmax = -73.95, ymax = 4.85)

propiedades <- propiedades %>%
  filter(!is.na(price), !is.na(surface_total), !is.na(surface_covered)) %>% # eliminar NAs
  filter(price > 0, surface_total > 0) %>% # eliminar valores inválidos
  mutate(
    lat  = as.numeric(lat),
    lon  = as.numeric(lon),
    price = as.numeric(price),
    surface_total  = as.numeric(surface_total),
    surface_covered= as.numeric(surface_covered),
    price_m2_venta = if_else(operation == "Venta",    price / surface_total, NA_real_),
    rent_m2_mo     = if_else(operation == "Alquiler", price / surface_total, NA_real_))  %>%
  # Detecta pares invertidos de longitudes y latitudes y los corrige
  mutate(
    swapped = dplyr::between(lon, 3, 6) & dplyr::between(lat, -75, -73),
    lat  = if_else(swapped, lon, lat),
    lon  = if_else(swapped, lat, lon)
  ) %>%
  mutate(
    in_bbox = dplyr::between(lon, bbox_bog["xmin"], bbox_bog["xmax"]) &
      dplyr::between(lat, bbox_bog["ymin"], bbox_bog["ymax"])
  )

# Convertir a objeto espacial

propiedades <- st_as_sf(propiedades, coords = c("lon", "lat"), crs = crs_wgs)

# Verificar errores de localización

message(sprintf("Total crudo: %d | Dentro bbox: %d | Fuera bbox: %d",
                nrow(propiedades), nrow(filter(propiedades, in_bbox)), nrow(filter(propiedades, !in_bbox))))

# -----------------------------------------------------------------------
# Shapefile de UPZ (Actualizados a Junio 6, 2025) y unión de propiedades
# -----------------------------------------------------------------------

upz <- st_read(path_upz) %>%
  clean_names() %>%
  mutate(
    cod_upz = str_replace(str_to_upper(as.character(cod_upz)), "^UPZ\\s*", "") %>%
      str_trim()
  ) %>%
  st_transform(crs_wgs)

# Pegar UPZ a propiedades

propiedades_upz <- st_join(
  propiedades,
  upz %>% dplyr::select(cod_upz, nombre_upz),
  join = st_within,
  left = TRUE
)

message(sprintf("UPZ asignada a %.1f%% de propiedades.",
                100*mean(!is.na(propiedades_upz$cod_upz))))

# Proporción por UPZ

propiedades_upz %>%
  st_drop_geometry() %>%
  count(operation) %>%
  print(n = Inf)


propiedades_upz <- propiedades_upz %>%
  st_drop_geometry() %>%
  filter(!is.na(cod_upz), !is.na(operation)) %>%
  mutate(operation = str_to_lower(operation)) %>%  # <- paso clave
  count(cod_upz, operation) %>%
  pivot_wider(names_from = operation, values_from = n, values_fill = 0) %>%
  mutate(
    total        = venta + alquiler,
    prop_venta   = if_else(total > 0, venta/total, NA_real_),
    prop_arriendo= if_else(total > 0, alquiler/total, NA_real_)
  )

# Ejemplo: ver top UPZ por proporción de venta
propiedades_upz %>%
  arrange(desc(prop_venta)) %>%
  head(10) %>%
  print()

# -----------------------------------------------------------------
# Importar bases de datos de Censo
# -----------------------------------------------------------------

personas <- read_csv(path_censo_personas, show_col_types = FALSE) %>% 
  clean_names() %>%
  mutate(cod_encuestas = as.character(cod_encuestas))

marco <- import(path_censo_marco) %>% 
  clean_names() %>%
  mutate(
    cod_encuestas = as.character(cod_encuestas),
    cod_dane_anm  = as.character(cod_dane_anm)
  )

# Shapefile de Manzanas CNPV (Sacado de ArcGIS Online, filtrado para Bogotá D.C.)

manzanas <- sf::st_read(path_manzanas) %>%
  clean_names() %>%
  st_transform(crs_bog)

# -----------------------------------------------------------------
# Procesamiento Censo: población por encuesta y manzana
# -----------------------------------------------------------------

pop_por_encuesta <- personas %>%
  filter(!is.na(cod_encuestas)) %>%
  count(cod_encuestas, name = "pop_enc")

# Pegamos población por encuesta con marco de georreferenciación

mgn <- marco  %>%
  arrange(cod_encuestas)  %>%
  distinct(cod_encuestas, .keep_all = TRUE)  %>% ## En el marco de georreferenciación pareciera haber una sola observación por encuesta pero por si acaso
  dplyr :: select(cod_encuestas, ua_clase, cod_dane_anm)  %>%
  left_join(pop_por_encuesta, by = "cod_encuestas")  %>%
  mutate(pop_enc = coalesce(pop_enc, 0L))  %>%
  filter(ua_clase == 1, !is.na(cod_dane_anm)) ## Nos quedamos solo con lo que es urbano dado que las UPZ son unidades urbanas

# Población por manzanas en marco

pop_mzna <- mgn  %>%
  group_by(cod_dane_anm)  %>%
  summarise(pop_mzna = sum(pop_enc, na.rm = TRUE), .groups = "drop") 

# Pegamos shapefile de manzanas con información de población por manzana

manzanas <- manzanas  %>%
  mutate(mancodigo = as.character(id_unifica))  %>%
  left_join(pop_mzna, by = c("id_unifica" = "cod_dane_anm"))  %>%
  mutate(pop_mzna = dplyr::coalesce(pop_mzna, 0))

# Verificamos que los valores que venían en el archivo de ArcGIS y los que calculamos desde personas coinciden

manzanas_filtr <- manzanas %>% dplyr::select(mpio, uasect_urb, ua_manzana, id_unifica, sexo_total, mancodigo, pop_mzna, geometry)
head(manzanas_filtr)

# -----------------------------------------------------------------
# Crear polígono del Centro Internacional de Bogotá
# -----------------------------------------------------------------

bbox_centro <- c(xmin = -74.35, ymin = 4.611, xmax = -73.95, ymax = 4.63)
    
# Aproximamos el perímetro del Centro Internacional al polígono compuesto por: 
  # Hotel Tequendama al Sureste
  # Torre Atrio Sur al Suroeste
  # Hotel Centro Internacional al Noroeste
  # Parqueadero Edificio Teusacá Ecopetrol al Noreste 
    
Tequendama_SE <- opq(bbox = bbox_centro) %>%
 add_osm_feature(key = "name", value = "Hotel Tequendama") %>%
 osmdata_sf()
    
Atrio_SO <- opq(bbox = bbox_centro) %>%
 add_osm_feature(key = "name", value = "Torre Atrio Sur") %>%
 osmdata_sf()
    
Hotel_CI_NO <- opq(bbox = bbox_centro) %>%
  add_osm_feature(key = "name", value = "Hotel Centro Internacional") %>%
  osmdata_sf()
    
Ecopetrol_NE <- opq(bbox = bbox_centro) %>%
  add_osm_feature(key = "name", value = "Parqueadero Ecopetrol S.A.") %>%
  osmdata_sf()
    
# Extraer puntos de interés (POI)

SE_punto <- Tequendama_SE$osm_points
SO_punto <-  Atrio_SO$osm_points
NO_punto <- Hotel_CI_NO$osm_points
NE_punto <- Ecopetrol_NE$osm_points

pts <- bind_rows(
  mutate(SE_punto, lugar = "Hotel Tequendama"),
  mutate(SO_punto, lugar = "Torres Atrio"),
  mutate(NO_punto, lugar = "Hotel Centro Internacional"),
  mutate(NE_punto, lugar = "Edificio Ecopetrol")
) %>%
  st_as_sf() %>%
  distinct(lugar, .keep_all = TRUE)
    
# Asegurar WGS84 y ordenar por ángulo

cxy <- st_coordinates(st_centroid(st_union(pts)))[1, ]
xy  <- st_coordinates(st_geometry(pts))
ord <- order(atan2(xy[,2]-cxy[2], xy[,1]-cxy[1]))
coords_ord <- xy[ord, c("X","Y")]
coords_closed <- rbind(coords_ord, coords_ord[1,])

# Polígono y centroide y en 3116

poly_ci <- st_sfc(st_polygon(list(coords_closed)), crs = crs_wgs)
cent_ci <- st_centroid(poly_ci)

# Empaquetar como shapefile

centro_internacional     <- st_sf(name = "Centro Internacional", geometry = poly_ci)
centro_internacional_ctr <- st_sf(name = "Centroide CI", geometry = cent_ci)

# Mapa de validación

tmap_mode("view")
tm_basemap("OpenStreetMap") +
  tm_shape(poly_ci) + tm_polygons(col = "red", alpha = 0.25, border.col = "red") +
  tm_shape(pts) + tm_symbols(size = 0.1, col = "blue", alpha = 1) +
  tm_view(basemaps = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"))

# -----------------------------------------------------------------
# Extraer Parques y Plazas desde OSM
# -----------------------------------------------------------------

bbox_bogota <- st_bbox(upz)

parques_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

plazas_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "place", value = "square") %>%
  osmdata_sf()

# Función auxiliar para limpiar geometrías inválidas
limpiar_geometrias <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
  
  # Intentar reparar geometrías inválidas
  sf_obj <- sf_obj %>%
    st_make_valid() %>%
    filter(st_is_valid(geometry)) %>%
    filter(!st_is_empty(geometry))
  
  return(sf_obj)
}

# Combinar polígonos y multipolígonos, limpiando geometrías

espacios_abiertos <- bind_rows(
  limpiar_geometrias(parques_osm$osm_polygons) %>% 
    mutate(tipo = "Parque") %>%
    dplyr::select(osm_id, name, tipo, geometry),
  limpiar_geometrias(parques_osm$osm_multipolygons) %>% 
    mutate(tipo = "Parque") %>%
    dplyr::select(osm_id, name, tipo, geometry),
  limpiar_geometrias(plazas_osm$osm_polygons) %>% 
    mutate(tipo = "Plaza") %>%
    dplyr::select(osm_id, name, tipo, geometry),
  limpiar_geometrias(plazas_osm$osm_multipolygons) %>% 
    mutate(tipo = "Plaza") %>%
    dplyr::select(osm_id, name, tipo, geometry)
) %>%
  filter(!is.null(geometry)) %>%
  st_transform(crs_wgs)

espacios_abiertos <- espacios_abiertos %>%
  mutate(area_m2 = as.numeric(st_area(geometry)))

# -----------------------------------------------------------------
# Estadísticas Descriptivas
# -----------------------------------------------------------------

espacios_abiertos %>%
  st_drop_geometry() %>%
  group_by(tipo) %>%
  summarise(
    n = n(),
    area_total_km2 = sum(area_m2, na.rm = TRUE) / 1e6,
    area_media_m2 = mean(area_m2, na.rm = TRUE),
    area_mediana_m2 = median(area_m2, na.rm = TRUE)
  ) %>%
  print()


stats_propiedades <- propiedades %>%
  st_drop_geometry() %>%
  summarise(
    n = n(),
    across(
      c(price, price_m2, surface_total, surface_covered),
      list(
        media = ~mean(., na.rm = TRUE),
        desv_est = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        p25 = ~quantile(., 0.25, na.rm = TRUE),
        mediana = ~median(., na.rm = TRUE),
        p75 = ~quantile(., 0.75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

print(stats_propiedades)

tabla_operaciones <- propiedades %>%
  st_drop_geometry() %>%
  count(operation) %>%
  mutate(porcentaje = 100 * n / sum(n))

print(tabla_operaciones)

# -----------------------------------------------------------------
# Densidad Poblacional
# -----------------------------------------------------------------

# Calcular área y densidad por manzana

manzanas <- manzanas %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    densidad_hab_km2 = pop_mzna / area_km2
  )

# Estadísticas de densidad

stats_densidad <- manzanas %>%
  st_drop_geometry() %>%
  summarise(
    media_pop = mean(pop_mzna, na.rm = TRUE),
    media_dens = mean(densidad_hab_km2, na.rm = TRUE),
    sd_dens = sd(densidad_hab_km2, na.rm = TRUE),
    min_dens = min(densidad_hab_km2, na.rm = TRUE),
    mediana_dens = median(densidad_hab_km2, na.rm = TRUE),
    max_dens = max(densidad_hab_km2, na.rm = TRUE)
  )

print(stats_densidad)

# Validar duplicados

upz <- upz %>%
  group_by(cod_upz, nombre_upz) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  st_cast("MULTIPOLYGON")

# Agregar densidad a nivel UPZ

manzanas_upz <- st_join(
  manzanas %>% st_transform(crs_wgs),
  upz %>% dplyr::select(cod_upz, nombre_upz),
  join = st_within
)

densidad_upz <- manzanas_upz %>%
  st_drop_geometry() %>%
  group_by(cod_upz, nombre_upz) %>%
  summarise(
    poblacion_total = sum(pop_mzna, na.rm = TRUE),
    area_total_km2 = sum(area_km2, na.rm = TRUE),
    densidad_promedio = poblacion_total / area_total_km2,
    .groups = "drop"
  )

# Unir con UPZ

upz <- upz %>%
  left_join(densidad_upz, by = c("cod_upz", "nombre_upz")) %>%
  left_join(propiedades_upz, by = "cod_upz")

# -----------------------------------------------------------------
# MAPAS
# -----------------------------------------------------------------

# Mapa _ Proporción de propiedades en venta por UPZ

mapa_prop_venta <- ggplot() +
  geom_sf(data = upz, aes(fill = prop_venta), color = "white", size = 0.2) +
  geom_sf(data = centro_internacional_ctr, color = "red", size = 3, shape = 17) +
  scale_fill_viridis_c(
    name = "Proporción\nVenta",
    option = "C",
    na.value = "grey80",
    labels = scales::percent_format()
  ) +
  labs(
    title = "Proporción de Propiedades en Venta por UPZ",
    subtitle = "Centro Internacional marcado en rojo",
    caption = "Fuente:"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(mapa_prop_venta)

# Mapa _ Densidad poblacional por UPZ
mapa_densidad <- ggplot() +
  geom_sf(data = upz, aes(fill = densidad_promedio), color = "white", size = 0.2) +
  geom_sf(data = centro_internacional_ctr, color = "red", size = 3, shape = 17) +
  scale_fill_viridis_c(
    name = "Densidad\n(hab/km²)",
    option = "D",
    na.value = "grey80",
    trans = "log10",
    labels = scales::comma_format()
  ) +
  labs(
    title = "Densidad Poblacional por UPZ",
    subtitle = "Escala logarítmica - Centro Internacional en rojo",
    caption = "Fuente: Censo 2018 DANE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(mapa_densidad)

# Mapa _ Ubicación de parques y plazas

mapa_espacios <- ggplot() +
  geom_sf(data = upz, fill = "grey95", color = "grey80", size = 0.2) +
  geom_sf(data = espacios_abiertos, aes(fill = tipo), alpha = 0.6, color = NA) +
  geom_sf(data = centro_internacional_ctr, color = "red", size = 3, shape = 17) +
  scale_fill_manual(
    name = "Tipo",
    values = c("Parque" = "forestgreen", "Plaza" = "orange")
  ) +
  labs(
    title = "Distribución de Parques y Plazas en Bogotá",
    subtitle = "Centro Internacional en rojo",
    caption = "Fuente: OpenStreetMap"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(mapa_espacios)

# Mapa _ Tamaño de espacios abiertos



# -----------------------------------------------------------------
# Guardar bases 
# -----------------------------------------------------------------

dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

fp_gpkg        <- here::here("data", "processed", "bogota_processed.gpkg")
fp_prop_upz_rds<- here::here("data", "processed", "propiedades_upz.rds")
fp_prop_upz_csv<- here::here("data", "processed", "propiedades_upz.csv")
fp_dens_upz_rds<- here::here("data", "processed", "densidad_upz.rds")
fp_dens_upz_csv<- here::here("data", "processed", "densidad_upz.csv")
fp_pop_mzna_rds<- here::here("data", "processed", "pop_mzna.rds")
fp_pop_mzna_csv<- here::here("data", "processed", "pop_mzna.csv")

# UPZ
  st_write(upz, fp_gpkg, layer = "upz", delete_dsn = TRUE, quiet = TRUE)

# Manzanas
  st_write(manzanas, fp_gpkg, layer = "manzanas", append = TRUE, quiet = TRUE)

# propiedades (o propiedades_limpias si la tienes)
  st_write(propiedades, fp_gpkg, layer = "propiedades", append = TRUE, quiet = TRUE)

# espacios abiertos
  st_write(espacios_abiertos, fp_gpkg, layer = "espacios_abiertos", append = TRUE, quiet = TRUE)

# polígono y centroide del CI
  st_write(centro_internacional, fp_gpkg, layer = "ci_poly", append = TRUE, quiet = TRUE)
  st_write(centro_internacional_ctr, fp_gpkg, layer = "ci_centroid", append = TRUE, quiet = TRUE)

# Tablas a CSV
  write_rds(propiedades_upz, fp_prop_upz_rds); write_csv(propiedades_upz, fp_prop_upz_csv)
  write_rds(densidad_upz, fp_dens_upz_rds); write_csv(densidad_upz, fp_dens_upz_csv)
  write_rds(pop_mzna, fp_pop_mzna_rds); write_csv(pop_mzna, fp_pop_mzna_csv)
