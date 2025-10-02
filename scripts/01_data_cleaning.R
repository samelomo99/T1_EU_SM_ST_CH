# ============================================================
# Script_01: Carga y Limpieza de Datos
# ============================================================
# Objetivos:
# - Cargar las bases de bloque neón 
# - Explorar y limpiar datos (nombres, tipos, valores faltantes)
# - Generar dataset listo para análisis y modelación
# =============================================================================

# Cargar la configuración base
source(here::here("scripts", "00_Config.R"))

# Importamos los datos 


# Paths -- Cambiar para cada usuario corriendo el código

setwd("C:/Users/USUARIO/OneDrive - Universidad de los andes/Maestría/Economía Urbana/Taller 1")
path_censo_personas<- "Bogota/11Bogota/11_Bogota_CSV/CNPV2018_5PER_A2_11.CSV" 
path_censo_marco <- "Bogota/11Bogota/11_Bogota_CSV/CNPV2018_MGN_A2_11.CSV"
path_upz <- "UPZ/UPZ/TAprobacionNOfUPZ_2023.shp"
path_manzanas <- "Manzanas_CPNV/Manzanas_CNPV.shp"

# Igualar sistema de coordenadas para todo el código

crs_wgs <- 4326
crs_bog <- 3116 ## MAGNA SIRGAS de Bogotá


# Paquetes

if (!require("pacman")) install.packages("pacman")  # Instala pacman si falta
library(pacman)                                    # Activa pacman

# Cargar e instalar librer?as necesarias
pacman::p_load(
  tidyverse,    # Manipulaci?n y visualizaci?n de datos
  rio,          # Importar/exportar distintos formatos (Excel, Stata, etc.)
  viridis,      # Paletas de colores perceptualmente uniformes
  sf,           # Manejo de datos espaciales (shapefiles, geometr?as)
  osmdata,      # Descarga de datos desde OpenStreetMap
  raster,       # Datos raster (im?genes satelitales, elevaci?n, etc.)
  rasterVis,     # Visualizaci?n avanzada para raster
  data.table,
  janitor,
  rmapshaper,
  here, 
  stringr, 
  tmap
)

#Importar base de datos de propiedades y convertirla en tibble

propiedades <- import("dataTaller01_Amenidades.rds") %>% as_tibble()

head(propiedades)

# Filtramos datos sin NA en price y superficie, y sin ceros o negativos
propiedades <- propiedades %>% 
  filter(!is.na(price), !is.na(surface_total), !is.na(surface_covered)) %>% 
  filter(price > 0, surface_total > 0)

# Convertir coordenadas a numericas y crear objeto espacial (sf)
propiedades$lat <- as.numeric(propiedades$lat)
propiedades$lon <- as.numeric(propiedades$lon)

propiedades <- st_as_sf(propiedades, coords = c("lon", "lat"), crs = crs_wgs) 
## Si quisieramos pasarlas a MAGNA SIRGAS usaríamos tambipen %>% st_transform(crs_bog)

# Crear precio por metro cuadrado
propiedades <- propiedades %>%
  mutate(price_m2 = price / surface_total)




### Shapefile de UPZ (Actualizados a Junio 6, 2025)

upz <- st_read(path_upz) %>%
  clean_names() %>%
  st_transform(crs_wgs)

    ### Quitarle prefijo UPZ al código

upz <- upz %>%
  janitor::clean_names() %>%
  mutate(
    cod_upz = as.character(cod_upz),
    cod_upz = str_replace(str_to_upper(cod_upz), "^UPZ\\s*", "") %>% str_trim()) %>%
  st_transform(crs_wgs)

### Pegarle UPZ a propiedades

propiedades_upz <- st_join(
  propiedades,
  upz %>% dplyr::select(cod_upz, nombre_upz),
  join = st_within,
  left = TRUE
)

message(sprintf("UPZ asignada a %.1f%% de propiedades.",
                100*mean(!is.na(propiedades_upz$cod_upz))))

# Proporción por UPZ
propiedades_upz <- propiedades_upz %>%
  st_drop_geometry() %>%
  filter(!is.na(cod_upz), !is.na(operation)) %>%
  count(cod_upz, operation) %>%
  pivot_wider(names_from = operation, values_from = n, values_fill = 0) %>%
  mutate(
    modo = factor(operation, levels = c("venta","arriendo")),
    total        = venta + arriendo,
    prop_venta   = if_else(total > 0, venta/total, NA_real_),
    prop_arriendo= if_else(total > 0, arriendo/total, NA_real_)
  )

# Ejemplo: ver top UPZ por proporción de venta
prop_upz %>%
  arrange(desc(prop_venta)) %>%
  head(10) %>%
  print()




# Importar bases de datos de Censo

personas <- read_csv(path_censo_personas, show_col_types = FALSE) %>% 
  clean_names() %>%
  mutate(cod_encuestas = as.character(cod_encuestas))

marco <- import(path_censo_marco) %>% 
  clean_names() %>%
  mutate(cod_encuestas = as.character(cod_encuestas),
         cod_dane_anm = as.character(cod_dane_anm))
  
  ### Shapefile de Manzanas CNPV (Sacado de ArcGIS Online, filtrado para Bogotá D.C.)

manzanas <- sf::st_read(path_manzanas)  %>%
  janitor::clean_names() %>%
  sf::st_transform(crs_bog)


###### Procesamiento para pegar las bases de datos

# Población por encuesta en personas

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


    ### Verificamos que los valores que venían en el archivo de ArcGIS y los que calculamos desde personas coinciden

    manzanas_filtr <- manzanas %>% dplyr::select(mpio, uasect_urb, ua_manzana, id_unifica, sexo_total, mancodigo, pop_mzna, geometry)


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#### Crear un polígono para el Centro Internacional de Bogotá 
    bbox_centro <- c(xmin = -74.35, ymin = 4.611, xmax = -73.95, ymax = 4.63)
    
    ### Aproximamos el perímetro del Centro Internacional al polígono compuesto por 
      ## Hotel Tequendama al Sureste
      ## Torre Atrio Sur al Suroeste
      ## Hotel Centro Internacional al Noroeste
      ## Parqueadero Edificio Teusacá Ecopetrol al Noreste 
    
    # Descargar datos especificos
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
    
    # Extraer puntos de inter?s (POI)
    SE_punto <- Tequendama_SE$osm_points
    SO_punto <-  Atrio_SO$osm_points
    NO_punto <- Hotel_CI_NO$osm_points
    NE_punto <- Ecopetrol_NE$osm_points
    

    pts <- dplyr::bind_rows(
      dplyr::mutate(SE_punto, lugar = "Hotel Tequendama"), ## El Hotel tequendama téxnicamente está en la carrera décima pero, a esta altura de la ciudad la distancia entre la 10 y la séptima es muy pequeña. 
      dplyr::mutate(SO_punto,   lugar = "Torres Atrio"),
      dplyr::mutate(NO_punto,   lugar = "Hotel Centro Internacional"),
      dplyr::mutate(NE_punto, lugar = "Edificio Ecopetrol")
    ) |>
      sf::st_as_sf()
    
    pts <- pts %>%
      distinct(lugar, .keep_all = TRUE)
    
    
    



    
    # Asegurar WGS84 y ordenar por ángulo
    cxy <- st_coordinates(st_centroid(st_union(pts)))[1, ]
    xy  <- st_coordinates(st_geometry(pts))
    ord <- order(atan2(xy[,2]-cxy[2], xy[,1]-cxy[1]))
    coords_ord <- xy[ord, c("X","Y")]
    coords_closed <- rbind(coords_ord, coords_ord[1,])
    
    
    
    # Polígono y centroide y en 3116
    poly_ci <- sf::st_sfc(sf::st_polygon(list(coords_closed)), crs = crs_wgs)
    cent_ci      <- st_centroid(poly_ci)
    
    # Empaquetar como shapefile
    centro_internacional     <- sf::st_sf(name = "Centro Internacional", geometry = poly_ci)
    centro_internacional_ctr <- sf::st_sf(name = "Centroide CI",         geometry = cent_ci)
    
    # Verificar la creación del polígono

    tmap_mode("view")
    tm_basemap("OpenStreetMap") +
      tm_shape(poly_ci)  + tm_polygons(col = "red", alpha = 0.25, border.col = "red") +
      tm_shape(pts) + tm_symbols(size = 0.1, col = "blue", alpha = 1) +
      tm_view(basemaps = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"))
    
    
    
    
    
    
    
   #### Parques_OSM_Bog <- opq(bbox = bbox_bog) %>%
  #####    add_osm_feature(key = "leisure", value = "park") %>%
    ####  osmdata_sf()
    
    #restaurantes_OSM_Cali <- opq(bbox = bbox_cali) %>%
    # add_osm_feature(key = "amenity", value = "restaurant") %>%
    #osmdata_sf()
    
    
    
    
    
    
    





