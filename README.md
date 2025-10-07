# T1_EU_SM_ST_CH
Taller 1 - Economía Urbana - Maestría PEG Uniandes

Este repositorio contiene las **bases de datos** y **scripts** del Taller 1. A continuación se exponen las instrucciones para **reproducir los resultados**.

> **Importante:** por restricciones de tamaño, **no** se incluye la base de **personas del Censo 2018** (`CNPV2018_5PER_A2_11.CSV`). Abajo se indica cómo obtenerla y dónde ubicarla.

---

## 1) Estructura del repositorio

```
.
├─ data/
│  ├─ Bogota/11Bogota/11_Bogota_CSV/ <-- (NO incluido)
│  │  ├─ CNPV2018_1VIV_A2_11.CSV
│  │  ├─ CNPV2018_2HOG_A2_11.CSV
│  │  ├─ CNPV2018_5PER_A2_11.CSV   
│  │  └─ CNPV2018_MGN_A2_11.CSV
│  ├─ Manzanas_CNPV/Manzanas_CNPV.shp (y archivos asociados)
│  ├─ UPZ/UPZ/TAprobacionNOfUPZ_2023.shp (y asociados)
│  ├─ processed/ (archivos intermedios)
│  └─ dataTaller01_Amenidades.Rds
├─ scripts/
│  ├─ 00_config.R
│  ├─ 01_data_cleaning.R
│  ├─ 02_gradients.R
│  └─ 03_espacio_abierto.R
└─ README.md         (este archivo)
```
---

## 2) Requisitos

- **R** (≥ 4.2) y paquetes: `sf`, `here`, `janitor`, `dplyr`, `readr`, `stringr`, `tidyr`, `tmap`, `osmdata`, `ggplot2` (y los que se cargan en `scripts/00_config.R`).

> Asegurarse de tener los drivers/GDAL apropiados para `sf` y que el sistema pueda leer shapefiles.

---

## 3) Insumos de datos (Censo — personas)

El archivo del **`CNPV2018`** (para Bogotá D.C.) **no se distribuye** aquí por su tamaño.

Aquí el link de la carpeta para descargar desde OneDrive:
https://uniandes-my.sharepoint.com/:f:/g/personal/mc_hernandezl12_uniandes_edu_co/EtHonpgvLu1Mp2X1_unmJY0B7tmQoqUDWoKvWs6cftmMKQ?e=x92fSq 

De lo contrario:

1. Descargar desde la fuente oficial (**DANE**).
2. Debe ubicarse en:
   ```
   data/Bogota/11Bogota/11_Bogota_CSV/CNPV2018_5PER_A2_11.CSV
   ```
3. Si el nombre o la ubicación cambian, ajustar la variable `path_censo_personas` en `scripts/01_data_cleaning.R`.

---

## 4) Orden de ejecución

> **Ejecutar cada script en este orden**. Los ejemplos usan `Rscript`, pero se pueden abrir en RStudio y correr por secciones. Las **salidas** (tablas/figuras) se guardan en `results/`.

### 4.1 `scripts/00_config.R` — Configuración base
- **Qué hace:** fija el directorio raíz con `here`, carga y verifica paquetes, configura opciones globales (CRS, semilla, etc.).
- **Cómo correr:**
  ```bash
  Rscript scripts/00_config.R
  ```

### 4.2 `scripts/01_data_cleaning.R` — Carga y limpieza de datos
- **Qué hace:**
  - Carga `dataTaller01_Amenidades.Rds` (oferta inmobiliaria), censos (`CNPV2018_*`), UPZ y manzanas.
  - Depura NAs/inconsistencias; calcula `price_m2_venta` y `rent_m2_mo`.
  - Corrige lat/long invertidos; asegura CRS WGS84 y recorte a Bogotá.
  - Asigna **UPZ** a cada propiedad (intersección espacial).
  - Construye **población por manzana** y **densidad (hab/km²)** con CNPV 2018.
  - Calcula proporciones de **venta vs arriendo** por UPZ.
  - Genera **estadísticas descriptivas** (precios, superficies, densidad, etc.).
- **Cómo correr:**
  ```bash
  Rscript scripts/01_data_cleaning.R
  ```
  

### 4.3 `scripts/02_gradients.R` — Estimación de gradientes
- **Qué hace:**
  - Define especificaciones econométricas (dependiente, regresores, efectos fijos).
  - Estima gradientes urbanos y robustez
  - Exporta **tablas de regresión** y figuras de apoyo
- **Cómo correr:**
  ```bash
  Rscript scripts/02_gradients.R
  ```
  
### 4.4 `scripts/03_espacio_abierto.R` — Parques y plazas (OSM)
- **Qué hace:**
  - Descarga/lee datos de **OpenStreetMap** (parques y plazas).
  - Repara geometrías y calcula **áreas**.
  - Produce mapas de **distribución** y **tamaño** de espacios abiertos.
  - Estima el efecto de diversas variables de cercanía a espacios abiertos sobre precios de venta y alquiler. 
- **Cómo correr:**
  ```bash
  Rscript scripts/03_espacio_abierto.R
  ```
---

## 7) Fuentes de datos y crédito

- **Censo 2018 (DANE)** — microdatos y marco de georreferenciación.
- **OpenStreetMap (OSM)** — parques y plazas; © OpenStreetMap contributors.
- **Shapefiles UPZ y Manzanas** — según insumos indicados en `data/`.

