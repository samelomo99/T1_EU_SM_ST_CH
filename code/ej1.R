## --- 
# Universidad de los Andes
# Taller 1 - Economía Urbana
# Grupo 1 - Santiago Melo - Sara Torres - Corina Hernández
# ---

rm(list = ls())
require("pacman") #pacman facilita la carga e instalaci?n simult?nea de librer?as

p_load(
  tidyverse,
  modeldata,
  stargazer,
  broom,
  fixest,
  dplyr,
  summarytools,
  DataExplorer
)

setwd("C:/Users/samel/OneDrive/Datos adjuntos/Universidad de los Andes/V/Economía Urbana/Talleres/Taller 1/T1_EU_SM_ST_CH")

# Cargamos los datos
data_ej1 <- readRDS("data/dataTaller01_PriceIndeces.rds")
head(data_ej1)

## --- LIMPIEZA Y CREACIÓN DE VARIABLES --- ## ----

# Elección de variables #
data_ej1 <- data_ej1 %>% 
  dplyr::select(pin, year, sale_price, 
                township_code, class, year_built, 
                building_sqft, land_sqft, num_bedrooms, 
                num_full_baths, num_fireplaces, type_of_residence, 
                construction_quality, attic_finish, garage_attached, 
                garage_area_included, garage_size, basement_type, 
                roof_material, renovation, recent_renovation, porch,
                central_air)

# Descriptivas y tratamiento datos #
summary(data_ej1)
glimpse(data_ej1)

# NA Values
data_ej1_na <- data_ej1 %>% select_if(~ sum(is.na(.)) > 0) # Filtra solo variables con NA
plot_missing(data_ej1_na)

# Como land_sqft tiene tantos NA, lo mejor es dropear la variable
sum(is.na(data_ej1$land_sqft))
data_ej1 <- data_ej1 %>%
  select(-land_sqft)

# Además de eso, consideramos que hay NA  en algunas variables categóricas que
# tienen el dato en blank y por eso no se ven en la gráfica.
data.frame(table(data_ej1$roof_material)) %>%
  arrange(desc(Freq)) # ático terminado
data.frame(table(data_ej1$type_of_residence)) %>%
  arrange(desc(Freq)) # material techo
data.frame(table(data_ej1$class)) %>%
  arrange(desc(Freq)) # es una de las variables más importantes, si no se incluye, los años 2010 y 2012 no se vuelven significativos

# Eliminamos esas observaciones faltantes
data_ej1 <- data_ej1 %>%
  filter(roof_material != "")

# Verificamos de nuevo, no hay NA
data_ej1_na2 <- data_ej1 %>% select_if(~ sum(is.na(.)) > 0)
plot_missing(data_ej1_na2)

# Creación nuevas variables #

# Pasamos sale_price a logaritmo, más fácil de trabajar y se normaliza la distribución
data_ej1 <- data_ej1 %>% 
  mutate(ln_sale_price = log(sale_price))

data_ej1_rs <- data_ej1 %>% 
  mutate(ln_sale_price = log(sale_price))

ggplot(data_ej1, aes(x = ln_sale_price)) + 
  geom_histogram(bins = 50, col= "white")

# Transformar el año de venta a factor (con etiquetas)
data_ej1 <- data_ej1 %>%
  mutate(
    year = factor(
      year,
      levels = 2000:2020,
      labels = paste0("d", 2000:2020)
    )
  )

# --- MODELO HEDÓNICO --- # ----
X_ctrl <- c("year_built", "class", "building_sqft", "num_bedrooms", 
            "num_full_baths", "num_fireplaces", "type_of_residence",
            "construction_quality", "attic_finish", "garage_attached",
            "garage_size", "basement_type", "roof_material", "renovation", 
            "porch", "central_air")


formula_hed <- as.formula(
  paste("ln_sale_price ~ factor(year) +", paste(X_ctrl, collapse = " + "))
)

reg_hed <- lm(formula_hed, data = data_ej1)
reg_hed_ef_clu <- feols(
  ln_sale_price ~ factor(year) + year_built + class  + building_sqft +
    num_full_baths + num_fireplaces + type_of_residence + num_bedrooms +
    construction_quality + attic_finish + garage_attached + garage_size +
    basement_type + roof_material + renovation + porch + central_air
  | pin + township_code,      # efectos fijos
  vcov = ~pin,                # errores agrupados
  data = data_ej1
)

stargazer(reg_hed, type = "text")
etable(reg_hed_ef_clu)

# --- REPEAT SALES INDEX --- # ----

# Calculamos # de veces que fue vendido el hogar
data_ej1_rs <- data_ej1_rs %>% 
  group_by(pin) %>% 
  mutate(times_sold = row_number()) %>%  # cada venta se enumera en orden
  ungroup()

data.frame(table(data_ej1_rs$times_sold)) %>%
  arrange(desc(Freq)) # Con esta metodología se pierden 292.879 datos

# Filtramos casas vendidas más de una vez
data_ej1_rs <- data_ej1_rs %>% 
  group_by(pin) %>% 
  mutate(sold_multiple = ifelse(n() > 1, 1, 0)) %>% 
  ungroup() %>% 
  filter(sold_multiple == 1)

# Datos de ventas y años consecutivos
prices_house_sales <- data_ej1_rs %>% 
  group_by(pin) %>%
  mutate(price0 = lag(sale_price)) %>%
  ungroup() %>%
  filter(!is.na(price0)) %>% # quedarse con pares de ventas consecutivas
  rename(price1 = sale_price) %>%
  select(price1, price0)

times_house_sales <- data_ej1_rs %>% 
  group_by(pin) %>%
  mutate(time0 = lag(year)) %>%
  ungroup() %>%
  filter(!is.na(time0)) %>%        # mismos pares consecutivos
  rename(time1 = year) %>%
  select(time1, time0)

# Primeras diferencias
rep_sales <- cbind(prices_house_sales, times_house_sales)
head(rep_sales)

# Transformamos las variables
price1 <- log(rep_sales$price1)
price0 <- log(rep_sales$price0)
time1  <- rep_sales$time1
time0  <- rep_sales$time0

# Diferencial de precios y construcción de matriz
dv <- price1 - price0
timevar <- sort(unique(c(time0, time1)))
nt <- length(timevar)# número de periodos
n  <- length(dv) #número de observaciones

# Regla: 
  # +1 si segunda venta
  # -1 si primera venta
  # 0 en otro caso

xmat <- matrix(0, nrow = n, ncol = nt - 1)
for (j in seq(2, nt)) {
  xmat[, j - 1] <- ifelse(time1 == timevar[j],  1, xmat[, j - 1])
  xmat[, j - 1] <- ifelse(time0 == timevar[j], -1, xmat[, j - 1])
}
colnames(xmat) <- paste0("Time", timevar[-1]) 
xmat

# Estimamos el modelo RS
reg_rs <- lm(dv ~ xmat + 0)
summary(reg_rs)
residuals(reg_rs)

# Guardamos los residuos del modelo y la diferencia de tiempo entre ventas
e <- residuals(reg_rs)
xvar <- time1 - time0
xvar

# Con esto, podemos tenemos que ajustar la varianza condicional (errores tienden a crecer con el tiempo)
# Condición Bailey–Muth–Nourse (BMN)

# Estimamos los pesos del modelo
reg_rs <- lm(e^2 ~ xvar) #Ajuste de varianza condicional de los errores
wgt <- fitted(reg_rs) #Predicciones = estimación de la varianza
samp <- wgt > 0 
wgt <- ifelse(samp == TRUE, 1 / wgt, 0)

# Tras estimar los pesos (donde las varianzas más altas deberían tener menor ponderación)
# hacemos una regresión lineal ponderada

# Regresión Ponderada Repeat Sales
# -1 para evitar colinealidad, se quita el intercepto (no es necesario estimarlo)
reg_rs <- lm(dv ~ xmat - 1, weights = wgt)
stargazer(reg_rs, type = "text", digits = 4)

# --- RESULTADO FINAL --- # ----
stargazer(reg_hed, reg_rs, type = "text")
etable(reg_hed_ef_clu)

# --- Índice Hedónico 
coef_hed <- broom::tidy(reg_hed) %>%
  filter(grepl("^factor\\(year\\)", term)) %>%
  mutate(
    year = as.numeric(stringr::str_extract(term, "\\d{4}")),  # extrae el año
    type = "Índice Hedónico"
  ) %>%
  select(year, estimate, se = std.error, type)

# --- Índice Hedónico con EF y Clúster
coef_hed_ef_clu <- broom::tidy(reg_hed_ef_clu) %>%
  filter(grepl("^factor\\(year\\)", term)) %>%
  mutate(
    year = as.numeric(stringr::str_extract(term, "\\d{4}")),
    type = "Índice Hedónico con EF y Clúster"
  ) %>%
  select(year, estimate, se = std.error, type)

# --- Repeat Sales
coef_rs <- broom::tidy(reg_rs) %>%
  mutate(
    year = as.numeric(stringr::str_extract(term, "\\d{4}")),
    type = "Índice de Ventas Repetidas"
  ) %>%
  filter(!is.na(year)) %>%
  select(year, estimate, se = std.error, type)

# --- Índice en log price ----
# Combinamos las gráficas
index_all_logp <- bind_rows(coef_hed, coef_hed_ef_clu, coef_rs)

# Agregamos año base (2000)
base_year <- tibble(
  year = 2000,
  estimate = 0,
  type = c("Índice Hedónico", 
           "Índice Hedónico con EF y Clúster", 
           "Índice de Ventas Repetidas")
)

# Unimos y ordenamos
index_all_logp <- bind_rows(index_all_logp, base_year) %>%
  arrange(type, year)

# Graficamos
ggplot(index_all_logp, aes(x = year, y = estimate, color = type, linetype = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  labs(
    x = "Año",
    y = "Variación logarítmica del precio (base = 2000)",
    title = "Evolución del precio de la vivienda en Cook County, Illinois",
    subtitle = "Comparación de índices: Hedónico, Hedónico con EF y Clúster, Ventas Repetidas (en log y sin bandas)",
    color = "Método de estimación",
    linetype = "Método de estimación"
  ) +
  scale_color_manual(values = c(
    "Índice Hedónico" = "#1B9E77",
    "Índice Hedónico con EF y Clúster" = "#7570B3",
    "Índice de Ventas Repetidas" = "#D95F02"
  )) +
  scale_x_continuous(breaks = seq(2000, max(index_all_logp$year, na.rm = TRUE), 2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# --- Índice en % ----

# Combinamos los coeficientes
index_all_percent <- bind_rows(coef_hed, coef_hed_ef_clu, coef_rs) %>%
  mutate(
    perc_change = (exp(estimate) - 1) * 100,
    se = se * 100,  # convierte el error estándar a porcentaje
    upper = perc_change + 1.96 * se,
    lower = perc_change - 1.96 * se
  )

# Agregamos año base (2000)
base_year <- tibble(
  year = 2000,
  estimate = 0,
  perc_change = 0,
  se = 0,
  upper = 0,
  lower = 0,
  type = c("Índice Hedónico", 
           "Índice Hedónico con EF y Clúster", 
           "Índice de Ventas Repetidas")
)

index_all_percent <- bind_rows(index_all_percent, base_year) %>%
  arrange(type, year)

# Graficamos
ggplot(index_all_percent, aes(x = year, y = perc_change, color = type, linetype = type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = type), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  labs(
    x = "Año",
    y = "Variación porcentual del precio (base = 2000)",
    title = "Evolución del precio de la vivienda en Cook County, Illinois",
    subtitle = "Comparación de índices: Hedónico, Hedónico con EF y Clúster, Ventas Repetidas",
    color = "Método de estimación",
    linetype = "Método de estimación",
    fill = "Método de estimación"
  ) +
  scale_color_manual(values = c(
    "Índice Hedónico" = "#1B9E77",
    "Índice Hedónico con EF y Clúster" = "#7570B3",
    "Índice de Ventas Repetidas" = "#D95F02"
  )) +
  scale_fill_manual(values = c(
    "Índice Hedónico" = "#1B9E77",
    "Índice Hedónico con EF y Clúster" = "#7570B3",
    "Índice de Ventas Repetidas" = "#D95F02"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(2000, max(index_all_percent$year, na.rm = TRUE), 2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
