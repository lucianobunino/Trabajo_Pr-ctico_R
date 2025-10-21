
# TRABAJO PRÁCTICO – PROGRAMACIÓN EN R
# Ejercicio de Análisis de Datos
# Dataset: Estadísticas de jugadores NBA 2018–2019

# ----------------------------------------------------------
# Preparación
# ----------------------------------------------------------
set.seed(1234)
library(tidyverse)
library(readxl)
library(readr)

# Cargamos los datos
nba_datos <- read_csv("https://raw.githubusercontent.com/dataprofessor/data/master/nba-player-stats-2019.csv")

# Renombramos las variables (simplicidad)
nba <- nba_datos %>% 
  transmute(
    Jugador      = Player,
    Posicion     = Pos,
    Equipo       = Tm,
    Edad         = Age,
    Partidos     = G,
    Minutos      = MP,
    Puntos       = PTS,
    Triples      = `3P`,
    Porc_Triples = `3P%`,
    TirosLibres  = FT,
    Porc_TL      = `FT%`,
    Rebotes      = TRB,
    Asistencias  = AST
  )

# BUscamos SOLO posiciones puras (sin combinaciones)
posiciones_validas <- c("C", "PF", "SF", "SG", "PG")
nba <- nba %>% 
  filter(Posicion %in% posiciones_validas)

# Eliminamos filas con N/A en variables clave
nba <- nba %>% 
  filter(!is.na(Puntos), !is.na(Minutos), !is.na(Porc_TL), !is.na(Triples))

#  Ordenamos 
nba <- nba %>%
  mutate(Posicion = factor(Posicion, levels = c("PG","SG","SF","PF","C")))

# ----------------------------------------------------------
# Las Preguntas;

# 1. ¿Cómo varía la efectividad de tiros libres (Porc_TL) según la posición?
# 2. ¿Cuál es la relación entre Minutos y Puntos, y cómo cambia por posición?
# 3. ¿Cómo se relaciona la posición con el promedio de Triples anotados por partido?

# ----------------------------------------------------------
# Desarrollo
# ----------------------------------------------------------

#1) Efectividad de tiros libres por posición (solo posiciones puras)
resumen_tl <- nba %>% 
  group_by(Posicion) %>% 
  summarise(
    Cantidad_Jugadores = n(),
    Promedio_TL = mean(Porc_TL, na.rm = TRUE),
    Mediana_TL  = median(Porc_TL, na.rm = TRUE),
    Desvio_TL   = sd(Porc_TL, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(Promedio_TL))


graf_tl <- ggplot(resumen_tl, aes(x = Posicion, y = Promedio_TL, fill = Posicion)) +
  geom_col(width = 0.9) +
  geom_text(aes(label = paste0(round(Promedio_TL*100, 1), "%")),
            vjust = -0.35, size = 4) +
  labs(
    title = "Efectividad promedio en tiros libres por posición (NBA 2018–19)",
    x = "Posición del jugador",
    y = "Porcentaje de tiros libres (FT%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graf_tl)

#2) Relación entre minutos y puntos (por posición)
correlacion_mp_pts <- cor(nba$Minutos, nba$Puntos, use = "complete.obs")

graf_mp_pts <- ggplot(nba, aes(x = Minutos, y = Puntos, color = Posicion)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre minutos y puntos por partido",
    x = "Minutos promedio por partido",
    y = "Puntos por partido",
    color = "Posición"
  ) +
  theme_minimal()

print(graf_mp_pts)


#3) Promedio de triples por posición
resumen_3p <- nba %>% 
  group_by(Posicion) %>% 
  summarise(
    Cantidad_Jugadores = n(),
    Promedio_Triples = mean(Triples, na.rm = TRUE),
    Mediana_Triples  = median(Triples, na.rm = TRUE),
    Desvio_Triples   = sd(Triples, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(Promedio_Triples))

graf_triples <- ggplot(resumen_3p, aes(x = Posicion, y = Promedio_Triples, fill = Posicion)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = round(Promedio_Triples, 2)), vjust = -0.35, size = 4) +
  labs(
    title = "Promedio de triples anotados por posición (NBA 2018–19)",
    x = "Posición del jugador",
    y = "Triples por partido"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graf_triples)


# ----------------------------------------------------------
# 5) Conclusion 

#CONCLUSIONES :
#Las posiciones perimetrales (PG/SG) presentan, en promedio, mejor efectividad de tiros libres; 
#los pivots (C) muestran los valores más bajos.
#La relación entre Minutos y Puntos es positiva
#que la pendiente difiere por posición (PG/SG suelen escalar más con los minutos que C).
#La especialización en el triple recae en PG/SG (y luego SF); PF/C aportan menos desde el perímetro


# Análisis econométrico con Gapminder
# Ingreso por persona 
#-----------------------------------------------------
# preparación
#----------------------------------------------------


#para correr el código, favor descargar dataset "gapminder" del repositorio
#e importar.
gm <- read_csv("gapminder.csv")

# Aseguramos tipos para year e income_per_person y limpiamos N/As en esas dos
gm <- gm %>%
  mutate(
    year = as.integer(year),
    income_per_person = as.numeric(income_per_person)
  ) %>%
  filter(!is.na(country), !is.na(year), !is.na(income_per_person)) %>%
  arrange(country, year)

#----------------------------------------------------------
# PARTE 2: Esperanza de vida y género (incisos 5–9)
#----------------------------------------------------------

# ---------------------------
# (1) Serie de Argentina
# ---------------------------
ar <- gm %>% filter(country == "Argentina") %>% arrange(year)

ggplot(ar, aes(year, income_per_person)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(title = "Argentina: ingreso por persona", x = "Año", y = "USD") +
  theme_minimal()

# Comentario: Muestra una tendencia creciente de largo plazo con periodos de volatilidad intermedios.

# ---------------------------
# (2) Entrenamiento / 3 modelos
# ---------------------------

# Definimos t reescalado y separo últimos 10 años para TEST
ar <- ar %>% mutate(t = year - min(year))
ult_10 <- tail(sort(unique(ar$year)), 10)
train  <- ar %>% filter(!year %in% ult_10)
test   <- ar %>% filter( year %in% ult_10)

# Modelos: lineal, polinomio grado 2 y grado 10
mod_lin <- lm(income_per_person ~ t, data = train)
mod_g2  <- lm(income_per_person ~ poly(t, 2,  raw = TRUE), data = train)
mod_g10 <- lm(income_per_person ~ poly(t, 10, raw = TRUE), data = train)

# Predicciones sobre toda la serie para graficar
grid_pred <- ar %>%
  select(year, t) %>% distinct() %>% arrange(year) %>%
  mutate(
    y_lin = predict(mod_lin,  newdata = .),
    y_g2  = predict(mod_g2,   newdata = .),
    y_g10 = predict(mod_g10,  newdata = .)
  )

ggplot(ar, aes(year, income_per_person)) +
  geom_point(alpha = 0.6, size = 1.7) +
  geom_line(alpha = 0.4) +
  geom_line(data = grid_pred, aes(y = y_lin, color = "Lineal"), linewidth = 1) +
  geom_line(data = grid_pred, aes(y = y_g2,  color = "Grado 2"), linewidth = 1) +
  geom_line(data = grid_pred, aes(y = y_g10, color = "Grado 10"), linewidth = 1) +
  scale_color_manual(values = c("Lineal"="#1b9e77","Grado 2"="#7570b3","Grado 10"="#d95f02")) +
  labs(title = "Argentina: ajustes sobre income_per_person",
       x = "Año", y = "USD", color = "Modelo") +
  theme_minimal()

# Métricas en TRAIN y TEST
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))

met_train <- tibble(
  Modelo = c("Lineal","G2","G10"),
  RMSE = c(
    rmse(train$income_per_person, predict(mod_lin, newdata = train)),
    rmse(train$income_per_person, predict(mod_g2,  newdata = train)),
    rmse(train$income_per_person, predict(mod_g10, newdata = train))
  ),
  MAE = c(
    mae(train$income_per_person, predict(mod_lin, newdata = train)),
    mae(train$income_per_person, predict(mod_g2,  newdata = train)),
    mae(train$income_per_person, predict(mod_g10, newdata = train))
  )
)

met_test <- tibble(
  Modelo = c("Lineal","G2","G10"),
  RMSE = c(
    rmse(test$income_per_person, predict(mod_lin, newdata = test)),
    rmse(test$income_per_person, predict(mod_g2,  newdata = test)),
    rmse(test$income_per_person, predict(mod_g10, newdata = test))
  ),
  MAE = c(
    mae(test$income_per_person, predict(mod_lin, newdata = test)),
    mae(test$income_per_person, predict(mod_g2,  newdata = test)),
    mae(test$income_per_person, predict(mod_g10, newdata = test))
  )
)
#El modelo lineal muestra bien la tendencia general del ingreso en Argentina, 
#aunque no capta todas las subidas y bajadas. 
#El polinómico de grado 2 se ajusta un poco mejor porque permite cierta curvatura. 
#En cambio, el de grado 10 se pasa de complejo: ajusta perfecto los datos viejos pero 
#predice mal los últimos años. 
#Los modelos lineal o grado 2 terminan funcionando mejor porque generalizan más

# ----------------------------------------------------------
# (3) Correlaciones con 4 países sudamericanos (SIN Argentina)
# ----------------------------------------------------------

# separamos los paises de interes:
paises_sa <- c("Brazil","Chile","Uruguay","Peru")

gm_sa <- gm %>% filter(country %in% paises_sa)

# (a) Correlación de niveles 
niveles_wide <- gm_sa %>%
  select(country, year, income_per_person) %>%
  pivot_wider(names_from = country, values_from = income_per_person) %>%
  arrange(year)

niveles_mat <- niveles_wide %>% select(-year) %>% drop_na() %>% as.matrix()
cor_niveles <- cor(niveles_mat)
print(round(cor_niveles, 3))

# (b) Correlación de variaciones % anuales (YoY) 
crec_sa <- gm_sa %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(yoy = 100 * (income_per_person/lag(income_per_person) - 1)) %>%
  ungroup()

crec_wide <- crec_sa %>%
  select(country, year, yoy) %>%
  pivot_wider(names_from = country, values_from = yoy) %>%
  arrange(year)

crec_mat <- crec_wide %>% select(-year) %>% drop_na() %>% as.matrix()
cor_yoy <- cor(crec_mat)
print(round(cor_yoy, 3))

#comentario: Entre Brasil, Chile, Uruguay y Perú se observa que los niveles de
#ingreso por persona están  correlacionados, lo que indica que las economías 
#tienden a moverse en direcciones parecidas en el largo plazo.
#Sin embargo viendo las variaciones anuales las correlaciones son mas debiles.
#Mostrando que en el largo plazo tienden a ver un crecimiento similar
#en el corto plazo cada país tiene sus propios ciclos y volatilidad economica de corto plazo

#----------------------------------------------------------
# PARTE 2: Esperanza de vida y género (incisos 5–9)
#----------------------------------------------------------

yr_selecto <- 2004

gm_y <- gm %>%
  filter(year == yr_selecto) %>%
  select(country, life_expectancy, life_expectancy_female, income_per_person) %>%
  mutate(
    across(
      c(life_expectancy, life_expectancy_female, income_per_person),
      ~ as.numeric(
        gsub(",", ".",        # cambia coma por punto decimal
             gsub("[^0-9,\\.]", "", # elimina todo menos números, puntos o comas
                  trimws(as.character(.)))) # quita espacios
      )
    )
  ) %>%
  drop_na(life_expectancy, life_expectancy_female, income_per_person)




#----------------------------------------------------------
# 5) Gráfico: life_expectancy vs life_expectancy_female
#----------------------------------------------------------
ggplot(gm_y, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = paste0("Año ", yr_selecto, ": Esperanza de vida (total vs mujeres)"),
    x = "Esperanza de vida mujeres",
    y = "Esperanza de vida (total)"
  ) +
  theme_minimal()

# Comentario:
# Se observa una relación lineal positiva en la cual los paises donde 
#las mujeres viven más también tienen mayor esperanza de vida promedio.


#----------------------------------------------------------
# 6) Regresión life_expectancy ~ life_expectancy_female
#    y cálculo de R^2
#------------------------------------------------------------
m_simple <- lm(life_expectancy ~ life_expectancy_female, data = gm_y)
summary(m_simple)$coefficients     # coeficientes
r2_simple <- summary(m_simple)$r.squared
r2_simple


# Comentario:
# El coeficiente estimado suele ser cercano a 1 y R^2 
# Esto lleva a que la esperanza de vida femenina explica casi toda la variación
# en la esperanza de vida total

#-------------------------------------------------------------
# 7) Test H0: life_expectancy_female = life_expectancy
#    vs life_expectancy_female > life_expectancy 
#----------------------------------------------------------
t_res <- t.test(gm_y$life_expectancy_female, gm_y$life_expectancy,
                paired = TRUE, alternative = "greater")
t_res

# Comentario:
# El test rechaza H0,
# Esto indica que, en promedio, la esperanza de vida de las mujeres
# es significativamente mayor que la del total 

#-------------------------------------------------------
# 8) Regresión múltiple: life_expectancy ~ life_expectancy_female + income_per_person
#----------------------------------------------------------
gm_y <- gm_y %>% mutate(log_income = log(income_per_person))
m_mult <- lm(life_expectancy ~ life_expectancy_female + log_income, data = gm_y)
summary(m_mult)$coefficients
r2_mult <- summary(m_mult)$r.squared
r2_mult
# Comparación con el modelo simple:
c(R2_simple = r2_simple, R2_multiple = r2_mult)

# Comentario:
# Al incluir income.per.person, R^2 aumenta 
# Esto muestra que el ingreso ayuda a explicar diferencias que existen entre 
#los países, aunque el efecto principal sigue siendo el de life.expectancy.female.
# Vale la pena incluir income.per.person porque da una imagen mas precisa.

#----------------------------------------------------------
# 9) Modelo alternativo 
#----------------------------------------------------------
candidatas <- c("log_income", "fertility", "child_mortality", "population",
                "hiv_prevalence", "co2_emissions")

presentes <- intersect(candidatas, names(gm_y))
vars_elige <- head(presentes, 3)   # máximo tres
if (length(vars_elige) == 0) stop("No hay covariables disponibles para el inciso 9.")

form_9 <- as.formula(paste("life_expectancy ~", paste(vars_elige, collapse = " + ")))
m_alt <- lm(form_9, data = gm_y)

list(
  Variables_usadas_inciso9 = vars_elige,
  Coeficientes = summary(m_alt)$coefficients,
  R2 = summary(m_alt)$r.squared
)
# Comentario;
# Un modelo con income_per_person, fertility y child_mortality
#explica bien la esperanza de vida. El ingreso se asocia
#positivamente, mientras que la fertilidad y la mortalidad infantil lo
#hacen negativamente. Este "mix"   de variables muestra bien
#las condiciones de cada país.

# Ejercicio de Simulación 1: Demanda Cobb–Douglas
#-------------------------------------------------------------

# Bienes y preferencias 
p1  <- 2              # precio bien 1
p2  <- 3              # precio bien 2
alfa1 <- 0.6          # 0<alfa1<1
alfa2 <- 1 - alfa1    # con alfa1 + alfa2 = 1

#------------------------------------------------------------
# 1) Generación de ingresos
#-----------------------------------------------------------
simular_ingreso <- function(n, k, escala = 1) {
  rchisq(n, df = k) * escala
}

#-----------------------------------------------------------
# 2) Demanda óptima Cobb–Douglas
#----------------------------------------------------------
demanda_cd <- function(Y, p1, p2, alfa1) {
  alfa2 <- 1 - alfa1
  x1 <- (alfa1 * Y) / p1
  x2 <- (alfa2 * Y) / p2
  U  <- (x1 ^ alfa1) * (x2 ^ alfa2)
  tibble(x1 = x1, x2 = x2, U = U)
}


#---------------------------------------------------------
# 3) Simulación base 
#-----------------------------------------------------------
n  <- 10000   # hogares
k  <- 6       
esc <- 1000   # escala 

Y_base   <- simular_ingreso(n, k, escala = esc)
dem_base <- demanda_cd(Y = Y_base, p1 = p1, p2 = p2, alfa1 = alfa1) |>
  mutate(Y = Y_base)

# Medidas y cuartiles 
resumen_base <- dem_base |>
  summarise(
    media_x1 = mean(x1), q1_x1 = quantile(x1, .25), med_x1 = median(x1), q3_x1 = quantile(x1, .75),
    media_x2 = mean(x2), q1_x2 = quantile(x2, .25), med_x2 = median(x2), q3_x2 = quantile(x2, .75),
    media_U  = mean(U),  q1_U  = quantile(U,  .25), med_U  = median(U),  q3_U  = quantile(U,  .75)
  )
print(resumen_base)
# Reunimos las tres variables en un mismo data frame largo
dem_long <- dem_base |>
  select(x1, x2, U) |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valor")

# Gráfico conjunto
ggplot(dem_long, aes(x = valor, fill = variable)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 40, color = "white") +
  scale_fill_manual(values = c("x1" = "#1b9e77", "x2" = "#d95f02", "U" = "#7570b3"),
                    name = "Variable",
                    labels = c("x1* (bien 1)", "x2* (bien 2)", "U* (utilidad)")) +
  labs(title = "Distribuciones simuladas de x1*, x2* y U* (n = 10,000 hogares)",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal(base_size = 13)


#-----------------------------------------------------------
# 4) Probabilidad de bajo consumo en un bien
#-------------------------------------------------------------

prob_bajo_consumo <- function(x1, x2, c, j) {
  if (j == 1) mean(x1 < c)
  else if (j == 2) mean(x2 < c)
  else stop("j debe ser 1 o 2")
}

c_umbral <- 50
pb_x1_base <- prob_bajo_consumo(dem_base$x1, dem_base$x2, c = c_umbral, j = 1)
pb_x2_base <- prob_bajo_consumo(dem_base$x1, dem_base$x2, c = c_umbral, j = 2)


#-----------------------------------------------------------
# 5) Shock de precios
#-----------------------------------------------------------
p1_shock <- 1.2 * p1
dem_shock <- demanda_cd(Y = Y_base, p1 = p1_shock, p2 = p2, alfa1 = alfa1) |>
  mutate(Y = Y_base)

#-----------------------------------------------------------
# 6) Visualización comparada
#-----------------------------------------------------------
bind_rows(
  dem_base  |> transmute(x1, estado = "pre shock"),
  dem_shock |> transmute(x1, estado = "post shock")
) |>
  ggplot(aes(x = x1, fill = estado)) +
  geom_histogram(position = "identity", alpha = .5, bins = 40) +
  labs(title = "x1*: antes y después del shock (p1 ↑ 20%)",
       x = "x1*", y = "Frecuencia", fill = "") +
  theme_minimal()

# consumo antes y después del shock
pb_x1_post <- prob_bajo_consumo(dem_shock$x1, dem_shock$x2, c = c_umbral, j = 1)


# Cambio en utilidad promedio
delta_U <- mean(dem_shock$U) - mean(dem_base$U)

#Conclusion: Cuando aumenta el precio del bien 1 la masa de hogares que 
#consumen muy poco del bien 1 aumenta, ya que el bien se vuelve relativamente 
#más caro. COmo consecuencia, la utilidad  promedio también disminuye, ya que los 
#hogares no tienen acceso las mismas combinaciones de consumo que solian tener
#con su ingreso.

#---------------------------------------------------------------
# 7) Heterogeneidad en preferencias
#-------------------------------------------------------------
a_beta <- 8; b_beta <- 6
alfa1_i <- rbeta(n, a_beta, b_beta)  
alfa2_i <- 1 - alfa1_i

# Base con heterogeneidad
dem_base_het <- tibble(Y = Y_base, alfa1 = alfa1_i, alfa2 = alfa2_i) |>
  mutate(
    x1 = (alfa1 * Y) / p1,
    x2 = (alfa2 * Y) / p2,
    U  = (x1 ^ alfa1) * (x2 ^ alfa2)
  )

# Post shock con heterogeneidad
dem_shock_het <- tibble(Y = Y_base, alfa1 = alfa1_i, alfa2 = alfa2_i) |>
  mutate(
    x1 = (alfa1 * Y) / p1_shock,
    x2 = (alfa2 * Y) / p2,
    U  = (x1 ^ alfa1) * (x2 ^ alfa2)
  )

# Histogramas superpuestos 
bind_rows(
  dem_base_het  |> transmute(x1, estado = "pre shock (het)"),
  dem_shock_het |> transmute(x1, estado = "post shock (het)")
) |>
  ggplot(aes(x = x1, fill = estado)) +
  geom_histogram(position = "identity", alpha = .5, bins = 40) +
  labs(title = "x1*: pre vs post shock con heterogeneidad (α1 ~ Beta)",
       x = "x1*", y = "Frecuencia", fill = "") +
  theme_minimal()

# Probabilidad de bajo consumo con heterogeneidad
pb_x1_base_het <- prob_bajo_consumo(dem_base_het$x1, dem_base_het$x2, c = c_umbral, j = 1)
pb_x1_post_het <- prob_bajo_consumo(dem_shock_het$x1, dem_shock_het$x2, c = c_umbral, j = 1)


# variacion utilidad con heterogeneidad
delta_U_het <- mean(dem_shock_het$U) - mean(dem_base_het$U)

#Conclusion; # Al introducir heterogeneidad, los hogares cambian en
# cuánto valoran el bien 1. Aquellos con α1 más alto 
# son más afectados por el impacto del shock de precios, se reduce su consumo y su utilidad.
# Los que tienen α1 bajo se ven menos afectados.
