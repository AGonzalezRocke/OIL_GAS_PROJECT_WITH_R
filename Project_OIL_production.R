######################################
#PROJECT: OIL PRODUCTION
#Descripción: Revisión de la producción de petróleo 1967-1999
#Autor: Abraham González Roque
#Fecha: 01/09/2026
# Base de datos obtenida de KAGGLE

######################################
# Lectura e identificación de datos
######################################
# Importar el archivo CSV
datos <- read.csv(
  "C:/Users/ag_ro/Documents/CURSO DATA SCIENCE/OIL-GAS/oil-and-gas-summary-production-data-1967-1999.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Mostrar las primeras observaciones
head(datos)

# Número de filas y columnas
dim(datos)

# Número de filas
nrow(datos)

# Número de columnas
ncol(datos)

# Nombres exactos de las columnas
names(datos)

# Estructura general de la base
str(datos)

# Clase de cada variable
sapply(datos, class)

# Tipo interno de cada variable
sapply(datos, typeof)

# Variables cuantitativas
variables_cuantitativas <- names(datos)[sapply(datos, is.numeric)]

# Variables cualitativas
variables_cualitativas <- names(datos)[sapply(
  datos,
  function(x) is.character(x) || is.factor(x)
)]
#Otraopcion para elegir las variables cualitativas
variables_cualitativas <- names(datos)[sapply(
  datos,is.character)]
#Mostrarlas variables
variables_cuantitativas
variables_cualitativas

######################################
##GRÁFICAS##
######################################

#Producción oil vs gas
ggplot(
  datos,
  aes(
    x = `Oil Produced, bbl`,
    y = `Gas Produced, Mcf`
  )
) +
  geom_point(
    alpha = 0.4,
    color = "steelblue"
  ) +
  labs(
    title = "Producción de petróleo y gas — datos crudos",
    subtitle = "Antes de la limpieza y adecuación de los datos",
    x = "Producción de petróleo",
    y = "Producción de gas"
  ) +
  theme_minimal()
# Dispersión Produccion de petroleo por año
ggplot(
  datos,
  aes(
    x = `Production Year`,
    y = `Oil Produced, bbl`,
  )
) +
  geom_point(
    alpha = 0.4,
    color = "darkgreen"
  ) +
  labs(
    title = "Dispersión de la producción — datos crudos",
    subtitle = "Antes de la limpieza y adecuación",
    x = "Año",
    y = "Producción de petróleo"
  ) +
  theme_minimal()

#Histograma produccion petróleo
ggplot(
  datos,
  aes(x = `Oil Produced, bbl`)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución de la producción — datos crudos",
    x = "Producción",
    y = "Frecuencia"
  ) +
  theme_minimal()

#Producción de Gas por año#
ggplot(
  datos,
  aes(
    x = `Production Year`,
    y = `Gas Produced, Mcf`
  )
) +
  geom_point(
    alpha = 0.35,
    color = "darkorange"
  ) +
  labs(
    title = "Dispersión de la producción de gas por año",
    subtitle = "Datos crudos",
    x = "Año de producción",
    y = "Gas producido (Mcf)"
  ) +
  theme_minimal()

#Produccion de agua, esta grafica fue omitida en el reporte#
#Porque no aporta informacion importante
ggplot(
  datos,
  aes(x = `Water produced, bbl`)
) +
  geom_histogram(
    bins = 30,
    fill = "dodgerblue3",
    color = "white"
  ) +
  labs(
    title = "Distribución de la producción de agua",
    subtitle = "Datos crudos",
    x = "Agua producida (bbl)",
    y = "Número de registros"
  ) +
  theme_minimal()

library(ggplot2)

ggplot(
  datos,
  aes(x = `Water produced, bbl`)
) +
  geom_histogram(
    bins = 30,
    fill = "dodgerblue3",
    color = "white"
  ) +
  scale_x_log10() +
  labs(
    title = "Distribución de la producción de agua",
    subtitle = "Datos crudos con escala logarítmica",
    x = "Agua producida (bbl, escala log10)",
    y = "Número de registros"
  ) +
  theme_minimal()

################
#Produccion por operador

library('dplyr')
library('ggplot2')

produccion_operador <- datos %>%
  filter(
    !is.na(Operator),
    trimws(Operator) != ""
  ) %>%
  group_by(Operator) %>%
  summarise(
    produccion_petroleo = sum(
      `Oil Produced, bbl`,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  slice_max(
    produccion_petroleo,
    n = 15,
    with_ties = FALSE
  )

ggplot(
  produccion_operador,
  aes(
    x = reorder(Operator, produccion_petroleo),
    y = produccion_petroleo
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",")
  ) +
  labs(
    title = "Producción total de petróleo por operador",
    subtitle = "15 operadores con mayor producción",
    x = "Operador",
    y = "Petróleo producido (bbl)"
  ) +
  theme_minimal()

########
#Produccion petroleo por formacion
###########
produccion_formacion <- datos %>%
  filter(
    !is.na('PRODUCING_FORMATION'),
    trimws('PRODUCING_FORMATION') != ""
  ) %>%
  group_by(PRODUCING_FORMATION) %>%
  summarise(
    produccion_petroleo = sum(
      `Oil Produced, bbl`,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  slice_max(
    produccion_petroleo,
    n = 15,
    with_ties = FALSE
  )

ggplot(
  produccion_formacion,
  aes(
    x = reorder(
        PRODUCING_FORMATION,
      produccion_petroleo
    ),
    y = produccion_petroleo
  )
) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",")
  ) +
  labs(
    title = "Producción total de petróleo por formación",
    subtitle = "15 formaciones con mayor producción",
    x = "Formación productora",
    y = "Petróleo producido (bbl)"
  ) +
  theme_minimal()
######################################
##Adecuación y limpieza##
######################################

# Resumen únicamente de variables numéricas
datos_numericos <- datos[sapply(datos, is.numeric)]
summary(datos_numericos)

# Valores nulos por columna
colSums(is.na(datos))

# Cadenas vacías por columna
sapply(datos, function(x) {
  if (is.character(x)) {
    sum(trimws(x) == "", na.rm = TRUE)
  } else {
    0
  }
})
#  Al revisarhay variasfilas sin contenido, pero sin especificar NA

install.packages('dplyr')
library('dplyr')
#Estandarizar todos los FIELD a NA
datos <- datos %>%
  mutate(
    Field = trimws(Field),
    Field = na_if(Field, "")
  )
#Revisamos ejecutando el apartado anterior y FIELD ya sale en ceros, es decir, si hay dato faltante esta como NA

#Revision de campos diferentes por localidad
# Si hay coincidencia de  Town+County unica se asigna el mismo Field al dato faltante NA
revision_field <- datos %>%
  filter(!is.na(Field)) %>%
  group_by(County, Town) %>%
  summarise(
    cantidad_fields = n_distinct(Field),
    fields_encontrados = paste(sort(unique(Field)), collapse = ", "),
    .groups = "drop"
  )

revision_field
#Como solo hay dos casos unicos para asignar la variable, 
#decidi dejar todo en NA y no copiar dato con coincidencia Town+County


#Revision de producing formation

#Estandarizar todos los PRODUCING_FORMATION a NA
#Renombre la columna porque me  da problemas con el espacio
datos <- datos %>%
  rename(PRODUCING_FORMATION = `Producing Formation`)
#Instale paquete  porque no podia asignar NA como lo hice en columna FIELD
install.packages("stringr")
library("stringr")
#REASIGNACION DE NA EN DATOS FALTANTES
datos <- datos %>%
  mutate(
    PRODUCING_FORMATION = str_squish(
      as.character(PRODUCING_FORMATION)
    ),
    PRODUCING_FORMATION = if_else(
      is.na(PRODUCING_FORMATION) |
        PRODUCING_FORMATION == "",
      NA_character_,
      PRODUCING_FORMATION
    )
  )
#VERIFICAR QUE SI SE ASIGNó -NA- A DATOS FALTANTES
sapply(datos, function(x) {
  if (is.character(x)) {
    sum(trimws(x) == "", na.rm = TRUE)
  } else {
    0
  }
})
#VERIFICAR QUE YA NO HAY COLUMNAS VACIAS - 
#EN EL  CASO DE  TOWN NO ES TAN IMPORTANTE EN ESTE MOMENTO CON 657 VACIOS
datos %>%
  summarise(
    across(
      c(Field, Operator, Town, PRODUCING_FORMATION),
      ~ sum(.x == "", na.rm = TRUE)
    )
  )

#para no usar ni modificar datos originales
datos <- datos %>%
  mutate(
    FIELD_CLAVE = str_to_upper(Field),
    OPERATOR_CLAVE = str_to_upper(Operator)
  )
#REVISAR COINCIDENCIAS
revision_formacion <- datos %>%
  filter(
    !is.na(PRODUCING_FORMATION),
    !is.na(FIELD_CLAVE),
    !is.na(OPERATOR_CLAVE)
  ) %>%
  group_by(
    FIELD_CLAVE,
    OPERATOR_CLAVE
  ) %>%
  summarise(
    cantidad_formaciones =
      n_distinct(PRODUCING_FORMATION),
    formaciones = paste(
      sort(unique(PRODUCING_FORMATION)),
      collapse = ", "
    ),
    .groups = "drop"
  )

revision_formacion

#Revisar coincidencias seguras (unicas), ambiguas=354, unicas=2498
revision_formacion %>%
  count(
    tipo = if_else(
      cantidad_formaciones == 1,
      "Única",
      "Ambigua"
    )
  )
# MAPAR DE FORMACIONES CON LAS COINCIDENCIAS UNICAS
mapa_formaciones <- datos %>%
  filter(
    !is.na(PRODUCING_FORMATION),
    !is.na(FIELD_CLAVE),
    !is.na(OPERATOR_CLAVE)
  ) %>%
  distinct(
    FIELD_CLAVE,
    OPERATOR_CLAVE,
    PRODUCING_FORMATION
  ) %>%
  group_by(
    FIELD_CLAVE,
    OPERATOR_CLAVE
  ) %>%
  filter(
    n_distinct(PRODUCING_FORMATION) == 1
  ) %>%
  ungroup() %>%
  rename(
    FORMACION_RECUPERADA = PRODUCING_FORMATION
  )
# APLICAR LAS COINCIDENCIAS
datos_revision <- datos %>%
  left_join(
    mapa_formaciones,
    by = c(
      "FIELD_CLAVE",
      "OPERATOR_CLAVE"
    )
  ) %>%
  mutate(
    PRODUCING_FORMATION_NEW = coalesce(
      PRODUCING_FORMATION,
      FORMACION_RECUPERADA
    )
  )
#VERIFICACION (RECUPERADOS 56 DATOS)
datos_revision %>%
  summarise(
    NA_antes =
      sum(is.na(PRODUCING_FORMATION)),
    
    recuperados =
      sum(
        is.na(PRODUCING_FORMATION) &
          !is.na(PRODUCING_FORMATION_NEW)
      ),
    
    NA_restantes =
      sum(is.na(PRODUCING_FORMATION_NEW))
  )

#Revisión de que se conserva el numero de filas y no se duplicaron
nrow(datos)
nrow(datos_revision)

#RESUMEN DATOS NULOS
resumen_nulos <- datos_revision %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "VARIABLE",
    values_to = "NULOS"
  ) %>%
  mutate(
    PORCENTAJE = round(
      NULOS / nrow(datos_revision) * 100,
      2
    )
  ) %>%
  arrange(desc(PORCENTAJE))

resumen_nulos

#REVISIÓN DE DUPLICADOS
sum(duplicated(datos_revision)) #encontró 4 duplicados

#Para revisar los datos 
datos_revision %>%
  filter(
    duplicated(datos_revision) |
      duplicated(datos_revision, fromLast = TRUE)
  )
# Después de revisar los datos y checar que SI son duplicados
datos_limpios <- datos_revision %>%
  distinct()
#Verificamos de nuevo si hay duplicados
duplicados_eliminados <-
  sum(duplicated(datos_limpios))


#Renombre columnas porque me  da problemas con el espacio
datos_limpios <- datos_limpios %>%
  rename(ACTIVE_OIL_WELLS=`Active Oil Wells`,
         INACTIVE_OIL_WELLS = `Inactive Oil Wells`,
         ACTIVE_GAS_WELLS = `Active Gas Wells`,
         INACTIVE_GAS_WELLS = `Inactive Gas Wells`,
         INJECTION_WELLS =`Injection Wells`,
         DISPOSAL_WELLS =`Disposal Wells`,
         PRODUCTION_DATE_START = `Production Date Entered`,
         SELF_USE_WELL = `Self-use Well`,
         OIL_PRODUCED_BBL = `Oil Produced, bbl`,
         GAS_PRODUCED_MCF = `Gas Produced, Mcf`,
         WATER_PRODUCED_BBL = `Water produced, bbl`
         )
datos_limpios <- datos_limpios %>%
  rename(PRODUCTION_YEAR=`Production Year`
  )
#Revision de datos fuera de rangos posibles (todos en ceros)
datos_limpios %>%
  summarise(
    invalid_year =
      sum(PRODUCTION_YEAR < 1967 |
            PRODUCTION_YEAR > 1999,
          na.rm = TRUE),
    
    pozos_negativos =
      sum(ACTIVE_OIL_WELLS < 0,
          na.rm = TRUE),
    
    petroleo_negativo =
      sum(OIL_PRODUCED_BBL < 0,
          na.rm = TRUE),
    
    gas_negativo =
      sum(GAS_PRODUCED_MCF < 0,
          na.rm = TRUE)
  )
#Revision de produccion sin pozos activos
datos_limpios %>%
  filter(
    ACTIVE_OIL_WELLS == 0,
    OIL_PRODUCED_BBL > 0
  )
# Revision que esos pozos hayan estado activos y les cambiaron el status
# al momento de documentar la base de datos
casos_especiales <- datos_limpios %>%
  filter(
    ACTIVE_OIL_WELLS == 0,
    OIL_PRODUCED_BBL > 0
  )
casos_especiales %>%
  summarise(
    total_casos = n(),
    
    con_pozos_inactivos =
      sum(INACTIVE_OIL_WELLS > 0, na.rm = TRUE),
    
    sin_pozos_activos_ni_inactivos =
      sum(
        INACTIVE_OIL_WELLS == 0,
        na.rm = TRUE
      ),
    
    porcentaje_con_inactivos =
      round(
        mean(INACTIVE_OIL_WELLS > 0,
             na.rm = TRUE) * 100,
        2
      )
  )
# Revision si estos casos fueron en un año en particular.
# Lo cual no fue asi, posiblemente fueron errores en la captura de los datos
casos_especiales %>%
  count(PRODUCTION_YEAR) %>%
  arrange(PRODUCTION_YEAR)
# Colocamos una etiqueta para identificar estos datos 
datos_limpios <- datos_limpios %>%
  mutate(
    CERO_ACTIVOS_CON_PRODUCCION =
      ACTIVE_OIL_WELLS == 0 &
      OIL_PRODUCED_BBL > 0
  )
#

######################################
#ESTADISTICA DESCRIPTIVA
######################################

# Revision de Oil_Produced
summary(datos_limpios$OIL_PRODUCED_BBL)

# REVISION MEDIA, MEDIANA, DESVIACION ESTANDAR.DATOS MUY CARGADOS A LA DERECHA
datos_limpios %>%
  summarise(
    total_registros = n(),
    
    datos_disponibles =
      sum(!is.na(OIL_PRODUCED_BBL)),
    
    datos_faltantes =
      sum(is.na(OIL_PRODUCED_BBL)),
    
    OIL_ceros =
      sum(OIL_PRODUCED_BBL == 0, na.rm = TRUE),
    
    OIL_media =
      mean(OIL_PRODUCED_BBL, na.rm = TRUE),
    
    OIL_mediana =
      median(OIL_PRODUCED_BBL, na.rm = TRUE),
    
    OIL_desviacion_estandar =
      sd(OIL_PRODUCED_BBL, na.rm = TRUE),
    
    OIL_minimo =
      min(OIL_PRODUCED_BBL, na.rm = TRUE),
    
    OIL_Q1 =
      quantile(
        OIL_PRODUCED_BBL,
        0.25,
        na.rm = TRUE
      ),
    
    OIL_Q3 =
      quantile(
        OIL_PRODUCED_BBL,
        0.75,
        na.rm = TRUE
      ),
    
    OIL_maximo =
      max(OIL_PRODUCED_BBL, na.rm = TRUE)
  )

# Revision de producción cero 78.26% producción positiva = 21.74%
datos_limpios %>%
  summarise(
    total = n(),
    OIL_produccion_cero =
      sum(OIL_PRODUCED_BBL == 0, na.rm = TRUE),
    
    OIL_produccion_positiva =
      sum(OIL_PRODUCED_BBL > 0, na.rm = TRUE),
    
    OIL_porcentaje_cero = round(
      mean(OIL_PRODUCED_BBL == 0, na.rm = TRUE) * 100,
      2
    ),
    OIL_porcentaje_positivo = round(
      mean(OIL_PRODUCED_BBL > 0, na.rm = TRUE) * 100,
      2
    )
  )
#ANÁLISIS SOLO DE LA PRODUCCIÓN POSITIVA
df_oil_positivo <- datos_limpios %>%
  filter(OIL_PRODUCED_BBL > 0)

summary(
  df_oil_positivo$OIL_PRODUCED_BBL
)
#análisis por cuantiles
quantile(
  df_oil_positivo$OIL_PRODUCED_BBL,
  probs = c(
    0,
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)

# Datos atípicos. El detalle es que no  serán tan atípicos.
Q1_OIL_POSITIVE <- quantile(
  df_oil_positivo$OIL_PRODUCED_BBL,
  0.25,
  na.rm = TRUE
)

Q3_OIL_POSITIVE <- quantile(
  df_oil_positivo$OIL_PRODUCED_BBL,
  0.75,
  na.rm = TRUE
)

RIC <- Q3_OIL_POSITIVE - Q1_OIL_POSITIVE

limite_inferior <- Q1_OIL_POSITIVE - 1.5 * RIC
limite_superior <- Q3_OIL_POSITIVE + 1.5 * RIC

limite_inferior
limite_superior
#CONTEO  DE VALORES ATIPICOS NO TAN ATIPICOS. 14.2% DE VALORES ATIPICOS
df_oil_positivo %>%
  summarise(
    total = n(),
    
    atipicos = sum(
      OIL_PRODUCED_BBL > limite_superior,
      na.rm = TRUE
    ),
    
    porcentaje_atipicos = round(
      mean(
        OIL_PRODUCED_BBL > limite_superior,
        na.rm = TRUE
      ) * 100,
      2
    )
  )


# DISTRIBUCION DE LA PRODUCCION POSITIVA  DE OIL
library(ggplot2)

ggplot(
  df_oil_positivo,
  aes(x = OIL_PRODUCED_BBL)
) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución de la producción positiva de petróleo",
    x = "Producción de petróleo (bbl)",
    y = "Frecuencia"
  ) +
  theme_minimal()
# Es muy dificil interpretar la gráfica anterior
#Aplicación de logaritmo para lograr mejor vista
df_oil_positivo <- df_oil_positivo %>%
  mutate(
    LOG_OIL_PRODUCED =
      log1p(OIL_PRODUCED_BBL)
  )
#Histograma distribucion logaritmica de produccion positiva petroleo
ggplot(
  df_oil_positivo,
  aes(x = LOG_OIL_PRODUCED)
) +
  geom_histogram(
    bins = 40,
    fill = "darkorange",
    color = "white"
  ) +
  labs(
    title = "Distribución logarítmica de la producción de petróleo",
    x = "log(1 + producción de petróleo)",
    y = "Frecuencia"
  ) +
  theme_minimal()
#BOX PLOT PERO NO SIRVE 
ggplot(
  df_oil_positivo,
  aes(y = log1p(OIL_PRODUCED_BBL))
) +
  geom_boxplot(
    fill = "blue",
    color = "darkorange"
  ) +
  labs(
    title = "Producción positiva en escala logarítmica",
    y = "log(1 + producción de petróleo)",
    x = NULL
  ) +
  theme_minimal()

#CÁLCULO DE ASIMETRÍA #ASIMETRÍA ORIGINAL = 16.15
calcular_asimetria <- function(x) {
  x <- x[!is.na(x)]
  
  n <- length(x)
  media <- mean(x)
  desviacion <- sd(x)
  
  (n / ((n - 1) * (n - 2))) *
    sum(((x - media) / desviacion)^3)
}

asimetria_original <- calcular_asimetria(
  df_oil_positivo$OIL_PRODUCED_BBL
)

asimetria_original
#ASIMETRÍA CON ESCALA LOG = 0.346
asimetria_log <- calcular_asimetria(
  log1p(df_oil_positivo$OIL_PRODUCED_BBL)
)

asimetria_log

#comparación
data.frame(
  DISTRIBUCION = c("Original", "Logarítmica"),
  ASIMETRIA = c(
    asimetria_original,
    asimetria_log
  )
)

# DEFINITIVAMENTE NO ME GUSTAN ESTOS GRAFICOS
box_original <- ggplot(
  df_oil_positivo,
  aes(y = OIL_PRODUCED_BBL)
) +
  geom_boxplot(
    fill = "steelblue",
    alpha = 0.8
  ) +
  labs(
    title = "Escala original",
    x = NULL,
    y = "Producción de petróleo (bbl)"
  ) +
  theme_minimal()

box_log <- ggplot(
  df_oil_positivo,
  aes(y = log1p(OIL_PRODUCED_BBL))
) +
  geom_boxplot(
    fill = "darkorange",
    alpha = 0.8
  ) +
  labs(
    title = "Escala logarítmica",
    x = NULL,
    y = "log(1 + producción)"
  ) +
  theme_minimal()

box_original
box_log

# PARA EL SIGUIENTE ANÁLISIS
#VAMOS A USAR SOLO DATOS CON PRODUCCIÓN Y POZOS ACTIVOS
df_relacion_oil <- datos_limpios %>%
  filter(
    OIL_PRODUCED_BBL > 0,
    ACTIVE_OIL_WELLS > 0
  )
#RECTIFICACIÓN DEL CONTENIDO DE LOS  DATOS A ANALIZAR
df_relacion_oil %>%
  summarise(
    registros_analizados = n(),
    
    minimo_pozos =
      min(ACTIVE_OIL_WELLS),
    
    maximo_pozos =
      max(ACTIVE_OIL_WELLS),
    
    minimo_produccion =
      min(OIL_PRODUCED_BBL),
    
    maximo_produccion =
      max(OIL_PRODUCED_BBL)
  )
#Diagrama de dispersión original
library("ggplot2")

ggplot(
  df_relacion_oil,
  aes(
    x = ACTIVE_OIL_WELLS,
    y = OIL_PRODUCED_BBL
  )
) +
  geom_point(
    color = "steelblue",
    alpha = 0.35
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red"
  ) +
  labs(
    title = paste(
      "Relación entre pozos activos",
      "y producción de petróleo"
    ),
    x = "Pozos petroleros activos",
    y = "Producción de petróleo (bbl)"
  ) +
  theme_minimal()
#Diagrama de dispersión ESCALA LOGARITMICA
ggplot(
  df_relacion_oil,
  aes(
    x = ACTIVE_OIL_WELLS,
    y = OIL_PRODUCED_BBL
  )
) +
  geom_point(
    color = "darkorange",
    alpha = 0.35
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "blue"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = paste(
      "Relación entre pozos activos",
      "y producción de petróleo"
    ),
    subtitle = "Escala logarítmica en ambos ejes",
    x = "Pozos activos (escala logarítmica)",
    y = "Producción en bbl (escala logarítmica)"
  ) +
  theme_minimal()
max(df_relacion_oil$ACTIVE_OIL_WELLS)

#CALCULO CORRELACION PEARSON= 0.6636, VALOR QUE SE INTERPRETA
#COMO UNA CORRELACION FUERTE
cor_pearson_original <- cor(
  df_relacion_oil$ACTIVE_OIL_WELLS,
  df_relacion_oil$OIL_PRODUCED_BBL,
  method = "pearson",
  use = "complete.obs"
)
cor_pearson_original
#TRANSFORMAR VARIABLES A LOG
df_relacion_oil <- df_relacion_oil %>%
  mutate(
    LOG_ACTIVE_WELLS =
      log1p(ACTIVE_OIL_WELLS),
    
    LOG_OIL_PRODUCED =
      log1p(OIL_PRODUCED_BBL)
  )
#calculo de correlacion pearson con logaritmos = 0.5951
# VALOR QUE CASI SE PUEDE INTERPRETAR COMO MODERADA-FUERTE
cor_pearson_log <- cor(
  df_relacion_oil$LOG_ACTIVE_WELLS,
  df_relacion_oil$LOG_OIL_PRODUCED,
  method = "pearson",
  use = "complete.obs"
)
#cálculo de coeficiente Pearson
cor_pearson_log
#cORRLEACION SPEARMAN
cor_spearman <- cor(
  df_relacion_oil$ACTIVE_OIL_WELLS,
  df_relacion_oil$OIL_PRODUCED_BBL,
  method = "spearman",
  use = "complete.obs"
)

cor_spearman


#######################################################
#Correlación datos producción oil producción gas
######################################################
df_corr <- datos %>%
  transmute(
    PRODUCCION_PETROLEO = `Oil Produced, bbl`,
    PRODUCCION_GAS = `Gas Produced, Mcf`
  ) %>%
  filter(
    !is.na(PRODUCCION_PETROLEO),
    !is.na(PRODUCCION_GAS),
    PRODUCCION_PETROLEO > 0,
    PRODUCCION_GAS > 0
  )

corr_pearson <- cor(
  df_corr$PRODUCCION_PETROLEO,
  df_corr$PRODUCCION_GAS,
  method = "pearson"
)

corr_pearson

prueba_correlacion <- cor.test(
  df_corr$PRODUCCION_PETROLEO,
  df_corr$PRODUCCION_GAS,
  method = "pearson"
)

prueba_correlacion

########
#Gráfica de dispersión petroleo gas
library(ggplot2)

ggplot(
  df_corr,
  aes(
    x = PRODUCCION_PETROLEO,
    y = PRODUCCION_GAS
  )
) +
  geom_point(
    alpha = 0.3,
    color = "steelblue"
  ) +
  labs(
    title = "Producción de petróleo y producción de gas",
    subtitle = "Registros con ambas producciones mayores que cero",
    x = "Petróleo producido (bbl)",
    y = "Gas producido (Mcf)"
  ) +
  theme_minimal()

#######################################################
#En los registros que producen simultáneamente petróleo y gas, 
#¿existe asociación entre los volúmenes producidos de ambos hidrocarburos?
#######################################################
#CREAR COLUMNA CON TIPO DE PRODUCCIÓN
datos_limpios <- datos_limpios %>%
  mutate(
    TIPO_PRODUCCION = case_when(
      OIL_PRODUCED_BBL > 0 &
        GAS_PRODUCED_MCF > 0 ~ "OIL N GAS",
      
      OIL_PRODUCED_BBL > 0 &
        GAS_PRODUCED_MCF == 0 ~ "OIL",
      
      OIL_PRODUCED_BBL == 0 &
        GAS_PRODUCED_MCF > 0 ~ "GAS",
      
      OIL_PRODUCED_BBL == 0 &
        GAS_PRODUCED_MCF == 0 ~ "NO PRODUCTION",
      
      TRUE ~ NA_character_
    )
  )
# porcentaje de datos con produccion oil n gas = 4.12%
datos_limpios %>%
  count(TIPO_PRODUCCION) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100,
      2
    )
  )

###############################
library(ggplot2)

resumen_produccion <- datos_limpios %>%
  count(TIPO_PRODUCCION) %>%
  filter(!is.na(TIPO_PRODUCCION)) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100,
      2
    ),
    etiqueta = paste0(
      porcentaje,
      "%"
    )
  )

ggplot(
  resumen_produccion,
  aes(
    x = "",
    y = n,
    fill = TIPO_PRODUCCION
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  labs(
    title = "Distribución por tipo de producción",
    fill = "Tipo de producción"
  ) +
  theme_void()
###############################

#subconjunto de  datos
df_oil_gas <- datos_limpios %>%
  filter(
    OIL_PRODUCED_BBL > 0,
    GAS_PRODUCED_MCF > 0
  )
# revisión de AÑOS de la producción
df_oil_gas %>%
  count(PRODUCTION_YEAR) %>%
  arrange(PRODUCTION_YEAR)
#PRINCIPALES CAMPOS PRODUCTORES
df_oil_gas %>%
  count(Field, sort = TRUE) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100,
      2
    )
  ) %>%
  slice_head(n = 10)
#CORRELACIONES OIL  + GAS
pearson_original <- cor(
  df_oil_gas$OIL_PRODUCED_BBL,
  df_oil_gas$GAS_PRODUCED_MCF,
  method = "pearson",
  use = "complete.obs"
)
pearson_original
spearman <- cor(
  df_oil_gas$OIL_PRODUCED_BBL,
  df_oil_gas$GAS_PRODUCED_MCF,
  method = "spearman",
  use = "complete.obs"
)

pearson_log <- cor(
  log1p(df_oil_gas$OIL_PRODUCED_BBL),
  log1p(df_oil_gas$GAS_PRODUCED_MCF),
  method = "pearson",
  use = "complete.obs"
)

data.frame(
  METODO = c(
    "Pearson original",
    "Spearman",
    "Pearson logarítmico"
  ),
  CORRELACION = c(
    pearson_original,
    spearman,
    pearson_log
  )
)
# GRÁFICOS DE DISPERSIÓN OIL + GAS
library('ggplot2')

ggplot(
  df_oil_gas,
  aes(
    x = OIL_PRODUCED_BBL,
    y = GAS_PRODUCED_MCF
  )
) +
  geom_point(
    color = "darkgreen",
    alpha = 0.35
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = paste(
      "Relación entre la producción",
      "de petróleo y gas"
    ),
    subtitle = paste(
      "Registros con producción positiva",
      "de ambos hidrocarburos"
    ),
    x = "Producción de petróleo (bbl, escala logarítmica)",
    y = "Producción de gas (Mcf, escala logarítmica)"
  ) +
  theme_minimal()
######################################################
#REGRESION LINEAL ESCALA LOGARITMICA PRODUCCION PETROLEO GAS
######################################################
modelo_log <- lm(
  log10(PRODUCCION_GAS) ~ log10(PRODUCCION_PETROLEO),
  data = df_corr
)

summary(modelo_log)


coef(modelo_log)

confint(
  modelo_log,
  level = 0.95
)

library(ggplot2)

ggplot(
  df_corr,
  aes(
    x = log10(PRODUCCION_PETROLEO),
    y = log10(PRODUCCION_GAS)
  )
) +
  geom_point(
    alpha = 0.35,
    color = "steelblue"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red"
  ) +
  labs(
    title = "Regresión entre producción de petróleo y gas",
    subtitle = "Modelo lineal en escala logarítmica",
    x = "log10 de petróleo producido (bbl)",
    y = "log10 de gas producido (Mcf)"
  ) +
  theme_minimal()

par(mfrow = c(2, 2))
plot(modelo_log)
par(mfrow = c(1, 1))

coef(modelo_log)
confint(modelo_log)
#######################################################
#¿Existen diferencias significativas en la producción 
# de petróleo entre las formaciones productoras?
#######################################################

#PREPARACIÓN DE  DATOS
df_anova_oil <- datos_limpios %>%
  filter(
    !is.na(PRODUCING_FORMATION),
    OIL_PRODUCED_BBL > 0
  ) %>%
  group_by(PRODUCING_FORMATION) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  mutate(
    PRODUCING_FORMATION =
      factor(PRODUCING_FORMATION),
  )
#LISTADO DE FORMACIONES A ANALIZAR
df_anova_oil %>%
  count(
    PRODUCING_FORMATION,
    sort = TRUE
  )
#ESTADÍSTICA DESCRIPTIVA POR FORMACIÓN
resumen_formaciones_oil <- df_anova_oil %>%
  group_by(PRODUCING_FORMATION) %>%
  summarise(
    registros = n(),
    
    produccion_total =
      sum(OIL_PRODUCED_BBL),
    
    media =
      mean(OIL_PRODUCED_BBL),
    
    mediana =
      median(OIL_PRODUCED_BBL),
    
    desviacion =
      sd(OIL_PRODUCED_BBL),
    
    Q1 =
      quantile(OIL_PRODUCED_BBL, 0.25),
    
    Q3 =
      quantile(OIL_PRODUCED_BBL, 0.75),
    
    .groups = "drop"
  ) %>%
  arrange(desc(mediana))

resumen_formaciones_oil


df_anova_oil <- df_anova_oil %>%
  mutate(
    LOG_ACTIVE_WELLS =
      log1p(ACTIVE_OIL_WELLS),
    
    LOG_OIL_PRODUCED =
      log1p(OIL_PRODUCED_BBL)
  )
#gráfico boxplot por formación
library(ggplot2)

ggplot(
  df_anova_oil,
  aes(
    x = reorder(
      PRODUCING_FORMATION,
      LOG_OIL_PRODUCED,
      FUN = median
    ),
    y = LOG_OIL_PRODUCED
  )
) +
  geom_boxplot(
    fill = "steelblue",
    alpha = 0.75,
    outlier.alpha = 0.25
  ) +
  coord_flip() +
  labs(
    title = paste(
      "Producción de petróleo",
      "por formación productora"
    ),
    subtitle = paste(
      "Formaciones con al menos 30 registros",
      "de producción positiva"
    ),
    x = "Formación productora",
    y = "log(1 + producción de petróleo)"
  ) +
  theme_minimal()
###########################################
#REVISIÓN ANIO VS PRODUCCION
#sELECCIÓN DE DATOS CON PRODUCCIÓN
produccion_anual <- datos %>%
  filter(
    !is.na(`Production Year`),
    !is.na(`Oil Produced, bbl`)
  ) %>%
  group_by(
    ANIO = `Production Year`
  ) %>%
  summarise(
    PRODUCCION_TOTAL = sum(
      `Oil Produced, bbl`,
      na.rm = TRUE
    ),
    NUMERO_REGISTROS = n(),
    .groups = "drop"
  )
produccion_anual

#GRAFICO
ggplot(
  produccion_anual,
  aes(
    x = ANIO,
    y = PRODUCCION_TOTAL
  )
) +
  geom_point(
    size = 2,
    color = "steelblue"
  ) +
  geom_line(
    color = "steelblue"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red"
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",")
  ) +
  labs(
    title = "Producción total anual de petróleo",
    subtitle = "Periodo 1967–1999",
    x = "Año",
    y = "Producción total (bbl)"
  ) +
  theme_minimal()
#REGRESION LINEAL
modelo_produccion_anual <- lm(
  PRODUCCION_TOTAL ~ ANIO,
  data = produccion_anual
)

summary(modelo_produccion_anual)
coef(modelo_produccion_anual)
confint(modelo_produccion_anual)


b0 <- coef(modelo_produccion_anual)[1]
b1 <- coef(modelo_produccion_anual)[2]

prediccion_1990 <- b0 + b1 * 1990
prediccion_2000 <- b0 + b1 * 2000

ultimo_anio <- max(
  produccion_anual$ANIO,
  na.rm = TRUE
)

anios_prediccion <- data.frame(
  ANIO = seq(
    min(produccion_anual$ANIO),
    2010,
    by = 1
  )
)

intervalos <- predict(
  modelo_produccion_anual,
  newdata = anios_prediccion,
  interval = "prediction",
  level = 0.95
)

anios_prediccion <- bind_cols(
  anios_prediccion,
  as.data.frame(intervalos)
)

########
ggplot() +
  # Intervalo de predicción
  geom_ribbon(
    data = anios_prediccion,
    aes(
      x = ANIO,
      ymin = lwr,
      ymax = upr
    ),
    fill = "red",
    alpha = 0.15
  ) +
  
  # Producción observada
  geom_line(
    data = produccion_anual,
    aes(
      x = ANIO,
      y = PRODUCCION_TOTAL
    ),
    color = "steelblue"
  ) +
  
  geom_point(
    data = produccion_anual,
    aes(
      x = ANIO,
      y = PRODUCCION_TOTAL
    ),
    color = "steelblue",
    size = 2
  ) +
  
  # Recta estimada y pronóstico
  geom_line(
    data = anios_prediccion,
    aes(
      x = ANIO,
      y = fit
    ),
    color = "red",
    linewidth = 1
  ) +
  
  # Separación entre observación y pronóstico
  geom_vline(
    xintercept = ultimo_anio,
    linetype = "dashed",
    color = "gray40"
  ) +
  
  # Referencia de producción igual a cero
  geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(big.mark = ",")
  ) +
  
  labs(
    title = "Producción total anual de petróleo",
    subtitle = "Datos observados 1967–1999 y proyección hasta 2010",
    x = "Año",
    y = "Producción total (bbl)",
    caption = paste(
      "La línea vertical marca el final de los datos observados.",
      "La franja representa el intervalo de predicción del 95 %."
    )
  ) +
  theme_minimal()



###################################
#ANOVA DE UNA VIA
#######################################
#Modelo  ANOVA

library('dplyr')
library('ggplot2')
#FILTRO PARA EVITAR NA
df_anova_oil <- df_anova_oil %>%
  filter(
    !is.na(LOG_OIL_PRODUCED),
    !is.na(PRODUCING_FORMATION)
  ) %>%
  mutate(
    PRODUCING_FORMATION =
      factor(PRODUCING_FORMATION)
  )
#REVISION DE GRUPOS
df_anova_oil %>%
  count(
    PRODUCING_FORMATION,
    sort = TRUE
  )
#MODELO ANOVA
modelo_anova_oil <- aov(
  LOG_OIL_PRODUCED ~ PRODUCING_FORMATION,
  data = df_anova_oil
)

tabla_anova <- summary(
  modelo_anova_oil
)[[1]]

#eta cuadrada
eta_cuadrada <-
  tabla_anova[
    "PRODUCING_FORMATION",
    "Sum Sq"
  ] /
  sum(tabla_anova[, "Sum Sq"])

eta_cuadrada
summary(modelo_anova_oil)

#ANOVA DE WELCH
resultado_welch <- oneway.test(
  LOG_OIL_PRODUCED ~ PRODUCING_FORMATION,
  data = df_anova_oil,
  var.equal = FALSE
)

resultado_welch

#comparaciones Welch
comparaciones_welch <- pairwise.t.test(
  x = df_anova_oil$LOG_OIL_PRODUCED,
  g = df_anova_oil$PRODUCING_FORMATION,
  p.adjust.method = "holm",
  pool.sd = FALSE
)

comparaciones_welch

########################
resumen_grafico <- df_anova_oil %>%
  group_by(PRODUCING_FORMATION) %>%
  summarise(
    N = n(),
    MEDIA_LOG = mean(
      LOG_OIL_PRODUCED,
      na.rm = TRUE
    ),
    DESVIACION = sd(
      LOG_OIL_PRODUCED,
      na.rm = TRUE
    ),
    ERROR_ESTANDAR =
      DESVIACION / sqrt(N),
    LIMITE_INFERIOR =
      MEDIA_LOG -
      qt(0.975, df = N - 1) *
      ERROR_ESTANDAR,
    LIMITE_SUPERIOR =
      MEDIA_LOG +
      qt(0.975, df = N - 1) *
      ERROR_ESTANDAR,
    .groups = "drop"
  ) %>%
  filter(N >= 2)
############
ggplot(
  resumen_grafico,
  aes(
    x = reorder(
      PRODUCING_FORMATION,
      MEDIA_LOG
    ),
    y = MEDIA_LOG
  )
) +
  geom_point(
    size = 3,
    color = "steelblue"
  ) +
  geom_errorbar(
    aes(
      ymin = LIMITE_INFERIOR,
      ymax = LIMITE_SUPERIOR
    ),
    width = 0.20,
    color = "steelblue"
  ) +
  coord_flip() +
  labs(
    title = "Producción media de petróleo por formación",
    subtitle = paste(
      "Media logarítmica e intervalos",
      "de confianza del 95 %"
    ),
    x = "Formación productora",
    y = "Media de log(1 + producción)"
  ) +
  theme_minimal()
#################################################
#segundo analisis ANOVA por operador
#variable dependiente log produccion oil
#factor operador

###############################################
library(dplyr)

formaciones_candidatas <- datos_limpios %>%
  filter(
    !is.na(PRODUCING_FORMATION),
    !is.na(OPERATOR_CLAVE),
    OIL_PRODUCED_BBL > 0
  ) %>%
  group_by(PRODUCING_FORMATION) %>%
  summarise(
    REGISTROS = n(),
    OPERADORES = n_distinct(OPERATOR_CLAVE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(OPERADORES),
    desc(REGISTROS)
  )

formaciones_candidatas
#conservaro operadores  con almenos 5 registros
df_anova_operador <- datos_limpios %>%
  filter(
    PRODUCING_FORMATION == 'CHIPMUNK',
    !is.na(OPERATOR_CLAVE),
    !is.na(OIL_PRODUCED_BBL),
    OIL_PRODUCED_BBL > 0
  ) %>%
  transmute(
    OPERADOR = factor(OPERATOR_CLAVE),
    PRODUCCION_OIL = OIL_PRODUCED_BBL,
    LOG_OIL_PRODUCED = log1p(OIL_PRODUCED_BBL)
  ) %>%
  add_count(
    OPERADOR,
    name = "N_OPERADOR"
  ) %>%
  filter(N_OPERADOR >= 5) %>%
  droplevels()
#COMPROBAR OPERADORES CON SUS REGISTROS DE LA FORMACION RICHBURG, BRADFORD, CHIPMUNK
df_anova_operador %>%
  count(
    OPERADOR,
    sort = TRUE
  )
#
modelo_anova_operador <- aov(
  LOG_OIL_PRODUCED ~ OPERADOR,
  data = df_anova_operador
)

summary(modelo_anova_operador)

#ETA CUADRADA
tabla_anova <- summary(
  modelo_anova_operador
)[[1]]

eta_cuadrada_operador <-
  tabla_anova["OPERADOR", "Sum Sq"] /
  sum(tabla_anova[, "Sum Sq"])

eta_cuadrada_operador

#welch operador
welch_operador <- oneway.test(
  LOG_OIL_PRODUCED ~ OPERADOR,
  data = df_anova_operador,
  var.equal = FALSE
)

welch_operador
###########################################################
#H₀: todos los operadores tienen la misma producción media
#de petróleo dentro de la formación seleccionada.
#
#H₁: al menos un operador presenta una media diferente.
###########################################################
comparaciones_operadores <- pairwise.t.test(
  x = df_anova_operador$LOG_OIL_PRODUCED,
  g = df_anova_operador$OPERADOR,
  p.adjust.method = "holm",
  pool.sd = FALSE
)

comparaciones_operadores

library('dplyr')
library('ggplot2')

operadores_principales <- df_anova_operador %>%
  count(OPERADOR, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(OPERADOR)

datos_grafico_operador <- df_anova_operador %>%
  filter(
    OPERADOR %in% operadores_principales
  )

ggplot(
  datos_grafico_operador,
  aes(
    x = reorder(
      OPERADOR,
      LOG_OIL_PRODUCED,
      FUN = median
    ),
    y = LOG_OIL_PRODUCED
  )
) +
  geom_boxplot(
    fill = "steelblue",
    alpha = 0.7,
    outlier.alpha = 0.4
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.2,
    size = 1
  ) +
  coord_flip() +
  labs(
    title = "Producción de petróleo por operador",
    subtitle = paste(
      "Los 15 operadores con más registros en",
      'CHIPMUNK'
    ),
    x = "Operador",
    y = "log(1 + producción de petróleo)"
  ) +
  theme_minimal()

####################################################
##Análisis KMEANS
#
#################################################
#Selección de datos
###############################################

datos_kmeans <- datos_limpios %>%
  filter(
    !is.na(OPERATOR_CLAVE)
  ) %>%
  group_by(
    OPERADOR = OPERATOR_CLAVE
  ) %>%
  summarise(
    PRODUCCION_OIL = sum(
      OIL_PRODUCED_BBL,
      na.rm = TRUE
    ),
    PRODUCCION_GAS = sum(
      GAS_PRODUCED_MCF,
      na.rm = TRUE
    ),
    PRODUCCION_AGUA = sum(
      WATER_PRODUCED_BBL,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(
    PRODUCCION_OIL +
      PRODUCCION_GAS +
      PRODUCCION_AGUA > 0
  )
#Aplicar Log
variables_kmeans <- datos_kmeans %>%
  transmute(
    OIL_LOG = log1p(PRODUCCION_OIL),
    GAS_LOG = log1p(PRODUCCION_GAS),
    AGUA_LOG = log1p(PRODUCCION_AGUA)
  )

datos_escalados <- scale(
  variables_kmeans
)
####################################################
##Análisis KMEANS
###KMEANS CON VARIABLES ORIGINALES
variables_normales <- datos_kmeans %>%
  select(PRODUCCION_OIL, PRODUCCION_GAS, PRODUCCION_AGUA)

datos_escalados_normal <- scale(
  variables_normales
)

set.seed(123)

wcss_normal <- numeric(10)

for (k in 1:10) {
  modelo <- kmeans(
    datos_escalados_normal,
    centers = k,
    nstart = 25
  )
  
  wcss_normal[k] <- modelo$tot.withinss
}

plot(
  1:10,
  wcss_normal,
  type = "b",
  pch = 19,
  xlab = "Número de clústeres (K)",
  ylab = "WCSS",
  main = "Método del codo: datos originales"
)
#AL PARECER SON 3 CLUSTERS LOS ADECUADOS
set.seed(123)

modelo_kmeans_normal <- kmeans(
  datos_escalados_normal,
  centers = 3,
  nstart = 25
)

datos_kmeans$CLUSTER_NORMAL <-
  factor(modelo_kmeans_normal$cluster)

datos_kmeans %>%
  group_by(CLUSTER_NORMAL) %>%
  summarise(
    OPERADORES = n(),
    OIL_PROMEDIO = mean(PRODUCCION_OIL),
    GAS_PROMEDIO = mean(PRODUCCION_GAS),
    AGUA_PROMEDIO = mean(PRODUCCION_AGUA),
    .groups = "drop"
  )

###grafica
centros_normales <- datos_kmeans %>%
  group_by(CLUSTER_NORMAL) %>%
  summarise(
    PRODUCCION_OIL = mean(PRODUCCION_OIL),
    PRODUCCION_GAS = mean(PRODUCCION_GAS),
    .groups = "drop"
  )

ggplot(
  datos_kmeans,
  aes(
    x = PRODUCCION_OIL,
    y = PRODUCCION_GAS,
    color = CLUSTER_NORMAL,
    size = PRODUCCION_AGUA
  )
) +
  geom_point(
    alpha = 0.60
  ) +
  geom_point(
    data = centros_normales,
    aes(
      x = PRODUCCION_OIL,
      y = PRODUCCION_GAS,
      color = CLUSTER_NORMAL
    ),
    shape = 4,
    size = 5,
    stroke = 2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Segmentación de operadores mediante K-means",
    subtitle = "Variables originales estandarizadas; K = 3",
    x = "Producción total de petróleo (bbl)",
    y = "Producción total de gas (Mcf)",
    color = "Clúster",
    size = "Producción de agua"
  ) +
  theme_minimal()
####################################################
##Análisis KMEANS
###KMEANS CON VARIABLES LOG
set.seed(123)

wcss_log <- numeric(10)

for (k in 1:10) {
  modelo <- kmeans(
    datos_escalados,
    centers = k,
    nstart = 25
  )
  
  wcss_log[k] <- modelo$tot.withinss
}

plot(
  1:10,
  wcss_log,
  type = "b",
  pch = 19,
  xlab = "Número de clústeres (K)",
  ylab = "WCSS",
  main = "Método del codo: variables logarítmicas"
)

#3 CLUSTERS
set.seed(123)

modelo_kmeans_log <- kmeans(
  datos_escalados,
  centers = 4,
  nstart = 25
)

datos_kmeans$CLUSTER_LOG <-
  factor(modelo_kmeans_log$cluster)

table(datos_kmeans$CLUSTER_LOG)

modelo_kmeans_log$centers

resumen_clusters_log <- datos_kmeans %>%
  group_by(CLUSTER_LOG) %>%
  summarise(
    OPERADORES = n(),
    OIL_PROMEDIO = mean(PRODUCCION_OIL),
    GAS_PROMEDIO = mean(PRODUCCION_GAS),
    AGUA_PROMEDIO = mean(PRODUCCION_AGUA),
    .groups = "drop"
  )

resumen_clusters_log

datos_kmeans <- datos_kmeans %>%
  mutate(
    OIL_LOG = variables_kmeans$OIL_LOG,
    GAS_LOG = variables_kmeans$GAS_LOG,
    AGUA_LOG = variables_kmeans$AGUA_LOG
  )

centros_log <- datos_kmeans %>%
  group_by(CLUSTER_LOG) %>%
  summarise(
    OIL_LOG = mean(OIL_LOG),
    GAS_LOG = mean(GAS_LOG),
    .groups = "drop"
  )

library(ggplot2)
#grafica

ggplot(
  datos_kmeans,
  aes(
    x = OIL_LOG,
    y = GAS_LOG,
    color = CLUSTER_LOG,
    size = AGUA_LOG
  )
) +
  geom_point(
    alpha = 0.60
  ) +
  geom_point(
    data = centros_log,
    aes(
      x = OIL_LOG,
      y = GAS_LOG,
      color = CLUSTER_LOG
    ),
    shape = 4,
    size = 5,
    stroke = 2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Segmentación de operadores mediante K-means",
    subtitle = "Variables logarítmicas estandarizadas; K = 4",
    x = "log(1 + producción de petróleo)",
    y = "log(1 + producción de gas)",
    color = "Clúster",
    size = "log(1 + producción de agua)"
  ) +
  theme_minimal()