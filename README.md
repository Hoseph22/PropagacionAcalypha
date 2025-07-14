# PropagacionAcalypha

# ANALISIS UNIVARIADO
#1) LIBRERÍAS
#Instala si no las tienes
#install.packages("googlesheets4")
#install.packages("agricolae")
#install.packages("ggplot2")
#install.packages("dplyr")

library(googlesheets4)
library(agricolae)
library(ggplot2)
library(dplyr)

#2) AUTENTICACIÓN Y DATOS 
#Autenticación a tu cuenta Google
gs4_auth()

#URL de tu Google Sheets
url <-"https://docs.google.com/spreadsheets/d/1PSIhPrPvb76q80dG1OWw04p0YnlM7r9SDAhVRDLs4nA/edit?usp=drive_link"

#Leer hoja específica (ajusta nombre y filas a saltar)
datos <- read_sheet(url, sheet = "Hoja1", skip = 1)

#Revisar
head(datos)

#Factores
datos$Tratamiento <- as.factor(datos$Tratamiento)
datos$Fecha <- as.factor(datos$Fecha)

#3) VARIABLES 
variables_df1 <- c("NH", "DB", "PHB","PSB", "LB")
variables_df2 <- c("NN", "LR", "PHR","PSR", "DR")
variables_todas <- c(variables_df1, variables_df2)

nombres_facet <- c(
  "NH" = "Número de hojas",
  "DB" = "Diámetro de brote (cm)",
  "PHB" = "Peso húmedo de brote (g)",
  "PSB" = "Peso seco de brote (g)",
  "LB" = "Longitud de brote (cm)",
  "NN" = "Número de nudos",
  "LR" = "Longitud de raíz (cm)",
  "PHR" = "Peso húmedo de raíz (g)",
  "PSR" = "Peso seco de raíz (g)",
  "DR" = "Diámetro de raíz (cm)"
)

#4) ANOVA + LSD 
letras_LSD <- data.frame()

for (var in variables_todas) {
  for (fecha in c("28", "60")) {
    datos_fecha <- datos %>% filter(Fecha == fecha)
    modelo <- aov(as.formula(paste(var, "~ Tratamiento")), data = datos_fecha)
    
    print(paste0("========= Variable: ", var, " / Fecha: ", fecha, " ========="))
    print(summary(modelo))
    
    LSD <- LSD.test(modelo, "Tratamiento", p.adj = "none")
    print(LSD)
    print(LSD$groups)
    
    grupos <- LSD$groups
    grupos$Tratamiento <- rownames(grupos)
    grupos$Variable <- var
    grupos$Fecha <- fecha
    letras_LSD <- bind_rows(letras_LSD, grupos)
  }
}

#5) RESUMEN + UNIR LETRAS 
resumen <- data.frame()

for (var in variables_todas) {
  temp <- datos %>%
    filter(Fecha %in% c("28", "60")) %>%
    group_by(Fecha, Tratamiento) %>%
    summarise(
      media = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      Variable = var,
      .groups = "drop"
    )
  resumen <- bind_rows(resumen, temp)
}

plot_data <- resumen %>%
  left_join(letras_LSD %>% select(Tratamiento, Fecha, Variable, groups),
            by = c("Tratamiento", "Fecha", "Variable"))

plot_data$FacetLabel <- nombres_facet[plot_data$Variable]

ajuste_letras <- c(
  "NH" = 0.02,
  "DB" = 0.005,
  "PHB" = 0.005,
  "PSB" = 0.005,
  "LB" = 0.03,
  "NN" = 0.0025,
  "LR" = 0.03,
  "PHR" = 0.005,
  "PSR" = 0.005,
  "DR" = 0.0025
)

plot_data$AjusteLetras <- ajuste_letras[plot_data$Variable]

df1 <- plot_data %>% filter(Variable %in% variables_df1)
df2 <- plot_data %>% filter(Variable %in% variables_df2)

#6) LETRAS POR SUBGRÁFICO 
letras_facets_df1 <- data.frame(
  Variable = variables_df1,
  FacetLabel = nombres_facet[variables_df1],
  LetraFacet = c("c", "a", "d", "e", "b")
)

letras_facets_df2 <- data.frame(
  Variable = variables_df2,
  FacetLabel = nombres_facet[variables_df2],
  LetraFacet = c("c", "b", "d", "e", "a")
)

coordenadas_letras_df1 <- df1 %>%
  group_by(FacetLabel) %>%
  summarise(
    x = 0.5,
    y = max(media + se, na.rm = TRUE) * 1.1
  ) %>%
  left_join(letras_facets_df1, by = "FacetLabel")

coordenadas_letras_df2 <- df2 %>%
  group_by(FacetLabel) %>%
  summarise(
    x = 0.5,
    y = max(media + se, na.rm = TRUE) * 1.1
  ) %>%
  left_join(letras_facets_df2, by = "FacetLabel")

#7) THEME PERSONALIZADO 
windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))

theme_bordes <- theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      angle = 90,
      hjust = 0.5,
      size = 14,
      family = "Times New Roman"
    ),
    plot.title = element_blank(),
    axis.title = element_text(size = 12, family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 12, family = "Times New Roman"),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    text = element_text(family = "Times New Roman"),
    panel.spacing = unit(1.2, "cm"),
    plot.margin = margin(5, 5, 5, 5)
  )

#8) ORDENAR TRATAMIENTOS 
datos$Tratamiento <- factor(datos$Tratamiento,
                            levels = c("T1", "T2", "T3", "T4")) 

#9) GRAFICO GRUPO 1 
gg1 <- ggplot(df1, aes(
  x = Tratamiento, y = media,
  group = Fecha,
  color = Fecha
)) +
  geom_line(position = position_dodge(width = 0.2), size = 1) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(
    aes(ymin = media - se, ymax = media + se),
    width = 0.1,
    position = position_dodge(width = 0.2)
  ) +
  geom_text(
    aes(
      label = groups,
      y = media + se + AjusteLetras * max(media, na.rm = TRUE)
    ),
    position = position_dodge(width = 0.2),
    size = 5,
    family = "Times New Roman"
  ) +
  facet_wrap(
    ~ FacetLabel,
    scales = "free_y",
    strip.position = "left",
    ncol = 2,
    nrow = 3
  ) +
  scale_x_discrete(drop = FALSE) +
  labs(
    x = "Tratamientos",
    y = NULL,
    color = "Fecha"
  ) +
  theme_bordes +
  theme(
    legend.position = c(0.8, 0.1),
    legend.justification = c(1, 0),
    legend.box = "vertical",
    legend.spacing.y = unit(0.3, 'cm'), 
    axis.title.x = element_text(size = 15, family = "Times New Roman")
  ) +
  scale_color_manual(
    values = c("28" = "#1b9e77", "60" = "#d95f02"),
    labels = c("28" = "28 dds", "60" = "60 dds"),
    name = "Fecha"
  ) +
  geom_text(
    data = coordenadas_letras_df1,
    aes(x = x, y = y, label = LetraFacet),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,
    size = 5, family = "Times New Roman", fontface = "bold"
  )

#10) GRAFICO GRUPO 2 
gg2 <- ggplot(df2, aes(
  x = Tratamiento, y = media,
  group = Fecha,
  color = Fecha
)) +
  geom_line(position = position_dodge(width = 0.2), size = 1) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(
    aes(ymin = media - se, ymax = media + se),
    width = 0.1,
    position = position_dodge(width = 0.2)
  ) +
  geom_text(
    aes(
      label = groups,
      y = media + se + AjusteLetras * max(media, na.rm = TRUE)
    ),
    position = position_dodge(width = 0.2),
    size = 5,
    family = "Times New Roman"
  ) +
  facet_wrap(
    ~ FacetLabel,
    scales = "free_y",
    strip.position = "left",
    ncol = 2,
    nrow = 3
  ) +
  scale_x_discrete(drop = FALSE) +
  labs(
    x = "Tratamientos",
    y = NULL,
    color = "Fecha"
  ) +
  theme_bordes +
  theme(
    legend.position = c(0.8, 0.1),
    legend.justification = c(1, 0),
    legend.box = "vertical",
    legend.spacing.y = unit(0.3, 'cm'), 
    axis.title.x = element_text(size = 15, family = "Times New Roman")
  ) +
  scale_color_manual(
    values = c("28" = "#7570b3", "60" = "#e7298a"),
    labels = c("28" = "28 dds", "60" = "60 dds"),
    name = "Fecha"
  ) +
  geom_text(
    data = coordenadas_letras_df2,
    aes(x = x, y = y, label = LetraFacet),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,
    size = 5, family = "Times New Roman", fontface = "bold"
  )

#11) MOSTRAR 
print(gg1)
print(gg2)


# ANALISIS DE COMPONENTES PRINCIPALES
#1) LIBRERÍAS 
#Instala si no las tienes
#install.packages("googlesheets4")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("showtext")
#install.packages("dplyr")

library(googlesheets4)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(showtext)

#2) AUTENTICACIÓN Y DATOS 
gs4_auth()

#URL de tu Google Sheet
url <- "https://docs.google.com/spreadsheets/d/1PSIhPrPvb76q80dG1OWw04p0YnlM7r9SDAhVRDLs4nA/edit?usp=sharing"

#Leer hoja (ajusta skip si tienes encabezado especial)
datos <- read_sheet(url, sheet = "Hoja1", skip = 1)


#3) VARIABLES Y NIVELES 
variables_df1 <- c("NH", "DB", "PHB", "PSB", "LB")
variables_df2 <- c("NN", "LR", "PHR", "PSR", "DR")
niveles_tratamiento <- c("T0", "T1", "T2", "T3")

#Forzar factor y limpiar espacios
datos$Tratamiento <- factor(trimws(datos$Tratamiento), levels = niveles_tratamiento)

#Verifica
print(table(datos$Tratamiento))

#4) PREPARAR DATOS PCA 

#⚡ Función robusta para asegurar T0
preparar_pca <- function(data, variables, niveles) {
  df <- data %>%
    filter(Tratamiento %in% niveles) %>%
    select(Tratamiento, all_of(variables))
  
  #Reemplaza NA con medias por columna
  df[ , variables] <- lapply(df[ , variables], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  
  #Verifica si T0 está
  if (!"T0" %in% df$Tratamiento) {
    # Agrega fila dummy
    fila_dummy <- df[1, ]
    fila_dummy[ , 2:ncol(fila_dummy)] <- mean(as.matrix(df[ , 2:ncol(df)]), na.rm = TRUE)
    fila_dummy$Tratamiento <- "T0"
    df <- bind_rows(df, fila_dummy)
    cat("⚡ Se agregó fila dummy para T0\n")
  }
  
  df$Tratamiento <- factor(df$Tratamiento, levels = niveles)
  return(df)
}

#⚡ GRUPO 1 y 2
df_pca1 <- preparar_pca(datos, variables_df1, niveles_tratamiento)
df_pca2 <- preparar_pca(datos, variables_df2, niveles_tratamiento)
# Verifica
print(table(df_pca1$Tratamiento))
print(table(df_pca2$Tratamiento))


#5) CONFIGURA FUENTE 
showtext_auto()
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")


#6) PCA GRUPO 1 
pca1 <- PCA(df_pca1, quali.sup = 1, graph = FALSE)

pca1_biplot <- fviz_pca_biplot(
  pca1,
  repel = TRUE,
  col.ind = df_pca1$Tratamiento,
  palette = "jco",
  addEllipses = TRUE,
  label = "var",
  col.var = "#1E1E1E",
  legend.title = "Tratamientos"
) +
  labs(title = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_blank(),
    legend.title = element_text(family = "Times New Roman", size = 13),
    legend.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 13),
    axis.text = element_text(family = "Times New Roman", size = 13),
    strip.text = element_text(family = "Times New Roman")
  )


#7) PCA GRUPO 2 
pca2 <- PCA(df_pca2, quali.sup = 1, graph = FALSE)

pca2_biplot <- fviz_pca_biplot(
  pca2,
  repel = TRUE,
  col.ind = df_pca2$Tratamiento,
  palette = "jco",
  addEllipses = TRUE,
  label = "var",
  col.var = "#1E1E1E",
  legend.title = "Tratamientos"
) +
  labs(title = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_blank(),
    legend.title = element_text(family = "Times New Roman", size = 13),
    legend.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 13),
    axis.text = element_text(family = "Times New Roman", size = 13),
    strip.text = element_text(family = "Times New Roman")
  )

#8) MOSTRAR 
print(pca1_biplot)
print(pca2_biplot)


# MATRIZ DE CORRELACIÓN PEARSON
#1) MATRIZ DE CORRELACIÓN 

#Activa showtext y Times New Roman una sola vez
library(showtext)
showtext_auto()
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")

#Une todas tus variables
variables_todas <- c(variables_df1, variables_df2)

#Filtra solo esas columnas y elimina filas incompletas
datos_cor <- datos %>%
  select(all_of(variables_todas)) %>%
  na.omit()

#Matriz de correlación de Pearson
cor_matrix <- cor(
  datos_cor,
  use = "pairwise.complete.obs",
  method = "pearson"
)

#Verifica
print(round(cor_matrix, 2))

#2) GRAFICAR CON CORRPLOT 

#Si no lo tienes:
#install.packages("corrplot")
library(corrplot)

#Correlograma estilo imagen — con todo en Times New Roman
corrplot(
  cor_matrix,
  method = "circle",                 # círculos
  type = "lower",                    # triángulo inferior
  order = "original",                # orden original
  tl.col = "black",                  # color etiquetas
  tl.cex = 1,                        # tamaño texto etiquetas
  tl.family = "Times New Roman",     # familia etiquetas
  addCoef.col = "black",             # coeficientes
  number.cex = 0.8,                  # tamaño coeficientes
  number.font = 1,                   # fuente normal para coef
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), # paleta
  cl.cex = 0.8,                      # tamaño escala leyenda
  cl.ratio = 0.2                     # ancho leyenda
)
