#' Análisis DBCA con gráficos y exportación
#'
#' Esta función realiza un análisis de varianza (DBCA), calcula medias,
#' agrupaciones de tratamientos (CLD), coeficiente de variación y genera
#' gráficos de barras y líneas. Además, exporta los resultados a Excel.
#' @param archivo nombre del archivo a analizar
#' @param nombre_salida nombre del archivo que guardara los CV, los valores p y letras de significancia
#' @param etiqueta_y nombre que tomara el eje y en todos los graficos generados
#'
#' @importFrom dplyr %>% filter mutate arrange bind_rows rename
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom stringr str_extract word
#' @importFrom ggplot2 ggplot aes geom_bar geom_text geom_line geom_point labs theme theme_minimal element_text ggsave
#' @importFrom stats aov anova reorder
#' @importFrom agricolae cv.model
#' @importFrom emmeans emmeans
#' @importFrom multcomp cld
#' @importFrom jtools theme_apa
#'
#' @returns Resumen de los tratamientos
#' @export
#'
#' @examples
#' \dontrun{
#' dbca_simple("Datos/TETRUR.xlsx", nombre_salida = "Resultados/Resultados.xlsx",
#' etiqueta_y = "Incidencia %")
#' }

dbca_simple <- function(archivo, nombre_salida = "Resultados/Resultados.xlsx",
                        etiqueta_y = "Valor") {

  #Cargar datos
  datos <- read_excel(archivo, sheet = "Base")

  datos$Tratamientos <- as.factor(datos$Tratamientos)
  datos$Repeticion <- as.factor(datos$Repeticion)

  #Pivotar
  datos_largos <- datos %>%   pivot_longer(cols = -c(Tratamientos, Repeticion),
                                           names_to = "Variable",
                                           values_to = "Valor")

  variables <- unique(datos_largos$Variable)

  valoresp <- data.frame(
    Variable = variables,
    Valorp = 0,
    CV = 0
  )

  grupos <- data.frame(
    Variable = character(),
    Tratamiento = character(),
    Grupos = character(),
    Media = numeric(),
    stringsAsFactors = FALSE
  )

  lista_graficos <- list()

  # BUCLE UNICO: ANOVA + Tukey
  for(i in seq_along(variables)) {
    df <- datos_largos |> filter(Variable == variables[i])

    # ANOVA
    mod1 <- aov(Valor ~ Repeticion + Tratamientos, data = df)
    tabla <- anova(mod1)
    valoresp[i, "Valorp"] <- tabla[1,5]  # p-valor
    cv <- cv.model(mod1)
    valoresp[i, "CV"] <- cv

    cat("Analizando:", variables[i], "\n")
    cat("--- ANOVA ---\n")
    print(summary(mod1))
    cat("CV:", cv, "%\n")

    # Tukey
    mod2 <- aov(Valor ~ Tratamientos, data = df)
    emm_model <- emmeans(mod2, ~ Tratamientos)

    tukey_groups <- suppressMessages(cld(emm_model,
                                         alpha = 0.05,
                                         Letters = LETTERS,
                                         adjust = "tukey"))

    tukey_groups <- tukey_groups %>%
      mutate(Variable = variables[i]) %>%
      rename(Grupos = .group,
             Media = emmean)  # Solo columnas necesarias

    # Agregar a dataframe grupos acumulado
    grupos <- bind_rows(grupos, tukey_groups)

    grupos$Grupos <- as.character(grupos$Grupos)

    grupos$Simplificados <- sapply(
      strsplit(trimws(grupos$Grupos), ""),  # quitas espacios iniciales y divides
      "[[", 1
    )

    # 1) Crear Simplificados en tukey_groups
    tukey_groups$Grupos <- as.character(tukey_groups$Grupos)
    tukey_groups$Grupos <- trimws(tukey_groups$Grupos)
    tukey_groups$Simplificados <- substr(tukey_groups$Grupos, 1, 1)

    # Crear dataframe individual (opcional, para compatibilidad)
    nombre_df <- paste0("tukey_groups_", variables[i])
    assign(nombre_df, tukey_groups)

    cat("--- GRUPOS TUKEY (FINALES) ---\n")
    print(tukey_groups[, c("Tratamientos", "Media", "Grupos", "Simplificados")])
    cat("\n")

  }

  # Exportar resultados
  lista_hojas <- list(
    "Valores_p" = valoresp,
    "Todos_Tukey" = grupos  # Dataframe unificado con todos los Tukey
  )

  # Agregar dataframes individuales si los necesitas
  for(var in variables) {
    nombre_df <- paste0("tukey_groups_", var)
    lista_hojas[[paste0("Tukey_", var)]] <- get(nombre_df)
  }

  write_xlsx(lista_hojas, path = "Resultados/Resultados.xlsx")

  dda <- unique(grupos$Variable)

  testigo <- grupos %>% filter(grepl("Testigo", Tratamientos))

  grupos <- grupos %>%
    mutate(Trat_base = word(Tratamientos, 2))

  grupos <- grupos %>%
    mutate(
      Aplicacion = as.numeric(str_extract(Variable, "(?<=DD)\\d+")),
      Dias = as.numeric(str_extract(Variable, "(?<=A)\\d+"))
    )

  grupos <- grupos %>%
    arrange(Aplicacion, Dias) %>%
    mutate(Variable = factor(Variable, levels = unique(Variable)))

  for (i in seq_along(dda)) {
    data <- grupos %>% filter(Variable == dda[i])

    lista_graficos[[dda[i]]] <- ggplot(data, aes(x = reorder(Tratamientos, Media), y = Media, fill = Trat_base)) +
      geom_bar(stat = "identity", position = "dodge", lwd = 0.4) +
      geom_text(data = data,
                aes(y = Media * 0.85, label = Simplificados), fontface = "bold", size = 5.5) +
      labs(title = paste("Analisis", variables[i]), y = etiqueta_y, x = "Tratamientos") +
      theme_minimal() +
      theme(legend.position = "bottom",
            text = element_text(color = "black"),
            plot.title = element_text(face = "bold", color= "black", size = 14),
            axis.title.x = element_text(face = "bold", size = 12),
            axis.title.y = element_text(face = "bold",size = 12),
            legend.text = element_text(color = "black", size = 12),
            legend.title = element_text(color = "black"),
            axis.text.x = element_text(angle = 35, hjust = 1, color = "black")) +
      theme(legend.position = "none") +
      geom_text(data = data,
                aes(y = Media +  3, label = round(Media,2)), size = 5)


    # Grafico de Lineas -------------------------------------------------------

    g_lineas <- ggplot(grupos, aes(x = as.factor(Variable), y = Media,
                                   color = Tratamientos, group = Tratamientos)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      labs(title = "Evolucion temporal de los tratamientos", x = "", y = etiqueta_y) +
      theme_apa() + theme(legend.position = "bottom",
                          text = element_text(color = "black"),
                          plot.title = element_text(color = "black", size = 14),
                          axis.title.x = element_text(size = 12, face = "bold"),
                          axis.title.y = element_text(size = 12, face = "bold"),
                          legend.text = element_text(color = "black", size = 10),
                          legend.title = element_text(color = "black"),
                          axis.text.x = element_text(angle = 35, hjust = 1, color = "black")) +
      geom_text(data = testigo, aes(x = as.factor(Variable), y = Media + 2, label = round(Media,2)),
                color = "black",
                fontface = "bold")

  }

  # Guardar cada gráfico de barras como PNG
  for (nombre in names(lista_graficos)) {
    ggsave(
      filename = paste0("Graficos/grafico_barras_", nombre, ".png"),
      plot = lista_graficos[[nombre]],
      width = 14, height = 8, units = "in", dpi = 300
    )
  }

  ggsave("Graficos/Grafica de Lineas.png", g_lineas,width = 14, height = 8 )

}
