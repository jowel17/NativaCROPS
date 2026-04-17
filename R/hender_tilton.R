#' Calcular eficacias con HENDERSON-TILTON
#'
#' Esta función realiza el calculo de las eficacias utilizando la formula HENDERSON-TILTON
#'
#' @param archivo nombre del archivo a analizar
#' @param promediar condicional que sirve si se necesita promediar mas alla de las repeticiones
#'
#'
#' @importFrom dplyr %>% filter mutate arrange bind_rows rename select contains
#' @importFrom dplyr across where any_of group_by summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom stringr str_extract word
#' @importFrom ggplot2 ggplot aes geom_bar geom_col geom_text geom_line geom_point
#' @importFrom ggplot2 labs theme theme_minimal element_text ggsave guides guide_legend
#' @importFrom jtools theme_apa
#' @importFrom rlang .data :=
#'
#' @returns Eficacias de los tratamientos
#' @export
#'
#' @examples
#' \dontrun{
#' hender_tilton("Datos/TETRUR.xlsx")
#' }
#'
#'


hender_tilton <- function(archivo, promediar = FALSE) {

  datos <- read_excel(archivo, sheet = "Base")

  if (promediar) {
    datos <- datos %>%
      pivot_longer(
        cols = -c(Tratamientos,Repeticion,Planta),
        names_to = "Variable",
        values_to = "Valor") %>%
      group_by(Tratamientos,Variable) %>%
      summarise(
        Media = mean(Valor),
        .groups = "drop"
      )
  } else {
    datos <- datos %>%
      pivot_longer(
        cols = -c(Tratamientos, Repeticion),
        names_to = "Variable",
        values_to = "Valor") %>%
      group_by(Variable, Tratamientos) %>%
      summarise(
        Media = mean(Valor))

  }

  # Limpiar datos
  ayuda <- datos
  ayuda$Variable <- as.factor(ayuda$Variable)
  datos$Variable <- as.factor(datos$Variable)


  # Formato ancho para tabla final
  datos_anchos <- datos %>%
    pivot_wider(names_from = "Variable", values_from = "Media")



  write_xlsx(datos_anchos,"Resultados/Tablas.xlsx")



  # ===============================
  # 🔥 PARTE IMPORTANTE (EFICACIA)
  # ===============================

  # Datos en formato ancho
  datos_ht <- datos %>%
    select(Variable, Tratamientos, Media) %>%
    pivot_wider(names_from = Variable, values_from = Media)

  # Separar control
  control <- datos_ht %>%
    filter(grepl("Testigo", Tratamientos))

  # Datos sin control
  resultados <- datos_ht %>%
    filter(!grepl("Testigo", Tratamientos))

  # Variables a evaluar
  var_1 <- setdiff(names(datos_ht), "Tratamientos")

  # ===============================
  # ✅ FUNCIÓN DE EFICACIA
  # ===============================

  calcular_eficacia <- function(data, control, vars) {

    control_DD1A0 <- control$DD1A0[1]

    for (var in vars) {

      control_var <- control[[var]][1]

      data <- data %>%
        mutate(
          !!paste0("Eficacia_", var) :=
            (1 - ((.data[[var]] / .data$DD1A0) *
                    (control_DD1A0 / control_var))) * 100
        )
    }

    return(data)
  }


  # Aplicar función
  resultados <- calcular_eficacia(resultados, control, var_1)




  resultados <- resultados %>%
    select(Tratamientos, contains("Eficacia")) %>%
    dplyr::select(-2)

  write_xlsx(resultados, "Resultados/Eficacias.xlsx")


  resultados_largos <- resultados %>%
    pivot_longer(
      cols = contains("Eficacia"),
      names_to = "Variable",
      values_to = "Eficacia(%)"
    )

  promedios_eficacias <- resultados_largos %>% group_by(Tratamientos) %>%
    summarise(
      Promedio = mean(`Eficacia(%)`)
    )

  grupos <- promedios_eficacias %>%
    mutate(Trat_base = word(Tratamientos, 2))


  grafico_1 <- ggplot(grupos,
                      aes(x = Promedio,
                          y = Tratamientos,
                          fill = Trat_base)) +
    geom_col() +
    geom_text(data = grupos,
              aes(x = 110, y = Tratamientos,
                  label = paste0(round(Promedio,2),"%"))) +
    theme(legend.position = "bottom") +
    theme_apa() + theme(legend.position = "none") +
    labs(title = "Eficacias(%) promedio de los diferentes Tratamientos",
         y = "Eficacia(%)")

  grafico_1

}
