#' Plot spider chart for ten specific personal values
#'
#' This function generates a spider chart based on the Basic Human Values.
#' It requires exactly ten value categories corresponding to the 10 predefined
#' personal values. This visualization helps in understanding the individual
#' or average profile of personal values.
#'
#' @importFrom fmsb radarchart
#' @importFrom stats setNames sd
#' @importFrom grDevices rgb
#' @importFrom graphics title
#' @param df A dataframe containing the computed values for the ten specific personal values.
#' @param valueMap A named vector where names correspond to the ten personal values:
#'        Conformity ("CO"), Tradition ("TR"), Benevolence ("BE"), Achievement ("AC"),
#'        Power ("PO"), Security ("SE"), Stimulation ("ST"), Self-direction ("SD"),
#'        Universalism ("UN"), Hedonism ("HE"). Vector values correspond to the column names
#'        in the dataframe.
#' @param corrected Logical. When TRUE, data are already ipsatized (range -4 to +4);
#'                  when FALSE, data are raw scores (range 1 to 5). Defaults to TRUE.
#' @param standerr Logical. When TRUE, plots mean-SE and mean+SE lines; when FALSE,
#'                plots only the mean. Default is TRUE.
#' @param na.rm Logical. When TRUE, NAs are ignored in calculations; when FALSE, NAs
#'              are preserved and will affect calculations. Default is TRUE.
#' @examples
#' df <- data.frame(
#'   conf_comp = runif(10, 1, 5), trad_comp = runif(10, 1, 5),
#'   bene_comp = runif(10, 1, 5), achie_comp = runif(10, 1, 5),
#'   power_comp = runif(10, 1, 5), sec_comp = runif(10, 1, 5),
#'   stim_comp = runif(10, 1, 5), selfdir_comp = runif(10, 1, 5),
#'   univ_comp = runif(10, 1, 5), hedo_comp = runif(10, 1, 5)
#' )
#' valueMap <- c(CO = "conf_comp", TR = "trad_comp",
#'               BE = "bene_comp", AC = "achie_comp",
#'               PO = "power_comp", SE = "sec_comp",
#'               ST = "stim_comp", SD = "selfdir_comp",
#'               UN = "univ_comp", HE = "hedo_comp")
#' tenvalplot(df, valueMap)
#' @export

tenvalplot <- function(df, valueMap, corrected = TRUE, standerr = TRUE, na.rm = TRUE) {
  specificValues <- c("SD", "ST", "HE", "AC",
                      "PO", "SE", "TR", "CO",
                      "BE", "UN")

  if (!identical(sort(names(valueMap)), sort(specificValues))) {
    stop("Please provide exactly ten value mappings, one for each personal value.")
  }

  if (!all(valueMap %in% names(df))) {
    stop("One or more specified columns not found in the dataframe.")
  }

  orderedValueMap <- setNames(valueMap[match(specificValues, names(valueMap))], specificValues)

  valueMeans <- colMeans(df[, orderedValueMap, drop = FALSE], na.rm = na.rm)
  n <- sum(!is.na(df[, orderedValueMap])) / length(orderedValueMap)  # Average number of non-NA observations per column
  valueSDs <- apply(df[, orderedValueMap, drop = FALSE], 2, sd, na.rm = na.rm)
  valueSEs <- valueSDs / sqrt(n)  # Calculate Standard Error

  # check di consistenza di dominio col parametro corrected:
  if (corrected && any(valueMeans > 4 | valueMeans < -4)) {
    stop("Values exceed the expected range (-4 to 4). If you provided raw scores, please set corrected = FALSE.")
  }
  if (!corrected && any(valueMeans < 1 | valueMeans > 5)) {
    stop("Values fall outside the expected range (1 to 5). If you provided ipsatized scores, please set corrected = TRUE.")
  }

  radarData <- data.frame(t(valueMeans))
  colnames(radarData) <- specificValues
  radar_min <- if (corrected) -4 else 1
  radar_max <- if (corrected) 4 else 5
  radarData <- rbind(
    rep(radar_max, length(specificValues)),
    rep(radar_min, length(specificValues)),
    radarData
  )

  # aggiungi errore standard solo se standerr e' true e non soggetto singolo:
  if (standerr && nrow(df) > 1) {
    radarData <- rbind(radarData, radarData[3, ] - valueSEs, radarData[3, ] + valueSEs)
    colori_linea <- c("black", "darkgray", "gray")
    colori_riempimento <- c(rgb(0, 0, 0, 0.15), rgb(0, 0, 0, 0.15), rgb(0, 0, 0, 0.15))
    tipi_linea <- c(1, 0, 0)
    larghezza_linea <- c(1.8, 1, 1)
    tipo_punti <- c(16, 32, 32)
  } else {
    colori_linea <- "black"
    colori_riempimento <- rgb(0, 0, 0, 0.15)
    tipi_linea <- 1
    larghezza_linea <- 1.8
    tipo_punti <- 16
  }

  vettore_etichette <- if (corrected) c(-4, -2, 0, +2, +4) else c(1, 2, 3, 4, 5)

  fmsb::radarchart(df = radarData,
                   axistype = 1,
                   caxislabels = vettore_etichette,
                   palcex = 0.8,
                   pcol = colori_linea,
                   pfcol = colori_riempimento,
                   plty = tipi_linea,
                   plwd = larghezza_linea,
                   pty = tipo_punti,
                   vlcex = 0.8)

  # Enhance the plot
  title(main = "Spider Chart: Ten Basic Human Values")
}


