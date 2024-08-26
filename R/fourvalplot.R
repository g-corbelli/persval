#' Plot spider chart for four higher order personal values
#'
#' This function generates a spider chart based on four higher order Basic Human Values:
#' Openness to Change, Self-Enhancement, Conservation, and Self-Transcendence.
#' This visualization helps in understanding the profile of higher order values for individuals or groups.
#'
#' @importFrom fmsb radarchart
#' @importFrom stats setNames sd
#' @importFrom grDevices rgb
#' @importFrom graphics title
#' @param df A dataframe containing computed values for the four higher order personal values.
#' @param valueMap A named vector where names are the four higher order personal values:
#'        Openness to Change ("OPEN"), Self-enhancement ("SELFENH"), Conservation ("CONS"),
#'        Self-transcendence ("SELFTRANS"). Vector values correspond to the column names in the dataframe.
#' @param corrected Logical. Indicates if data are ipsatized (range -4 to +4); if FALSE, data are raw scores (range 1 to 5).
#' @param standerr Logical. When TRUE, plots mean ± SE lines; when FALSE, plots only the mean.
#' @param na.rm Logical. When TRUE, NAs are ignored in calculations; when FALSE, NAs affect calculations.
#' @return A spider chart visualizing the four higher order values.
#' @examples
#' df <- data.frame(
#'   open_comp = runif(10, 1, 5), selfenh_comp = runif(10, 1, 5),
#'   cons_comp = runif(10, 1, 5), selftrans_comp = runif(10, 1, 5)
#' )
#' valueMap <- c(OPEN = "open_comp", SELFENH = "selfenh_comp",
#'               CONS = "cons_comp", SELFTRANS = "selftrans_comp")
#' fourvalplot(df, valueMap, corrected = TRUE, standerr = TRUE)
#' @export

fourvalplot <- function(df, valueMap, corrected = TRUE, standerr = TRUE, na.rm = TRUE) {
  specificValues <- c("OPEN", "SELFENH", "CONS", "SELFTRANS")  # Ensuring the order is correct

  if (!identical(sort(names(valueMap)), sort(specificValues))) {
    stop("Please provide exactly four value mappings, one for each higher order personal value.")
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

  title(main = "Spider Chart: Four Higher Order Values")
}
