#' Compute personal values from SVS-57 data
#'
#' This function computes personal values scores and different higher-order
#' dimensions related to Schwartz's theoretical framework based on the 57 items
#' of the Schwartz Value Survey (Schwartz, 1992).
#' By default, the function applies the recommended statistical adjustment
#' (individual MRAT centering) as suggested by the authors, to correct for
#' individual differences in response styles thus enhancing the interpretative
#' validity of the scores.
#'
#' @param df A data frame containing the raw responses for the SVS items.
#'           If "items" is not provided, it must have exactly 57 columns, and their order must
#'           correspond to the SVS items.
#' @param items An optional vector containing the names or indices of the columns that correspond
#'              to the SVS items. Must be exactly 57 items. If NULL, the function assumes the items
#'              are the only columns given in the "df" parameter.
#' @param correction Logical. When TRUE, the scores are corrected for individual differences
#'                   in the use of the response scale. Default is TRUE.
#' @param compute Character. Indicates which personal values scores to compute and return.
#'                Possible values are "all" (default), "ten.values", "four.higher", "two.foci",
#'                or "two.dynamics".
#' @param na.rm Logical. When TRUE, NAs are ignored in calculations; when FALSE,
#'              NAs are preserved and will affect calculations. Default is TRUE.
#'
#' @return A data frame with computed values. If both "df" and "items" parameters are provided,
#'         the returned data frame includes the original data with the calculations appended as new columns.
#' @note Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net,
#'       giuseppe.corbelli@uniroma1.it
#'
#' @examples
#' persval::svs(df = data.frame(
#'   svs1 = c(3, 1, 4), svs2 = c(2, 5, 3), svs3 = c(1, 5, 2), svs4 = c(4, 3, 5),
#'   svs5 = c(5, 2, 1), svs6 = c(3, 4, 2), svs7 = c(1, 2, 4), svs8 = c(3, 1, 5),
#'   svs9 = c(2, 4, 1), svs10 = c(5, 3, 2), svs11 = c(1, 4, 3), svs12 = c(2, 1, 5),
#'   svs13 = c(3, 5, 4), svs14 = c(1, 2, 3), svs15 = c(4, 5, 1), svs16 = c(2, 3, 4),
#'   svs17 = c(5, 1, 2), svs18 = c(3, 4, 1), svs19 = c(2, 3, NA), svs20 = c(1, 3, 4),
#'   svs21 = c(2, 5, 1), svs22 = c(4, 1, 5), svs23 = c(3, 4, 2), svs24 = c(5, 1, 3),
#'   svs25 = c(4, 2, 5), svs26 = c(1, 3, 2), svs27 = c(5, 4, 1), svs28 = c(2, 1, 4),
#'   svs29 = c(3, 5, 2), svs30 = c(1, 4, 3), svs31 = c(2, 3, 5), svs32 = c(4, 1, NA),
#'   svs33 = c(3, 5, 4), svs34 = c(1, 2, 3), svs35 = c(4, 1, 5), svs36 = c(2, 3, 4),
#'   svs37 = c(5, 2, 1), svs38 = c(4, 3, 2), svs39 = c(1, 5, 3), svs40 = c(2, 4, 1),
#'   svs41 = c(5, 1, 2), svs42 = c(3, 2, 4), svs43 = c(2, 5, 3), svs44 = c(4, 1, 5),
#'   svs45 = c(3, 2, 4), svs46 = c(1, 5, 3), svs47 = c(4, 3, 2), svs48 = c(5, 1, 2),
#'   svs49 = c(3, 4, 1), svs50 = c(2, 5, NA), svs51 = c(1, 4, 3), svs52 = c(2, 1, 5),
#'   svs53 = c(3, 5, 4), svs54 = c(1, 2, 3), svs55 = c(4, 5, 1), svs56 = c(2, 3, 4),
#'   svs57 = c(5, 1, 2)
#' ),
#' correction = TRUE,
#' compute = "all",
#' na.rm = TRUE)
#'
#' @export

svs <- function(df, items = NULL, compute = "all", correction = TRUE, na.rm = TRUE) {
  if (!is.null(items)) {
    if (length(items) != 57) {
      stop("Exactly 57 ordered items must be provided.")
    }
    if (is.numeric(items)) {
      required_columns <- names(df)[items]
    } else {
      required_columns <- items
    }
  } else {
    if (ncol(df) != 57) {
      stop("The data frame must contain exactly 57 columns.")
    }
    required_columns <- names(df)
  }

  if (!all(required_columns %in% names(df))) {
    stop("Data frame must contain all specified items.")
  }

  if (!(compute %in% c("all", "ten.values", "four.higher", "two.foci", "two.dynamics"))) {
    stop("Invalid value for the compute parameter.")
  }

  v <- df[, required_columns]
  mrat_pvq <- rowMeans(v, na.rm = na.rm)

  compute_val <- function(cols) {
    value <- rowMeans(df[, cols, drop = FALSE], na.rm = na.rm)
    if (correction) {
      value <- value - mrat_pvq
    }
    return(value)
  }

  val_names <- c("CONFORMITY", "TRADITION", "BENEVOLENCE", "UNIVERSALISM", "SELFDIRECTION",
                 "STIMULATION", "HEDONISM", "ACHIEVEMENT", "POWER", "SECURITY")
  val_cols <- list(
    c(required_columns[11], required_columns[20], required_columns[40], required_columns[47]),
    c(required_columns[18], required_columns[32], required_columns[36], required_columns[44], required_columns[51]),
    c(required_columns[33], required_columns[45], required_columns[49], required_columns[52], required_columns[54]),
    c(required_columns[1], required_columns[17], required_columns[24], required_columns[26], required_columns[29], required_columns[30], required_columns[35], required_columns[38]),
    c(required_columns[5], required_columns[16], required_columns[31], required_columns[41], required_columns[53]),
    c(required_columns[9], required_columns[25], required_columns[37]),
    c(required_columns[4], required_columns[50], required_columns[57]),
    c(required_columns[34], required_columns[39], required_columns[43], required_columns[55]),
    c(required_columns[3], required_columns[12], required_columns[27], required_columns[46]),
    c(required_columns[8], required_columns[13], required_columns[15], required_columns[22], required_columns[56])
  )

  values <- sapply(val_cols, compute_val, simplify = "array")
  values_df <- as.data.frame(values)
  names(values_df) <- paste0("val_", tolower(val_names))

  if (compute == "all" || compute == "four.higher") {
    values_df$VAL_OPENNESS <- rowMeans(values_df[, c("val_hedonism", "val_stimulation", "val_selfdirection")], na.rm = na.rm)
    values_df$VAL_SELFTR <- rowMeans(values_df[, c("val_benevolence", "val_universalism")], na.rm = na.rm)
    values_df$VAL_CONSERV <- rowMeans(values_df[, c("val_security", "val_conformity", "val_tradition")], na.rm = na.rm)
    values_df$VAL_SELFENH <- rowMeans(values_df[, c("val_achievement", "val_power")], na.rm = na.rm)
  }

  if (compute == "all" || compute == "two.foci") {
    values_df$FOC_individ <- rowMeans(values_df[, c("val_hedonism", "val_stimulation", "val_selfdirection", "val_achievement", "val_power")], na.rm = na.rm)
    values_df$FOC_collett <- rowMeans(values_df[, c("val_benevolence", "val_universalism", "val_security", "val_conformity", "val_tradition")], na.rm = na.rm)
  }

  if (compute == "all" || compute == "two.dynamics") {
    values_df$DYN_growth <- rowMeans(values_df[, c("val_hedonism", "val_stimulation", "val_selfdirection", "val_universalism", "val_benevolence")], na.rm = na.rm)
    values_df$DYN_protect <- rowMeans(values_df[, c("val_achievement", "val_power", "val_security", "val_conformity", "val_tradition")], na.rm = na.rm)
  }

  results <- if (compute == "all") {
    values_df
  } else if (compute == "four.higher") {
    values_df[, c("VAL_OPENNESS", "VAL_SELFTR", "VAL_CONSERV", "VAL_SELFENH")]
  } else if (compute == "two.foci") {
    values_df[, c("FOC_individ", "FOC_collett")]
  } else if (compute == "two.dynamics") {
    values_df[, c("DYN_growth", "DYN_protect")]
  } else {
    values_df
  }

  if (!is.null(items)) {
    results <- cbind(df, results)
  }

  return(results)
}

