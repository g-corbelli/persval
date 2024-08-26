#' Compute personal values from PVQ-40 data
#'
#' This function computes personal values scores and different higher-order
#' dimensions related to Schwartz's theoretical framework based on the 40 items
#' of the PVQ-40 Value Scale (Schwartz, 2006; Schwartz et al., 2001).
#' By default, the function applies the recommended statistical adjustment
#' (individual MRAT centering) as suggested by the authors, to correct for
#' individual differences in response styles thus enhancing the interpretative
#' validity of the scores.
#'
#' @param df A Data frame containing the raw responses for the 40 PVQ items. If "items" is not provided, it must have exactly 40 columns, and their order must correspond to the PVQ-40 items.
#' @param items An optional vector containing the names or indices of the columns that correspond to the PVQ-40 items. Must be exactly 40 items. If NULL, the function assumes the items are the only columns given in the "df" parameter.
#' @param correction Logical. When TRUE, the scores are corrected for individual differences in the use of the response scale. Default is TRUE.
#' @param compute Character. Indicates which personal values scores to compute and return. Possible values are "all" (default), "ten.values", "four.higher", "two.foci", or "two.dynamics".
#' @param na.rm Logical. When TRUE, NAs are ignored in calculations; when FALSE, NAs are preserved and will affect calculations. Default is TRUE.
#'
#' @return A data frame with computed values. If both "df" and "items" parameters are provided, the returned data frame includes the original data with the calculations appended as new columns.
#' @note Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net, giuseppe.corbelli@uniroma1.it
#'
#' @examples
#' persval::pvq40(df = data.frame(
#' pvq1 = c(3, 1, 4), pvq2 = c(2, 5, 3), pvq3 = c(1, 5, 2), pvq4 = c(4, 3, 5),
#' pvq5 = c(5, 2, 1), pvq6 = c(3, 4, 2), pvq7 = c(1, 2, 4), pvq8 = c(3, 1, 5),
#' pvq9 = c(2, 4, 1), pvq10 = c(5, 3, 2), pvq11 = c(1, 4, 3), pvq12 = c(2, 1, 5),
#' pvq13 = c(3, 5, 4), pvq14 = c(1, 2, 3), pvq15 = c(4, 5, 1), pvq16 = c(2, 3, 4),
#' pvq17 = c(5, 1, 2), pvq18 = c(3, 4, 1), pvq19 = c(2, 3, NA), pvq20 = c(1, 3, 4),
#' pvq21 = c(2, 5, 1), pvq22 = c(4, 1, 5), pvq23 = c(3, 4, 2), pvq24 = c(5, 1, 3),
#' pvq25 = c(4, 2, 5), pvq26 = c(1, 3, 2), pvq27 = c(5, 4, 1), pvq28 = c(2, 1, 4),
#' pvq29 = c(3, 5, 2), pvq30 = c(1, 4, 3), pvq31 = c(2, 3, 5), pvq32 = c(4, 1, NA),
#' pvq33 = c(3, 5, 4), pvq34 = c(1, 2, 3), pvq35 = c(4, 1, 5), pvq36 = c(2, 3, 4),
#' pvq37 = c(5, 2, 1), pvq38 = c(4, 3, 2), pvq39 = c(1, 5, 3), pvq40 = c(2, 4, 1)
#' ),
#' correction = TRUE,
#' compute = "all",
#' na.rm = TRUE)
#'
#' @export

pvq40 <- function(df, items = NULL, compute = "all", correction = TRUE, na.rm = TRUE) {
  if (!is.null(items)) {
    if (length(items) != 40) {
      stop("Exactly 40 ordered items must be provided.")
    }
    if (is.numeric(items)) {
      required_columns <- names(df)[items]
    } else {
      required_columns <- items
    }
  } else {
    if (ncol(df) != 40) {
      stop("The data frame must contain exactly 40 columns.")
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
    if (all(is.na(value))) {
      stop("Error: all the items for computing the value are missing.")
    }
    value
  }

  val_names <- c("CONFORMITY", "TRADITION", "BENEVOLENCE", "UNIVERSALISM", "SELFDIRECTION", "STIMULATION", "HEDONISM", "ACHIEVEMENT", "POWER", "SECURITY")
  val_cols <- list(
    c(required_columns[7], required_columns[16], required_columns[28], required_columns[36]), # CONFORMITY
    c(required_columns[9], required_columns[20], required_columns[25], required_columns[38]), # TRADITION
    c(required_columns[12], required_columns[18], required_columns[27], required_columns[33]), # BENEVOLENCE
    c(required_columns[3], required_columns[8], required_columns[19], required_columns[23], required_columns[29], required_columns[40]), # UNIVERSALISM
    c(required_columns[1], required_columns[11], required_columns[22], required_columns[34]), # SELFDIRECTION
    c(required_columns[6], required_columns[15], required_columns[30]), # STIMULATION
    c(required_columns[10], required_columns[26], required_columns[37]), # HEDONISM
    c(required_columns[4], required_columns[13], required_columns[24], required_columns[32]), # ACHIEVEMENT
    c(required_columns[2], required_columns[17], required_columns[39]), # POWER
    c(required_columns[5], required_columns[14], required_columns[21], required_columns[31], required_columns[35]) # SECURITY
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
