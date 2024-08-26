#' Compute personal values from TIVI data
#'
#' This function computes personal values scores and different higher-order
#' dimensions related to Schwartz's theoretical framework based on the 10 items
#' of the ultra-brief TIVI Questionnaire (Sandy et al., 2017).
#' By default, the function applies the recommended statistical adjustment
#' (individual MRAT centering) as suggested by the authors, to correct for
#' individual differences in response styles thus enhancing the interpretative
#' validity of the scores.
#'
#' @param df A data frame containing the raw responses for the 10 TIVI items.
#'           If "items" is not provided, it must have exactly 10 columns, and their order
#'           must correspond to the TIVI items.
#' @param items An optional vector containing the names or indices of the columns
#'              that correspond to the TIVI items. Must be exactly 10 items. If NULL,
#'              the function assumes the items are the only columns given in the "df" parameter.
#' @param correction Logical. When TRUE, the scores are corrected for individual differences
#'                   in the use of the response scale. Default is TRUE.
#' @param compute Character. Indicates which personal values scores to compute and return.
#'                Possible values are "all" (default), "ten.values", "four.higher", "two.foci",
#'                or "two.dynamics".
#' @param na.rm Logical. When TRUE, NAs are ignored in calculations; when FALSE,
#'              NAs are preserved and will affect calculations. Default is TRUE.
#'
#' @return A data frame with computed values. If both "df" and "items" parameters are
#'         provided, the returned data frame includes the original data with the calculations
#'         appended as new columns.
#' @note Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net,
#'       giuseppe.corbelli@uniroma1.it
#'
#' @examples
#' persval::tivi(df = data.frame(
#' tivi1 = c(3, 1, 4), tivi2 = c(2, 5, 3), tivi3 = c(1, 5, 2),
#' tivi4 = c(4, 3, 5), tivi5 = c(5, 2, 1), tivi6 = c(3, 4, 2), tivi7 = c(1, 2, 4),
#' tivi8 = c(3, 1, 5), tivi9 = c(2, 4, 1), tivi10 = c(5, 3, 2)
#' ),
#' correction = TRUE,
#' compute = "all",
#' na.rm = TRUE)
#'
#' @export

tivi <- function(df, items = NULL, compute = "all", correction = TRUE, na.rm = TRUE) {
  if (!is.null(items)) {
    if (length(items) != 10) {
      stop("Exactly 10 ordered items must be provided.")
    }
    if (is.numeric(items)) {
      required_columns <- names(df)[items]
    } else {
      required_columns <- items
    }
  } else {
    if (ncol(df) != 10) {
      stop("The data frame must contain exactly 10 columns.")
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
    required_columns[1],
    required_columns[2],
    required_columns[3],
    required_columns[4],
    required_columns[5],
    required_columns[6],
    required_columns[7],
    required_columns[8],
    required_columns[9],
    required_columns[10]
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
