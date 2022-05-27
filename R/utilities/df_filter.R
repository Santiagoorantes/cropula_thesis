# ------------------------------------------------------------------------------
# --- Generic DataFrame Filter ---
# ------------------------------------------------------------------------------

#' Filter dataframe based on a given filter list
#'
#' @param df dataframe object
#' @param my_filter list of columns and values to filter
#'
#' @return Filtered dataframe
#' @export
#'
filter_df <- function(df, my_filter) {

  # check if filter cols are in df and only keep those
  filter_cols <- names(my_filter)
  cols_in_df <- which(filter_cols %in% names(df))
  my_filter <- my_filter[cols_in_df]
  cat("Filtering columns: ", names(my_filter))

  # filter df
  for (i in 1:length(my_filter)) {
    df <- df %>%
      dplyr::filter(eval(sym(names(my_filter)[i])) %in% my_filter[[i]])
  }

  return(df)
}
