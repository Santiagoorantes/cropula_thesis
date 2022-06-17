# ------------------------------------------------------------------------------
# --- Generic DataFrame Filter ---
# ------------------------------------------------------------------------------

#' Filter dataframe based on a given filter list

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


group_agr_df <- function(df, my_group) {
  
  df <- df %>%
    group_by(!!!syms(my_group)) %>%
    dplyr::summarise(
      av_yield = sum(volume) / sum(sowed),
      av_price = sum(production_value) / sum(volume)
    )
  
}

