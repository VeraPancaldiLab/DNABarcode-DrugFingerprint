compare_dataframes <- function(df1, df2) {

  # compare dimensions and ids
  # message(all.equal(df1, df2), "\n\n")

  unique_df1_ids <- setdiff(df1$barcode_id, df2$barcode_id)
  if (length(unique_df1_ids)==0) {
    message("All barcode IDs in df1 are avalaible in df2.")
  } else{
    message(paste(length(unique_df1_ids),
                  " barcode IDs in df1 are missing in df2."))
  }

  unique_df2_ids <- setdiff(df2$barcode_id, df1$barcode_id)
  if (length(unique_df2_ids)==0) {
    message("All barcode IDs in df2 are avalaible in df1.")
  } else{
    message(paste(length(unique_df2_ids),
                  " barcode IDs in df2 are missing in df1."))
  }

  # compare shared content (same colnames and barcode IDs)
  shared_barcodes <- tibble::tibble(barcode_id =
                                      intersect(df2$barcode_id, df1$barcode_id))
  shared_colnames <- intersect(colnames(df1), colnames(df2))
  df1_consensual <- df1 |>
    semi_join(df2, by = "barcode_id") |>
    select(all_of(shared_colnames)) |>
    arrange()

  df2_consensual <- df2 |>
    semi_join(df1, by = "barcode_id") |>
    dplyr::select(all_of(shared_colnames)) |>
    dplyr::arrange(barcode_id)

  testthat::expect_equal(df1_consensual, df2_consensual,
                         info = "On the shared dimensions, dataframes are not equivalent")

  # return the most complete dataset
  if (all(dim(df1) > dim(df2))) {
    return(df1)
  }
  else {
    return(df2)
  }

}


format_concentrations <- function(x) {
  x_formatted <- sapply(x, function(number_to_format) {
    if (number_to_format %% 1 == 0) {  # Check if x is an integer
      sprintf("%03d", number_to_format)  # Left-pad integers to 3 digits
    } else {
      sprintf("%.2f", number_to_format)  # Format decimals with 2 decimal places
    }
  })
  return(x_formatted)

}

