compare_dataframes <- function(df1, df2) {

  # compare dimensions and ids
  #testthat::expect_equal(dim(df1), dim(df2), info = "Column counts differ")
  message(all.equal(df1, df2), "\n\n")

  unique_df1_ids <- setdiff(df1$key_barcode, df2$key_barcode)
  if (length(unique_df1_ids)==0) {
    message("All barcode IDs in df1 are avalaible in df2.")
  } else{
    message(paste(length(unique_df1_ids),
                  " barcode IDs in df1 are missing in df2."))
  }

  unique_df2_ids <- setdiff(df2$key_barcode, df1$key_barcode)
  if (length(unique_df2_ids)==0) {
    message("All barcode IDs in df2 are avalaible in df1.")
  } else{
    message(paste(length(unique_df2_ids),
                  " barcode IDs in df2 are missing in df1."))
  }

  # compare shared content (same colnames and barcode IDs)
  shared_barcodes <- tibble::tibble(key_barcode =
                                      intersect(df2$key_barcode, df1$key_barcode))
  shared_colnames <- intersect(colnames(df1), colnames(df2))
  df1_consensual <- df1 |>
    semi_join(df2, by = "key_barcode") |>
    select(all_of(shared_colnames)) |>
    arrange(key_barcode)
  df2_consensual <- df2 |>
    semi_join(df1, by = "key_barcode") |>
    select(all_of(shared_colnames)) |>
    arrange(key_barcode)

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


format_number <- function(x) {
  x_formatted <- sapply(x, function(number_to_format) {
    if (number_to_format %% 1 == 0) {  # Check if x is an integer
      sprintf("%03d", number_to_format)  # Left-pad integers to 3 digits
    } else {
      sprintf("%.2f", number_to_format)  # Format decimals with 2 decimal places
    }
  })
  return(x_formatted)

}

