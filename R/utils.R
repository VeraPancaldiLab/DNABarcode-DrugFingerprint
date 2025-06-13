# compare the content of two dataframes, return the most comprehensive
compare_dataframes <- function(old_data, new_data) {

  # compare barcode IDs ----
  unique_old_data_ids <- setdiff(old_data$barcode_id, new_data$barcode_id)
  if (length(unique_old_data_ids) == 0) {
    message("All barcode IDs in old_data are avalaible in new_data.")
  } else{
    message(paste(length(unique_old_data_ids),
                  " barcode IDs in old_data are missing in new_data."))
  }

  unique_new_data_ids <- setdiff(new_data$barcode_id, old_data$barcode_id)
  if (length(unique_new_data_ids) == 0) {
    message("All barcode IDs in new_data are avalaible in old_data.")
  } else{
    message(paste(length(unique_new_data_ids),
                  " barcode IDs in new_data are missing in old_data."))
  }

  # compare colnames of replicates ----
  specific_old_colnames <- setdiff(colnames(old_data), colnames(new_data))
  if (length(specific_old_colnames) == 0) {
    message("All replicates in old_data are avalaible in new_data.")
  } else{
    message(paste("Missing colnames in new data are",
                  paste(specific_old_colnames, collapse = ", \n")))
  }

  specific_new_colnames <- setdiff(colnames(new_data), colnames(old_data))
  if (length(specific_new_colnames) == 0) {
    message("All replicates in new_data are avalaible in old_data.")
  } else{
    message(paste("Missing colnames in old data are:\n",
                  paste(specific_new_colnames, collapse = ", \n")))
  }

  # compare similar content = same barcode IDs and same colnames ----
  shared_colnames <- intersect(colnames(old_data), colnames(new_data))
  common_barcode_ids <- intersect(old_data$barcode_id, new_data$barcode_id)
  old_data_reduced <- old_data |>
    dplyr::select(dplyr::all_of(shared_colnames)) |>
    dplyr::filter(barcode_id %in% c(common_barcode_ids)) |>
    dplyr::arrange(barcode_id)

  new_data_reduced <- new_data |>
    dplyr::select(dplyr::all_of(shared_colnames)) |>
    dplyr::filter(barcode_id %in% c(common_barcode_ids)) |>
    dplyr::arrange(barcode_id)

  all.equal(old_data_reduced, new_data_reduced)


  # return the most complete dataset ----
  return(list("old_reduced" = old_data_reduced,
              "new_reduced" = new_data_reduced))

}


compare_old_vs_new_distributions <- function(old_data, new_data) {

  # keep only shared colnames ----
  shared_colnames <- intersect(colnames(old_data), colnames(new_data))
  old_data <- old_data |>
    dplyr::select(dplyr::all_of(shared_colnames))
  new_data <- new_data |>
    dplyr::select(dplyr::all_of(shared_colnames))


  # compute all subsets of old versus new ----
  shared_distribution <- old_data |>
    dplyr::semi_join(new_data, by = "barcode_id") |>
    tidyr::pivot_longer(
      cols = !barcode_id,
      names_to = "replicates_id",
      values_to = "barcode_value") |>
    dplyr::mutate(subset_section="intersect")

  old_specific <- old_data |>
    dplyr::anti_join(new_data, by = "barcode_id") |>
    tidyr::pivot_longer(
      cols = !barcode_id,
      names_to = "replicates_id",
      values_to = "barcode_value")|>
    dplyr::mutate(subset_section="old/new")

  message(paste("Quantiles 0.9, 0.95, 0.99, 0.995, 0.999 and 1 of old distribution over new are",
                paste(quantile(old_specific$barcode_value, probs = c(0.9, 0.95, 0.99, 0.995, 0.999, 1)),
                      collapse = ", ")))

  new_specific <- new_data |>
    dplyr::anti_join(old_data, by = "barcode_id") |>
    tidyr::pivot_longer(
      cols = !barcode_id,
      names_to = "replicates_id",
      values_to = "barcode_value")|>
    dplyr::mutate(subset_section="new/old")

  message(paste("Quantiles 0.9, 0.95, 0.99, 0.995, 0.999 and 1 of new distribution over old are",
                paste(quantile(new_specific$barcode_value, probs = c(0.9, 0.95, 0.99, 0.995, 0.999, 1)),
                      collapse = ", ")))

  df_subsets <- shared_distribution |>
    dplyr::bind_rows(old_specific) |>
    dplyr::bind_rows(new_specific)

  subset_plot <- ggplot(df_subsets, aes(x=log1p(barcode_value), color=subset_section, fill=subset_section)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
    geom_density(alpha=0.6)+
    labs(title="Compare old versus new distributions",
         x="Barcode counts",
         y = "Density")+
    theme_minimal()

  return(subset_plot)
}

# Helper functions to homogenise `Concentration_ID`
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

# Convert unconventional characters to ASCII symbols, such as greek letters
enc_to_ascii <- function(text, replace_with = ".") {
  iconv(text, from = "UTF-8", to = "ASCII", sub = replace_with)
}

# Return the size of a ComplexHeatmap matrix
calc_ht_size <- function(ht, unit = "inch") {
  pdf(NULL)
  ht  <- ComplexHeatmap::draw(ht)
  w  <- ComplexHeatmap:::width(ht) |>
    grid::convertX(unit, valueOnly = TRUE)
  h  <- ComplexHeatmap:::height(ht) |>
    grid::convertY(unit, valueOnly = TRUE)
  dev.off()
  return(list(width=w, height=h))
}


