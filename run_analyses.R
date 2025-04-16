library(dplyr)

# expression_files <- list.files("./data/barcode-counts/",
#                                pattern = "exp.*\\.xlsx",
#                                full.names = TRUE)
#
# # change format + barcode name
# expression_matrices <- lapply(expression_files, function(old_filename) {
#   expression_matrix <- readxl::read_excel(old_filename) |>
#     dplyr::rename(barcode_id = 1)
#
#   new_filename <- old_filename |> basename() |> tools::file_path_sans_ext()
#
#   readr::write_csv(expression_matrix,
#                    file = file.path("data", "barcode-counts", paste0(new_filename, ".csv")))
#
# })

barcode_files <- list.files("./data/barcode-counts/",
                            pattern = "exp.*\\.csv",
                            full.names = TRUE)

# change format + barcode name
barcode_summaries <- purrr::map(barcode_files, function(filename) {
  barcode_counts <- readr::read_csv(filename, show_col_types = FALSE)
  experience_name <- filename |> basename() |> tools::file_path_sans_ext()
  experience_summary <- tibble::tibble(`Experience Name`= experience_name,
                                       Features= paste0("Barcode matrix contains: ", nrow(barcode_counts),
                                                        " unique barcode IDs, and ", ncol(barcode_counts)-1, " replicates."))
  return(experience_summary)
}) |>
  purrr::list_rbind()
