library(dplyr)

expression_files <- list.files("./data/barcode-counts/",
                               pattern = "exp.*\\.xlsx",
                               full.names = TRUE)

# change format + barcode name
expression_matrices <- lapply(expression_files, function(old_filename) {
  expression_matrix <- readxl::read_excel(old_filename) |>
    dplyr::rename(barcode_id = 1)

  new_filename <- old_filename |> basename() |> tools::file_path_sans_ext()

  readr::write_csv(expression_matrix,
                   file = file.path("data", "barcode-counts", paste0(new_filename, ".csv")))

})
