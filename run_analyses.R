library(dplyr)
library(flextable)

barcode_files <- list.files("./data/barcode-counts/",
                            pattern = "exp.*\\.csv",
                            full.names = TRUE)
barcode_summaries <- purrr::map(barcode_files, function(filename) {
  barcode_counts <- readr::read_csv(filename, show_col_types = FALSE)
  experience_name <- filename |> basename() |> tools::file_path_sans_ext()
  experience_summary <- tibble::tibble(`Experience Name`= experience_name,
                                       Features= paste0("Barcode matrix contains: ",
                                                        nrow(barcode_counts),
                                                        " unique barcode IDs, and ",
                                                        ncol(barcode_counts)-1, " replicates."))
  return(experience_summary)
}) |>
  purrr::list_rbind()


openxlsx::write.xlsx(barcode_summaries,
                     file = "./data/kept_experiences_overview.xlsx")

# 2) summarise discarded batches ----

discarded_files <- list.files("./data/barcode-counts/temp discarded/",
                              pattern = "exp.*\\.csv",
                              full.names = TRUE)

# change format + barcode name
barcode_discarded_summaries <- purrr::map(discarded_files, function(filename) {
  barcode_counts <- readr::read_csv(filename, show_col_types = FALSE)
  experience_name <- filename |> basename() |> tools::file_path_sans_ext()
  experience_summary <- tibble::tibble(`Experience Name`= experience_name,
                                       Features= paste0("Barcode matrix contains: ",
                                                        nrow(barcode_counts),
                                                        " unique barcode IDs, and ",
                                                        ncol(barcode_counts)-1, " replicates."))
  return(experience_summary)
}) |>
  purrr::list_rbind()

openxlsx::write.xlsx(barcode_discarded_summaries,
                     file = "./data/discarded_experiences_overview.xlsx")

