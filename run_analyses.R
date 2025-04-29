library(dplyr)
library(flextable)
library(openxlsx)

# 1) summarise removed batches ----

barcode_files_kept <- list.files("./data/barcode-counts/",
                            pattern = "exp.*\\.csv",
                            full.names = TRUE)
kept_barcode_summaries <- purrr::map(barcode_files_kept, function(filename) {
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


# openxlsx::write.xlsx(kept_barcode_summaries,
#                      file = "./data/logs/kept_experiences_overview.xlsx")

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

# openxlsx::write.xlsx(barcode_discarded_summaries,
#                      file = "./data/logs/discarded_experiences_overview.xlsx")


# 3) summarise old batches ----

old_barcode_files <- list.files("./old-data-backup/data-raw/barcode-counts/",
                            pattern = "exp.*\\.csv",
                            full.names = TRUE)
old_barcode_summaries <- purrr::map(old_barcode_files, function(filename) {
  barcode_counts <- readr::read_csv2(filename, show_col_types = FALSE)
  experience_name <- filename |> basename() |> tools::file_path_sans_ext()
  experience_summary <- tibble::tibble(`Experience Name`= experience_name,
                                       Features= paste0("Barcode matrix contains: ",
                                                        nrow(barcode_counts),
                                                        " unique barcode IDs, and ",
                                                        ncol(barcode_counts)-1, " replicates."))
  return(experience_summary)
}) |>
  purrr::list_rbind()

global_summaries <- list("kept" = kept_barcode_summaries,
                         "discarded" = barcode_discarded_summaries,
                         "old samples" = old_barcode_summaries)
openxlsx::write.xlsx(global_summaries,
                     file = "./data/logs/global_summaries_rows_by_cols.xlsx",
                     asTable = TRUE, overwrite = TRUE)

