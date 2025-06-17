library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(flextable)
library(openxlsx)
library(ggplot2)
source("R/utils.R")
#
# today_date <- format(Sys.Date(), "%Y-%m-%d")
# # 1) summarise new batches ----
#
# barcode_files_kept <- list.files("./data/barcode-counts/",
#                             pattern = "exp.*\\.tabular",
#                             full.names = TRUE)
# kept_barcode_summaries <- purrr::map(barcode_files_kept, function(filename) {
#   barcode_counts <- readr::read_tsv(filename, show_col_types = FALSE)
#   experience_name <- filename |> basename() |> tools::file_path_sans_ext()
#   experience_summary <- tibble::tibble(`Experience Name` = experience_name,
#                                        `Num Barcode IDs` = nrow(barcode_counts),
#                                        `Num Replicates` = ncol(barcode_counts) - 1)
#   return(experience_summary)
# }) |>
#   purrr::list_rbind()
#
# # 2) summarise old batches ----
#
# old_barcode_files <- list.files("./data/barcode-counts/old barcounts",
#                             pattern = "exp.*\\.csv",
#                             full.names = TRUE)
# old_barcode_summaries <- purrr::map(old_barcode_files, function(filename) {
#   barcode_counts <- readr::read_csv(filename, show_col_types = FALSE)
#   experience_name <- filename |> basename() |> tools::file_path_sans_ext()
#   experience_summary <- tibble::tibble(`Experience Name` = experience_name,
#                                        `Num Barcode IDs` = nrow(barcode_counts),
#                                        `Num Replicates` = ncol(barcode_counts) - 1)
#   return(experience_summary)
# }) |>
#   purrr::list_rbind()
#
# global_summaries <- list("new barcode counts" = kept_barcode_summaries,
#                          "old barcode counts" = old_barcode_summaries)
# openxlsx::write.xlsx(global_summaries,
#                      file = paste0("./data/global_summaries_rows_by_cols_", today_date, ".xlsx"),
#                      asTable = TRUE)

# change format + barcode ID primary key ----
barcode_files <- list.files("./data/barcode-counts/",
                            pattern = "exp.*\\.tabular",
                            full.names = TRUE)

barcode_matrices <- lapply(barcode_files, function(old_filename) {
  expression_matrix <- readr::read_tsv(old_filename) |>
    rename(barcode_id = 1)

  new_filename <- old_filename |> basename() |> tools::file_path_sans_ext()

  readr::write_tsv(expression_matrix,
                   file = file.path("data", "barcode-counts", paste0(new_filename, ".tsv")))
  unlink(old_filename, force = TRUE)
  return(new_filename)
})

# Experience by experience checking ----
exp_old <- readr::read_csv("data/barcode-counts/exp281022t25.tsv")
exp_new <- readr::read_tsv("data/barcode-counts/exp281022gamme.tsv")
reduced_dataset <- compare_dataframes(exp_old, exp_new)
distributions_plots <- compare_old_vs_new_distributions(exp_old, exp_new)

## Do new barcode IDs bring something to the table?
common_barcodes <- intersect(exp_old$barcode_id, exp_new$barcode_id)
exp_old_shortened <- exp_old |>
  select(barcode_id, dplyr::matches("Contro")) |>
  filter(barcode_id %in% common_barcodes) |>
  arrange(barcode_id)
exp_new_shortened <- exp_new |>
  select(barcode_id, dplyr::starts_with("CTRL")) |>
  filter(barcode_id %in% common_barcodes) |>
  arrange(barcode_id)

all.equal(exp_old_shortened$`9d_Contro1_000u_exp281022_run110723_01`,
    exp_new_shortened$`CTRL1-T75_000u_exp281022_run110723_22`)
quantile(exp_old_shortened$`9d_Contro2_000u_exp281022_run110723_02`,
         probs = c(0.99, 0.995, 0.999, 1), na.rm=TRUE)
quantile(exp_new_shortened$`CTRL4-T75_000u_exp281022_run110723_25`,
         probs = c(0.99, 0.995, 0.999, 1), na.rm=TRUE)



T25_standard <- readr::read_tsv("data/barcode-counts/exp281022t25.tsv") |>
  select(barcode_id, dplyr::starts_with("CTRL", ignore.case = FALSE))

time_course <- readr::read_tsv("data/barcode-counts/exp281022gamme.tsv") |>
  select(barcode_id, dplyr::starts_with("CTRL", ignore.case = FALSE)) |>
  inner_join(T25_standard, by = "barcode_id")

# Merge time-course experiences ----

# exp_old <- readr::read_csv("data/barcode-counts/old barcounts/exp281022_time course.csv") |>
#   select(barcode_id, dplyr::matches("Contro"))
# exp_new <- readr::read_tsv("data/barcode-counts/exp281022gamme.tsv") |>
#   dplyr::left_join(exp_old, by = "barcode_id") |>
#   mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>
#   dplyr::relocate(dplyr::matches("Contro"), .after = barcode_id)
# readr::write_tsv(exp_new, "data/barcode-counts/exp281022gamme.tsv")

exp_new <- readr::read_tsv("data/barcode-counts/exp281022gamme.tsv") |>
  dplyr::select(-dplyr::starts_with("CTRL", ignore.case = FALSE))
readr::write_tsv(exp_new, "data/barcode-counts/exp281022gamme.tsv")


# Check general metadata concentrations ----

## Compare Luca's comments with my kept compounds

old_compounds <- openxlsx::read.xlsx("./data/barcode_metadata_overview_2025-04-28.xlsx",
                                     sheet = "discarded compounds")
new_annotated_compounds <- openxlsx::read.xlsx("./data/barcode_metadata_overview_2025-05-06_luca_and_alex.xlsx",
                                               sheet = "discarded compounds")

merged_compounds <- old_compounds |>
  dplyr::inner_join(new_annotated_compounds, by = c("Compound", "Batch_ID",
                                                    "Date", "Run.date",
                                                   "Duration", "Duration_ID",
                                                    "OLD_Concentrations", "Concentrations",
                                                   "Replicates")) |>
  dplyr::relocate(MoA.y, .after = MoA.x) |>
  dplyr::relocate(Pathway.y, .after = Pathway.x) |>
  dplyr::filter(Pathway.y != Pathway.x)


openxlsx::write.xlsx(merged_compounds,
                     "./data/temp_curation_compounds.xlsx", asTable = TRUE)


