
# Library -----------------------------------------------------------------

library(magrittr)

file_path <- "/data/GSCALite/TCGA/expr/pancan33_expr.rds.gz"
gs_path <- "/data/GSCALite/01_gene_symbol.rds.gz"

# Load data ---------------------------------------------------------------
gs <- readr::read_rds(path = gs_path)
expr <- readr::read_rds(path = file_path)

# Filter gene symbol ----


expr %>%
  dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        .x %>%
          dplyr::filter(symbol %in% gs) %>%
          dplyr::select(-entrez_id) %>%
          dplyr::distinct(symbol, .keep_all = TRUE)
      }
    )
  ) -> expr_filter

source(file.path(here::here(), "functions", "data_function.R"))
expr_filter %>%
  dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        colnames(.x)[-1] %>%
          barcode_process() %>%
          filter_tumor_normal() %>%
          paired_sample() -> .sample


        .sample_s <- table(.sample$type) %>% as.numeric()
        if (gtools::invalid(.sample_s) || any(.sample_s < 10)) return(NULL)

        .x
      }
    )
  ) %>%
  dplyr::filter(!purrr::map_lgl(expr, is.null)) %>%
  dplyr::pull(cancer_types) -> paired_cancer_types

paired_cancer_types %>% readr::write_rds(path = file.path("/data/GSCALite/TCGA/expr", "paired_cancer_types.rds.gz"), compress = "gz")
expr_filter %>% readr::write_rds(path = file.path("/data/GSCALite/TCGA/expr", "pancan33_expr_filtered.rds.gz"), compress = "gz")
